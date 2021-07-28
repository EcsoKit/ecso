import haxe.Json;
import haxe.io.Path;
import haxe.DynamicAccess;
import sys.FileSystem;
import sys.Http;
import sys.io.File;

using StringTools;

typedef BuildManifest = {
	template:String,
	workflows:Array<JobManifest>
}

typedef JobManifest = {
	name:String,
	haxeVersion:String,
	url:String,
	jobs:Array<String>,
	os:{name:String, version:String},
	?haxeDownload:String,
	?nekoDownload:String,
	?development:Bool,
	haxeSources:String,
	githubWorkflow:String,
	?libraries:DynamicAccess<String>
}

typedef Job = {
	id:String,
	name:String,
	script:String
}

@:nullSafety
class Main {
	static final matchHaxeCheckout = ~/([\r\n]\s*)-\s*uses\s*:\s*(actions\/checkout@[A-Za-z0-9.]+)\s*[\r\n](.|\r|\n)+?(?=(\r|\n)\s*-)/gm;
	static final matchXmldocTasks = ~/([\r\n]\s*)-\s*name:[\w\s]+xmldoc[\w\s]+\s*:\s*[\w\W]+?(?=\n\n|\n\s*-)/gm;
	static final matchUploadArtifact = ~/([\r\n]\s*)-\s*name:[\w\s]+\s*:\s*(actions\/upload-artifact@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\sname:)\sname:\s([a-zA-Z${}.]+)[\w\W]+?(?=\n\n|\n\s*-)/gm;
	static final matchDownloadArtifact = ~/([\r\n]\s*)-\s*uses\s*:\s*(actions\/download-artifact@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\sname:)\sname:\s([a-zA-Z${}.]+)/gm;
	static final matchHaxeTests = ~/([\r\n]\s*)-\s*name: (Test[\w ()-]*)\s*[\n][\w\W]+?(?=haxe)(haxe RunCi\.hxml)[\w\W]+?(?=working-directory:)(working-directory:\s*([\w${}.\/ ]+))[\w\W]+?(?=\n\n|\n\s*-)/gm;
	static final matchHaxeTargets = ~/[\r\n\s]target:\s*\[([\w,\s'"]*)\]/gm;
	static final matchOpamInstallHaxe = ~/.*(opam install haxe[a-zA-Z -]*)(?=[0-9]| |\n).*/g;
	static final matchMakeHaxe = ~/.* ((opam config exec -- make) .* (haxe))(?= |\n).*\n/g;
	static final matchMakeHaxelib = ~/.* ((opam config exec -- make) .* (haxelib))(?= |\n).*\n/g;
	static final matchMakePackage = ~/.* (make .* (package_unix|package_bin)( +\w+)*)(?= |\n).*\n/g;
	static final matchBuildCheck = ~/.* (cygcheck|ldd|otool) .*(haxe|haxelib).*\n/gm;
	static final matchCheckOut = ~/.* (ls (-\w+ )*(\.\/)?out).*\n/g;
	static final matchCompileFs = ~/( |\/)(sys\/compile-fs\.hxml)( *)$/gm;
	static final matchRunnerOS = ~/runs-on:\s*(\w+)-(.+)/g;

	static function loadManifests(folder:String):Array<BuildManifest> {
		final manifests:Map<String, BuildManifest> = [];
		return [
			for (file in FileSystem.readDirectory(folder)) {
				final data = Json.parse(~/^\s*\/\/.*/gm.replace(File.getContent('$folder/$file'), ''));
				final manifest:BuildManifest = {
					template: data.template,
					workflows: data.workflows
				}
				// Load defaults
				@:nullSafety(Off)
				for (field in Reflect.fields(data.defaults)) {
					final defaultValue = Reflect.field(data.defaults, field);
					for (workflow in manifest.workflows)
						if (!Reflect.hasField(workflow, field))
							Reflect.setField(workflow, field, defaultValue);
				}
				// Merge
				final targetManifest = manifests[manifest.template];
				if (targetManifest != null) {
					for (workflow in manifest.workflows)
						targetManifest.workflows.push(workflow);
					continue;
				} else {
					manifests.set(manifest.template, manifest);
					manifest;
				}
			}
		];
	}

	static function main() {
		for (build in loadManifests("./builds")) {
			final jobs = [
				for (workflow in build.workflows) {
					var originalWorkflow = Http.requestUrl(workflow.url);
					final jobTab:String = {
						final r = ~/([ \t]*)jobs\s*:[\r\n]*([ \t]*)/;
						r.match(originalWorkflow);
						r.matched(1) + r.matched(2);
					}
					// Fix job name collisions
					inline function uniqueName(name:String):String {
						return '$name-${workflow.haxeVersion.replace('.', '-')}';
					}
					for (jobName in workflow.jobs) {
						originalWorkflow = new EReg('^\\s+needs:\\s*($jobName)\\s', "gm").map(originalWorkflow, r -> {
							r.matched(0).replace(jobName, '[packaging, ${uniqueName(jobName)}]');
						});
					}
					for (jobName in workflow.jobs) {
						final r = new EReg('\\n$jobTab$jobName\\s*:\\s*(#.*\\n|\\n)((\\s*\\n|$jobTab$jobTab.*\\n)+)', 'm');
						r.match(originalWorkflow);
						final job:Job = {
							id: uniqueName(jobName),
							name: 'Haxe ${workflow.haxeVersion} / $jobName',
							script: transform(r.matched(2), build, workflow)
						};
						job;
					}
				}
			];
			jobs.sort((a, b) -> if (a.id.contains("build") && b.id.contains("test")) {
				-1;
			} else if (a.id.contains("test") && b.id.contains("build")) {
				1;
			} else {
				0;
			});
			final template = File.getContent('./workflow-${build.template}.yml');
			final needs = template.contains('::CANCEL_PREVIOUS::') ? 'cancel' : '';
			jobs.unshift({
				id: 'packaging',
				name: 'Prepare package',
				script: align(File.getContent('./packaging-job.yml').replace('::NEEDS::', needs), '    ') + '\n'
			});
			final output = '../../.github/workflows';
			save(template, jobs, '$output/${build.template}.yml', build);
		}
	}

	static function save(template:String, jobs:Array<Job>, output:String, build:BuildManifest) {
		final JOB_TAB = {
			var r = ~/\n([ \t]*)::JOBS::/;
			r.match(template) ? r.matched(1) : "";
		}
		final yml = template
			.replace('::JOBS::', jobs.map(j -> '${j.id}:\n${JOB_TAB+JOB_TAB}name: ${j.name}\n${j.script}').join(JOB_TAB))
			.replace('::CANCEL_PREVIOUS::', align(File.getContent('./cancel-previous.yml'), JOB_TAB).substr(JOB_TAB.length));
		FileSystem.createDirectory(Path.directory(output));
		File.saveContent(output, yml);
	}

	static function transform(script:String, build:BuildManifest, manifest:JobManifest) {
		// Lock Runner OS
		script = matchRunnerOS.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var os = reg.matched(1).toLowerCase();
			var version = reg.matched(2);
			return matched.replace(os, manifest.os.name).replace(version, manifest.os.version);
		});

		// Replace Haxe checkout with `checkout-haxe.yml` and `checkout-ecso.yml`
		script = matchHaxeCheckout.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var action = reg.matched(2);

			if (matched.contains('    repository:')) {
				return matched;
			}

			final template = File.getContent('./checkout-haxe.yml');
			final templateHaxe = switch manifest.haxeSources.split('@') {
				case [repo, ref]: template.replace('::HAXE_REPO::', repo).replace('::HAXE_REF::', ref);
				case [repo]: template.replace('::HAXE_REPO::', repo).replace('::HAXE_REF::', '');
				case _: throw 'Invalid Haxe Sources ${manifest.haxeSources}';
			}

			final templateEcso = File.getContent('./checkout-ecso.yml');

			return align(templateHaxe, head) + align(templateEcso, head);
		});

		// Lock Ocaml setup
		script = matchOpamInstallHaxe.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var install = reg.matched(1);
			return if (manifest.libraries == null) {
				matched;
			} else {
				@:nullSafety(Off) 
				var libs = [for (lib => version in manifest.libraries) '"$lib=$version"'].join(" ");
				final assumeDepExts = manifest.os.name == "ubuntu" ? '--assume-depexts ' : '';
				matched.replace(install, 'opam install $libs --yes $assumeDepExts') + "\n" + matched;
			}
		});

		// Lock Neko version
		if (manifest.nekoDownload != null) {
			script = ~/(https?:\/\/\S+\/(neko_latest\.(zip|tar\.gz))\/?)\s/gm.map(script, function(reg:EReg) {
				final matched = reg.matched(0);
				final url = reg.matched(1);
				final file = reg.matched(2);
				final ext = reg.matched(3);
				final full:Bool = manifest.nekoDownload.startsWith("http");
				return matched.replace(url, full ? manifest.nekoDownload : url.replace(file, manifest.nekoDownload));
			});
		}

		// Build
		if (manifest.haxeDownload == null) {
			// Rename build jobs
			script = script.replace("Build Haxe", "Build Haxe + Ecso");
			// Build ecso as plugin (after haxelib)
			script = matchMakeHaxelib.map(script, function(reg:EReg) {
				var pos = reg.matchedPos();
				var makeEcso = "";
				matchMakeHaxe.map(script.substring(0, pos.pos), function(reg:EReg) {
					var matched = reg.matched(0);
					var cmd = reg.matched(1);
					var make = reg.matched(2);
					var haxe = reg.matched(3);
					makeEcso = matched.replace(haxe, "PLUGIN=ecso plugin");
					return "";
				});
				if (makeEcso == "")
					throw "Fail to find haxe make";
				var matched = reg.matched(0);
				return matched + makeEcso;
			});
		} else {
			// Rename build jobs
			script = script.replace("Build Haxe", "Build Ecso");
			// Build ecso instead of haxe/haxelib
			script = matchMakeHaxe.map(script, function(reg:EReg) {
				var matched = reg.matched(0);
				var cmd = reg.matched(1);
				var haxe = reg.matched(3);
				return matched.replace(haxe, "PLUGIN=ecso plugin");
			});
			script = matchMakeHaxelib.map(script, function(reg:EReg) {
				var matched = reg.matched(0);
				var cmd = reg.matched(1);
				return "";
			});
			script = matchMakePackage.map(script, function(reg:EReg) {
				final matched = reg.matched(0);
				final cmd = reg.matched(1);
				return matched.replace(cmd, "mkdir ./out");
			});
			script = matchBuildCheck.map(script, function(reg:EReg) {
				return "";
			});
		}

		// Move binaries
		script = matchCheckOut.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var cmd = reg.matched(1);
			final platform = switch manifest.os.name {
				case "ubuntu": "Linux";
				case "macos": "Mac";
				case "windows": "Windows";
				case _: throw false;
			}
			final location = './plugins/ecso/cmxs/$platform';
			final destination = './plugins/ecso/cmxs/hx-${manifest.haxeVersion}';
			final moveEcso = if (manifest.os.name == "windows") {
				matched.replace(cmd, 'mv -T $location $destination/' + "Windows${ARCH}");
			} else {
				matched.replace(cmd, 'mv $location $destination');
			}
			final checkEcso = matched.replace(cmd, switch manifest.os.name {
				case "windows": 'cygcheck $destination/' + "Windows${ARCH}/plugin.cmxs";
				case "ubuntu": 'ldd -v $destination/$platform/plugin.cmxs';
				case "macos": 'otool -L $destination/$platform/plugin.cmxs';
				case _: throw false;
			});
			return matched.replace(cmd, 'mkdir $destination') + moveEcso + checkEcso + matched;
		});

		// Upload artifact
		script = matchUploadArtifact.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			if (matched.contains("xmldoc"))
				return matched;
			var head = reg.matched(1);
			var action = reg.matched(2);
			var name = reg.matched(3);
			var tab = head.substr(head.lastIndexOf('\n') + 1);

			var uploadEcso = File.getContent('./upload-cmxs.yml');
			var uploadHaxe = if (manifest.haxeDownload == null) {
				matched.replace(name, '$name\n$tab    retention-days: 1');
			} else {
				'';
			};

			return align(uploadEcso, head) + uploadHaxe;
		});

		// Download artifact
		script = matchDownloadArtifact.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var action = reg.matched(2);
			var name = reg.matched(3);

			var template = File.getContent('./download-ecso.yml').replace('::ARTIFACT_NAME::', name);
			var downloadHaxe = if (manifest.haxeDownload == null) {
				matched.substr(head.length).replace(head, '\n');
			} else {
				final ext = switch manifest.os.name {
					case "windows": 'zip';
					case "ubuntu" | "macos": 'tar.gz';
					case _: throw false;
				}
				final file = Path.removeTrailingSlashes(manifest.haxeDownload).split('/').pop();
				if (file == null || !file.contains('.$ext'))
					throw 'Extention of the file to download ${manifest.haxeDownload} doesn\'t match the expected extension $ext';
				File.getContent('./download-file.yml')
					.replace('::URL::', manifest.haxeDownload)
					.replace('::OUTPUT_NAME::', 'haxe_bin.$ext')
					.replace('::TARGET_FOLDER::', './$name');
			}

			return align(template, head) + align(downloadHaxe, head);
		});

		// Force release/dev modes
		script = script.replace("startsWith(github.ref, 'refs/tags/')", manifest.development != null ? '${!manifest.development}' : 'true');

		// Edit tests
		script = matchHaxeTests.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var name = reg.matched(2);
			var run = reg.matched(3);
			var cwd = reg.matched(5);
			function correctRelativePaths(source:String, workingDirectory:String) {
				// Correct path to `compile-fs.hxml`
				return matchCompileFs.map(source, function(reg:EReg) {
					var matched = reg.matched(0);
					var head = reg.matched(1);
					var path = reg.matched(2);
					var tail = reg.matched(3);
					return matched.replace(path, '$workingDirectory/$path');
				});
			}
			// Redirect tests
			var test = matched.replace(cwd, "${{github.workspace}}/plugins/ecso/tests");
			return correctRelativePaths(test, cwd);
		});

		// Add test targets
		script = matchHaxeTargets.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var targets = reg.matched(1);
			var list = ~/\W+/g.split(targets);
			return if (list.length > 1) {
				// start with "interp"
				if (!targets.contains('interp'))
					list.unshift('interp');
				// filter some targets
				list = list.filter(target -> switch target {
					case "neko" | "php" | "python" | "lua" | "jvm" | "java" | "macro": false;
					case _: true;
				});
				// ensure some targets
				for (target in ["hl", "js", "cpp", "cs", "server"])
					if (!list.contains('server'))
						list.push('server');
				matched.replace(targets, list.join(', '));
			} else {
				matched;
			}
		});

		// Remove xmldoc generation
		script = matchXmldocTasks.map(script, function(reg:EReg) {
			return "";
		});

		return script;
	}

	static function align(value:String, head:String):String {
		final spaces = head.substr(head.lastIndexOf("\n") + 1);
		return head + value.replace('\n', '\n$spaces');
	}
}
