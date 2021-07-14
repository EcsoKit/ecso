import haxe.display.Protocol.FileParams;
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
	os:{name:String, version:String, ?arch:Int},
	?haxeDownload:String,
	haxeSources:String,
	githubWorkflow:String,
	?libraries:DynamicAccess<String>
}

typedef Job = {
	name:String,
	job:String
}

class Main {
	static final matchHaxeCheckout = ~/([\r\n]\s*)-\s*uses\s*:\s*(actions\/checkout@[A-Za-z0-9.]+)\s*[\r\n](.|\r|\n)+?(?=(\r|\n)\s*-)/gm;
	static final matchCancelPrevious = ~/([\r\n]\s*)-\s*uses\s*:\s*(styfle\/cancel-workflow-action@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\workflow_id:)\workflow_id:\s([0-9]+)/gm;
	static final matchUploadArtifact = ~/([\r\n]\s*)-\s*name:[\w\s]+\s*:\s*(actions\/upload-artifact@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\sname:)\sname:\s([a-zA-Z${}.]+)[\w\W]+?(?=\n\n|\n\s*-)/gm;
	static final matchDownloadArtifact = ~/([\r\n]\s*)-\s*uses\s*:\s*(actions\/download-artifact@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\sname:)\sname:\s([a-zA-Z${}.]+)/gm;
	static final matchHaxeTests = ~/([\r\n]\s*)-\s*name: (Test[\w ()-]*)\s*[\n][\w\W]+?(?=haxe)(haxe RunCi\.hxml)[\w\W]+?(?=working-directory:)(working-directory:\s*([\w${}.\/ ]+))[\w\W]+?(?=\n\n|\n\s*-)/gm;
	static final matchHaxeTargets = ~/[\r\n\s]target:\s*\[([\w,\s'"]*)\]/gm;
	static final matchOpamInstallHaxe = ~/.*(opam install haxe[a-zA-Z -]*)(?=[0-9]| |\n).*/g;
	static final matchOpamPackages = ~/\s*-\s*install\s+(\S+)\s+(\S+)\s*\n/g;
	static final matchMakeHaxe = ~/.* ((opam config exec -- make) .* (haxe))(?= |\n).*/g;
	static final matchMakeHaxelib = ~/.* (make) .* (haxelib)(?= |\n).*/g;
	static final matchCygcheckExe = ~/([\r\n]\s*).* (cygcheck (\.\/haxe\.exe))(?=').*/g;
	static final matchCheckOutUnix = ~/([\r\n] *)(ls -l out)( *)/g;
	static final matchCompileFs = ~/( |\/)(sys\/compile-fs\.hxml)( *)$/gm;
	static final matchWin32Test = ~/\s+windows-test\s*:(\s*)/gm;
	static final matchRunnerOS = ~/runs-on:\s*(\w+)-(.+)/g;
	static final mainName = ~/[\r\n]name:\s*(.+)/g;
	static final mainOn = ~/[\r\n]on:\s*(\[[\w\s,-]+\])/g;

	static function loadManifests(folder:String):Array<BuildManifest> {
		final manifests:Map<String,BuildManifest> = [];
		return [
			for (file in FileSystem.readDirectory(folder)) {
				final data = Json.parse(~/^\s*\/\/.*/gm.replace(File.getContent('$folder/$file'), ''));
				final manifest:BuildManifest = {
					template: data.template,
					workflows: data.workflows
				}
				// Load defaults
				for (field in Reflect.fields(data.defaults)) {
					final defaultValue = Reflect.field(data.defaults, field);
					for (workflow in manifest.workflows)
						if (!Reflect.hasField(workflow, field))
							Reflect.setField(workflow, field, defaultValue);
				}
				// Merge
				if(manifests.exists(manifest.template)) {
					final targetManifest = manifests[manifest.template];
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
						return '$name-${workflow.haxeVersion.replace('.','-')}';
					}
					for (jobName in workflow.jobs) {
						originalWorkflow = new EReg('^\\s+needs:\\s*($jobName)\\s',"gm").map(originalWorkflow, r -> {
							r.matched(0).replace(jobName, uniqueName(jobName));
						});
					}
					for (jobName in workflow.jobs) {
						final r = new EReg('\\n$jobTab$jobName\\s*:\\s*(#.*\\n|\\n)((\\s*\\n|$jobTab$jobTab.*\\n)+)', 'm');
						r.match(originalWorkflow);
						final job:Job = {
							name: uniqueName(jobName),
							job: transform(r.matched(2), build, workflow)
						};
						job;
					}
				}
			];
			jobs.sort((a, b) -> if (a.name.contains("build") && b.name.contains("test")) {
				-1;
			} else if (a.name.contains("test") && b.name.contains("build")) {
				1;
			} else {
				0;
			});
			final output = '../../.github/workflows';
			save(File.getContent('./workflow-${build.template}.yml'), jobs, '$output/${build.template}.yml', build);
		}
	}

	static function save(template:String, jobs:Array<Job>, output:String, build:BuildManifest) {
		final JOB_TAB = {
			var r = ~/\n([ \t]*)::JOBS::/;
			r.match(template) ? r.matched(1) : "";
		}
		final yml = template.replace('::JOBS::', jobs.map(j -> '${j.name}:\n${j.job}').join(JOB_TAB));
		FileSystem.createDirectory(Path.directory(output));
		File.saveContent(output, yml);
	}

	static function transform(script:String, build:BuildManifest, manifest:JobManifest) {
		// Lock Runner OS
		script = matchRunnerOS.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var os = reg.matched(1).toLowerCase();
			var version = reg.matched(2);
			// return if (os == manifest.os.name) {
			// 	matched.replace(version, manifest.os.version);
			// } else {
			// 	throw 'OS ${}';
			// }
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
				var libs = [for (lib => version in manifest.libraries) '"$lib=$version"'].join(" ");
				matched.replace(install, 'opam install $libs --yes ') + "\n" + matched;
			}
		});

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
			return matched + "\n" + makeEcso;
		});

		// Move binaries (Windows)
		script = matchCygcheckExe.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var cmd = reg.matched(2);
			var haxeExe = reg.matched(3);
			return matched
				+ matched.replace(cmd, 'mkdir ./plugins/ecso/cmxs/hx-${manifest.haxeVersion}')
				+ matched.replace(cmd,
					'mv -T ./plugins/ecso/cmxs/Windows ./plugins/ecso/cmxs/hx-${manifest.haxeVersion}' +
					"/Windows${ARCH}") // add architecture + move per haxe version
				+ matched.replace(cmd, 'haxe --cwd plugins/ecso/extra/readme build-haxelib.hxml')
				+ matched.replace(haxeExe, './plugins/ecso/cmxs/hx-${manifest.haxeVersion}' + "/Windows${ARCH}/plugin.cmxs"); // check result
		});
		// Move binaries (Mac and Linux)
		script = matchCheckOutUnix.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var ls = reg.matched(2);
			var tail = reg.matched(3);
			var platform = switch manifest.os.name {
				case "ubuntu": "Linux";
				case "macos": "Mac";
				case any: throw 'Unexpected platform $any';
			}
			return matched.replace(ls, 'mkdir ./plugins/ecso/cmxs/hx-${manifest.haxeVersion}')
				+ matched.replace(ls, 'mv ./plugins/ecso/cmxs/$platform ./plugins/ecso/cmxs/hx-${manifest.haxeVersion}')
				+ matched.replace(ls, './haxe --cwd plugins/ecso/extra/readme build-haxelib.hxml')
				+ matched;
		});

		// Upload artifact
		script = matchUploadArtifact.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			if (matched.contains("xmldoc"))
				return matched;
			var head = reg.matched(1);
			var action = reg.matched(2);
			var name = reg.matched(3);
			var tab = head.substring(head.indexOf(' '), head.lastIndexOf(' ') + 1);

			var uploadEcso = File.getContent('./upload-ecso.yml').replace('::ARTIFACT_NAME::', "ecso");
			var uploadHaxe = if(manifest.haxeDownload == null) {
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
				File.getContent('./download-file.yml')
					.replace('::URL::', manifest.haxeDownload)
					.replace('::OUTPUT_NAME::', 'haxe_bin.zip')
					.replace('::TARGET_FOLDER::', name); // manifest.os.arch == null ? name : name.replace("${{env.ARCH}}", ""+manifest.os.arch)
			}

			return align(template, head) + align(downloadHaxe, head);
		});

		// Match build kind
		script = script.replace("startsWith(github.ref, 'refs/tags/')", manifest.haxeDownload != null ? 'true' : 'false');

		// Rename build jobs
		script = script.replace("Build Haxe", "Build Haxe + Ecso");

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
					// return "${{github.workspace}}" + '/tests/$path$tail';
				});
			}
			// Redirect tests
			var test = matched.replace(cwd, "${{github.workspace}}/plugins/ecso/tests");
			// Generate readme
			var readme = matched.replace(name, "Generate readme")
				.replace(cwd, "${{github.workspace}}")
				.replace(run, "haxe --cwd plugins/ecso/extra/readme build-haxelib.hxml");
			// Only generate readme when
			return if (name.contains("SauceLabs")) {
				correctRelativePaths(test, cwd);
			} else {
				correctRelativePaths(test + readme, cwd);
			}
		});

		// Add test targets
		script = matchHaxeTargets.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var targets = reg.matched(1);
			var list = targets.split(',');
			return if (list.length > 1) {
				// start with "interp"
				if (!targets.contains('interp')) {
					list[0] = " " + list[0];
					list.unshift('interp');
				}
				// end with "server"
				if (!targets.contains('server'))
					list.push(' server');
				matched.replace(targets, list.join(','));
			} else {
				matched;
			}
		});

		// Disable Windows 32 tests
		script = matchWin32Test.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var br = reg.matched(1);
			return matched + "if: ${{ false }}" + br;
		});

		return script;
	}

	static function align(value:String, head:String):String {
		final spaces = head.substr(head.lastIndexOf("\n") + 1);
		return head + value.replace('\n', '\n$spaces');
	}
}
