import sys.FileSystem;
import sys.Http;
import sys.io.File;

using StringTools;

/**
	Commit ID used to download CI configuration and run logs.
 */
final HAXE_RUNS = [
	"windows" => 1988061016,
	"ubuntu" => 1988060957,
	"macos" => 1988061091
];
final HAXE_COMMIT_SHA = "a2f4ba95400edf10195ce2a1c87c56dc0d67111b";
final HAXE_VERSION = "4.2.2";

/**
	Workflow ID of the ECSO's CI. 
 */
final WORKFLOW_ID = "1610708";

/**
	Version locks of OCaml packages.
	Will be filled from `LOGS_URL` for the non-existing entries.
 */
final OS_LOCKS : Map<String,String> = [];

class Main {
	static final matchHaxeCheckout = ~/([\r\n]\s*)-\s*uses\s*:\s*(actions\/checkout@[A-Za-z0-9.]+)\s*[\r\n](.|\r|\n)+?(?=(\r|\n)\s*-)/gm;
	static final matchCancelPrevious = ~/([\r\n]\s*)-\s*uses\s*:\s*(styfle\/cancel-workflow-action@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\workflow_id:)\workflow_id:\s([0-9]+)/gm;
	static final matchUploadArtifact = ~/([\r\n]\s*)-\s*name:[\w\s]+\s*:\s*(actions\/upload-artifact@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\sname:)\sname:\s([a-zA-Z${}.]+)/gm;
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
	static final matchRunnerOS = ~/runs-on:\s*(\w+)-(\w+)/g;
	static final mainName = ~/[\r\n]name:\s*(.+)/g;
	static final mainOn = ~/[\r\n]on:\s*(\[[\w\s,-]+\])/g;

	static function main() {
		var script = Http.requestUrl('https://raw.githubusercontent.com/HaxeFoundation/haxe/$HAXE_COMMIT_SHA/.github/workflows/main.yml');
		var output = '../../.github/workflows';
		var locks = new Map<String,Map<String,String>>();
		// for(jobOS => jobID in HAXE_RUNS) {
			// // var logsURL = 'https://api.github.com/repos/HaxeFoundation/haxe/actions/jobs/$jobID/logs';
			// var logsURL = 'https://github.com/HaxeFoundation/haxe/commit/$HAXE_COMMIT_SHA/checks/$jobID/logs';
			// Sys.println('Download logs from $logsURL');
			// var http = new sys.Http(logsURL);
			// http.addParameter("accept", "application/vnd.github.v3+json");
			// http.addParameter("owner", "HaxeFoundation");
			// http.addParameter("repo", "haxe");
			// http.addParameter("job_id", "" + jobID);
			// // http.addParameter("filter", "latest");
			// // http.addParameter("per_page", "100");
			// // http.addParameter("page", "1");
			// http.onData = function(data:String) {
			// 	trace("got logs");
			// }
			// http.onBytes = function(data) {
			// 	trace("got bytes");
			// }
			// http.onError = function(error) {
			// 	trace("got error", error);
			// }
			// http.onStatus = function(status) {
			// 	trace("got status", status, http.responseHeaders.get("Location:"));
			// }
			// http.request(false);
			// var logs = Http.requestUrl(logsURL);
			// var lock = new Map<String,String>();
			// locks.set(jobOS, lock);

			// // Get Ocaml's package versions
			// matchOpamPackages.map(logs, function(reg:EReg) {
			// 	var lib = reg.matched(1);
			// 	var version = reg.matched(2);
			// 	if (!lock.exists(lib))
			// 		lock.set(lib, version);
			// 	else if (lock.get(lib) != version)
			// 		Sys.println('Override $lib version $version with ${lock.get(lib)}');
			// 	return "";
			// });
			
			// // Get OS Version
			// ~/Operating System\s+([A-Za-z_ -]+)\s+([0-9]+ |[0-9]+\.[0-9]+)/g.map(logs, function(reg:EReg) {
			// 	var os = reg.matched(1).toLowerCase();
			// 	var version = reg.matched(2);
			// 	os = if(os.contains(jobOS)) {
			// 		jobOS;
			// 	} else {
			// 		throw 'Unrecognized OS $os';
			// 	}
			// 	OS_LOCKS.set(os, version);
			// 	return "";
			// });
		// }

		// Hack until we figure out how to download logs
		OS_LOCKS.set("windows", "2019");
		OS_LOCKS.set("ubuntu", "18.04");
		OS_LOCKS.set("macos", "10.15");
		final locks = [
			"windows" => [
				"base-bigarray"               => "base",
				"base-bytes"                  => "base",
				"base-threads"                => "base",
				"base-unix"                   => "base",
				"camlp5"                      => "8.00~alpha05",
				"conf-libpcre"                => "1",
				"conf-neko"                   => "1",
				"conf-perl"                   => "1",
				"conf-perl-ipc-system-simple" => "1",
				"conf-perl-string-shellquote" => "1",
				"conf-pkg-config"             => "2",
				"conf-zlib"                   => "1",
				"cppo"                        => "1.6.7",
				"csexp"                       => "1.5.1",
				"ctypes"                      => "0.17.1",
				"dune"                        => "2.8.2b",
				"dune-configurator"           => "2.8.5",
				"extlib"                      => "1.7.8",
				"gen"                         => "0.5.3",
				"integers"                    => "0.4.0",
				"luv"                         => "0.5.7",
				"ocaml"                       => "4.07.0",
				"ocaml-compiler-libs"         => "v0.12.3",
				"ocaml-config"                => "2",
				"ocaml-migrate-parsetree"     => "2.1.0",
				"ocaml-variants"              => "4.07.0+mingw64c",
				"ocamlbuild"                  => "0.14.0",
				"ocamlfind"                   => "1.9.1",
				"ppx_derivers"                => "1.2.1",
				"ppxlib"                      => "0.22.0",
				"ptmap"                       => "2.0.5",
				"result"                      => "1.5",
				"sedlex"                      => "2.3",
				"seq"                         => "base",
				"sexplib0"                    => "v0.14.0",
				"sha"                         => "1.14",
				"stdlib-shims"                => "0.3.0",
				"uchar"                       => "0.0.2",
				"xml-light"                   => "2.4",
			],
			"ubuntu" => [
				"base-bigarray"               => "base",
				"base-bytes"                  => "base",
				"base-threads"                => "base",
				"base-unix"                   => "base",
				"bigarray-compat"             => "1.0.0",
				"camlp5"                      => "8.00~alpha05",
				"conf-libpcre"                => "1",
				"conf-m4"                     => "1",
				"conf-mbedtls"                => "1",
				"conf-neko"                   => "1",
				"conf-perl"                   => "1",
				"conf-perl-ipc-system-simple" => "1",
				"conf-perl-string-shellquote" => "1",
				"conf-pkg-config"             => "2",
				"conf-zlib"                   => "1",
				"cppo"                        => "1.6.7",
				"csexp"                       => "1.5.1",
				"ctypes"                      => "0.18.0",
				"dune"                        => "2.8.5",
				"dune-configurator"           => "2.8.5",
				"extlib"                      => "1.7.8",
				"gen"                         => "0.5.3",
				"integers"                    => "0.4.0",
				"luv"                         => "0.5.7",
				"ocaml"                       => "4.05.0",
				"ocaml-compiler-libs"         => "v0.12.3",
				"ocaml-config"                => "1",
				"ocaml-migrate-parsetree"     => "2.1.0",
				"ocaml-secondary-compiler"    => "4.08.1-1",
				"ocaml-system"                => "4.05.0",
				"ocamlbuild"                  => "0.14.0",
				"ocamlfind"                   => "1.8.1",
				"ocamlfind-secondary"         => "1.8.1",
				"ppx_derivers"                => "1.2.1",
				"ppxlib"                      => "0.22.0",
				"ptmap"                       => "2.0.5",
				"result"                      => "1.5",
				"sedlex"                      => "2.3",
				"seq"                         => "0.2.2",
				"sexplib0"                    => "v0.14.0",
				"sha"                         => "1.14",
				"stdlib-shims"                => "0.3.0",
				"uchar"                       => "0.0.2",
				"xml-light"                   => "2.4",
			],
			"macos" => [
				"base-bigarray"               => "base",
				"base-bytes"                  => "base",
				"base-threads"                => "base",
				"base-unix"                   => "base",
				"camlp5"                      => "8.00~alpha05",
				"conf-libpcre"                => "1",
				"conf-m4"                     => "1",
				"conf-mbedtls"                => "1",
				"conf-neko"                   => "1",
				"conf-perl"                   => "1",
				"conf-perl-ipc-system-simple" => "1",
				"conf-perl-string-shellquote" => "1",
				"conf-pkg-config"             => "2",
				"conf-zlib"                   => "1",
				"cppo"                        => "1.6.7",
				"csexp"                       => "1.5.1",
				"ctypes"                      => "0.17.1",
				"dune"                        => "2.8.5",
				"dune-configurator"           => "2.8.5",
				"extlib"                      => "1.7.8",
				"gen"                         => "0.5.3",
				"integers"                    => "0.4.0",
				"luv"                         => "0.5.7",
				"ocaml"                       => "4.07.1",
				"ocaml-base-compiler"         => "4.07.1",
				"ocaml-compiler-libs"         => "v0.12.3",
				"ocaml-config"                => "1",
				"ocaml-migrate-parsetree"     => "2.1.0",
				"ocaml-secondary-compiler"    => "4.08.1-1",
				"ocamlbuild"                  => "0.14.0",
				"ocamlfind"                   => "1.8.1",
				"ocamlfind-secondary"         => "1.8.1",
				"ppx_derivers"                => "1.2.1",
				"ppxlib"                      => "0.22.0",
				"ptmap"                       => "2.0.5",
				"result"                      => "1.5",
				"sedlex"                      => "2.3",
				"seq"                         => "base",
				"sexplib0"                    => "v0.14.0",
				"sha"                         => "1.14",
				"stdlib-shims"                => "0.3.0",
				"uchar"                       => "0.0.2",
				"xml-light"                   => "2.4",
			]
		];

		gen( script, output, locks, true );
		gen( script, output, locks, false );
	}

	static function gen(script : String, output : String, locks:Map<String,Map<String,String>>, main : Bool) {

		function getOS(from:EReg):String {
			final left = from.matchedLeft();
			final idx = left.lastIndexOf("runs-on:");
			if(idx < 0) throw 'Failed to get the OS of the current match ${from.matched(0)}';
			return if(matchRunnerOS.match(left.substr(idx))) {
				return matchRunnerOS.matched(1).toLowerCase();
			} else {
				throw 'Failed to match "runs-on:" withing $left';
			}
		}

		// Lock Runner OS
		script = matchRunnerOS.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var os = reg.matched(1).toLowerCase();
			var version = reg.matched(2);
			return if(version == "latest") {
				matched.replace('latest', OS_LOCKS.get(os));
			} else {
				matched;
			}
		});

		// Update cancelling previous run
		script = matchCancelPrevious.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var action = reg.matched(2);
			var workflowId = reg.matched(3);
			return if (main) {
				// Replace workflow id for cancelling previous run
				matched.replace(workflowId, WORKFLOW_ID);
			} else {
				// Remove workflow id
				matched.replace(workflowId, "0");
			}
		});

		// Replace Haxe checkout with `checkout-haxe.yml` and `checkout-ecso.yml`
		script = matchHaxeCheckout.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var action = reg.matched(2);

			if (matched.contains('    repository:')) {
				return matched;
			}

			final templateHaxe = File.getContent('./checkout-haxe.yml').replace('::HAXE_VERSION::', HAXE_COMMIT_SHA);
			final templateEcso = File.getContent('./checkout-ecso.yml');

			return align(templateHaxe, head) + align(templateEcso, head);
		});

		// Lock Ocaml setup
		script = matchOpamInstallHaxe.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var install = reg.matched(1);
			var lock = locks[getOS(reg)];
			var libs = [for(lib => version in lock) '"$lib=$version"'].join(" ");
			return matched.replace(install, 'opam install $libs --yes ') + "\n" + matched;
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
			if(makeEcso == "")
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
				+ matched.replace(cmd, 'mkdir ./plugins/ecso/cmxs/hx-$HAXE_VERSION')
				+ matched.replace(cmd, 'mv -T ./plugins/ecso/cmxs/Windows ./plugins/ecso/cmxs/hx-$HAXE_VERSION' + "/Windows${ARCH}") // add architecture + move per haxe version
				+ matched.replace(cmd, 'haxe --cwd plugins/ecso/extra/readme build-haxelib.hxml')
				+ matched.replace(haxeExe, './plugins/ecso/cmxs/hx-$HAXE_VERSION' + "/Windows${ARCH}/plugin.cmxs"); // check result
		});
		// Move binaries (Mac and Linux)
		script = matchCheckOutUnix.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var ls = reg.matched(2);
			var tail = reg.matched(3);
			var platform = switch getOS(reg) {
				case "ubuntu": "Linux";
				case "macos": "Mac";
				case any: throw 'Unexpected platform $any';
			}
			return matched.replace(ls, 'mkdir ./plugins/ecso/cmxs/hx-$HAXE_VERSION')
				+ matched.replace(ls, 'mv ./plugins/ecso/cmxs/$platform ./plugins/ecso/cmxs/hx-$HAXE_VERSION')
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

			var template = File.getContent('./upload-ecso.yml').replace('::ARTIFACT_NAME::', "ecso");

			return align(template, head) + matched.replace(name, '$name\n$tab    retention-days: 1');
		});

		// Download artifact
		script = matchDownloadArtifact.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var action = reg.matched(2);
			var name = reg.matched(3);

			var template = File.getContent('./download-ecso.yml').replace('::ARTIFACT_NAME::', name);

			return align(template, head) + matched;
		});

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
			var readme = matched.replace(name, "Generate readme").replace(cwd, "${{github.workspace}}").replace(run, "haxe --cwd plugins/ecso/extra/readme build-haxelib.hxml");
			// Only generate readme when
			return if(name.contains("SauceLabs")) {
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

		// Save
		FileSystem.createDirectory(output);
		if (main) {
			File.saveContent('$output/main.yml', script);
		} else {
			// Rename the workflow
			script = mainName.map(script, reg -> {
				var matched = reg.matched(0);
				var name = reg.matched(1);
				return matched.replace(name, 'Haxe $HAXE_VERSION');
			});
			script = mainOn.map(script, reg -> {
				var matched = reg.matched(0);
				var on = reg.matched(1);
				return "\n" + File.getContent('./on-release.yml');
			});
			File.saveContent('$output/haxe-$HAXE_VERSION.yml', script);
		}
	}

	static function align(value:String, head:String):String {
		final spaces = head.substr(head.lastIndexOf("\n") + 1);
		return head + value.replace('\n', '\n$spaces');
	}
}
