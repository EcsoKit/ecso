import sys.FileSystem;
import sys.Http;
import sys.io.File;

using StringTools;

/**
	Commit ID used to download CI configuration and run logs.
 */
final HAXE_COMMIT_SHA = "bf9ff69c0801082174f0b2b0a66faeb5356de580";
final HAXE_VERSION = "4.2.1";
final HAXE_RUNS = [
	"windows" => 1988061016,
	"ubuntu" => 1988060957,
	"macos" => 1988061091
];

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
	static final matchHaxeTests = ~/[\r\n\s]*(haxe RunCi\.hxml)([\w\W\r\n]+?(?=\sworking-directory:)\s)working-directory:\s([\w${}.\/ ]+)/gm;
	static final matchHaxeTargets = ~/[\r\n\s]target:\s*\[([\w,\s'"]*)\]/gm;
	static final matchOpamUpdateHaxe = ~/.*(opam update[a-zA-Z -]*)(?=[0-9]| |\n).*/g;
	static final matchOpamPackages = ~/\s*-\s*install\s+(\S+)\s+(\S+)\s*\n/g;
	static final matchMakeHaxe = ~/.* (make) .* (haxe)(?= |\n).*/g;
	static final matchMakeHaxelib = ~/.* (make) .* (haxelib)(?= |\n).*/g;
	static final matchCygcheckExe = ~/([\r\n]\s*).* (cygcheck) (\.\/haxe\.exe)(?=').*/g;
	static final matchCompileFs = ~/( |\/)(sys\/compile-fs\.hxml)( *)$/gm;
	static final matchWin32Test = ~/\s+windows-test\s*:(\s*)/gm;
	static final matchRunnerOS = ~/runs-on:\s*(\w+)-(\w+)/g;
	static final mainName = ~/[\r\n]name:\s*(.+)/g;

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
				"sexplib0"                    => "v0.14.0",
				"ppx_derivers"                => "1.2.1",
				"conf-libpcre"                => "1",
				"seq"                         => "base",
				"conf-mbedtls"                => "1",
				"cppo"                        => "1.6.7",
				"ocaml-compiler-libs"         => "v0.12.3",
				"result"                      => "1.5",
				"sha"                         => "1.13",
				"base-bytes"                  => "base",
				"conf-perl"                   => "1",
				"conf-zlib"                   => "1",
				"ocaml-migrate-parsetree"     => "2.1.0",
				"conf-neko"                   => "1",
				"xml-light"                   => "2.4",
				"ocamlbuild"                  => "0.14.0",
				"stdlib-shims"                => "0.3.0",
				"luv"                         => "0.5.7",
				"csexp"                       => "1.4.0",
				"extlib"                      => "1.7.8",
				"conf-perl-string-shellquote" => "1",
				"conf-perl-ipc-system-simple" => "1",
				"uchar"                       => "0.0.2",
				"ptmap"                       => "2.0.5",
				"ppxlib"                      => "0.22.0",
				"dune-configurator"           => "2.8.2",
				"camlp5"                      => "8.00~alpha05",
				"gen"                         => "0.5.3",
				"sedlex"                      => "2.3"
			],
			"ubuntu" => [
				"conf-mbedtls"                => "1",
				"conf-m4"                     => "1",
				"ocaml-secondary-compiler"    => "4.08.1-1",
				"conf-perl"                   => "1",
				"ocamlbuild"                  => "0.14.0",
				"conf-pkg-config"             => "2",
				"conf-neko"                   => "1",
				"ocamlfind"                   => "1.8.1",
				"conf-perl-string-shellquote" => "1",
				"conf-perl-ipc-system-simple" => "1",
				"uchar"                       => "0.0.2",
				"conf-zlib"                   => "1",
				"conf-libpcre"                => "1",
				"xml-light"                   => "2.4",
				"ocamlfind-secondary"         => "1.8.1",
				"base-bytes"                  => "base",
				"camlp5"                      => "8.00~alpha05",
				"dune"                        => "2.8.2",
				"stdlib-shims"                => "0.3.0",
				"sha"                         => "1.13",
				"sexplib0"                    => "v0.14.0",
				"seq"                         => "0.2.2",
				"result"                      => "1.5",
				"ppx_derivers"                => "1.2.1",
				"ocaml-migrate-parsetree"     => "2.1.0",
				"ocaml-compiler-libs"         => "v0.12.3",
				"integers"                    => "0.4.0",
				"cppo"                        => "1.6.7",
				"bigarray-compat"             => "1.0.0",
				"ptmap"                       => "2.0.5",
				"csexp"                       => "1.4.0",
				"ppxlib"                      => "0.22.0",
				"extlib"                      => "1.7.8",
				"ctypes"                      => "0.18.0",
				"dune-configurator"           => "2.8.2",
				"luv"                         => "0.5.7",
				"gen"                         => "0.5.3",
				"sedlex"                      => "2.3"
			],
			"macos" => [
				"sexplib0"                    => "v0.14.0",
				"ppx_derivers"                => "1.2.1",
				"conf-libpcre"                => "1",
				"seq"                         => "base",
				"conf-mbedtls"                => "1",
				"cppo"                        => "1.6.7",
				"ocaml-compiler-libs"         => "v0.12.3",
				"result"                      => "1.5",
				"sha"                         => "1.13",
				"base-bytes"                  => "base",
				"conf-perl"                   => "1",
				"conf-zlib"                   => "1",
				"ocaml-migrate-parsetree"     => "2.1.0",
				"conf-neko"                   => "1",
				"xml-light"                   => "2.4",
				"ocamlbuild"                  => "0.14.0",
				"stdlib-shims"                => "0.3.0",
				"luv"                         => "0.5.7",
				"csexp"                       => "1.4.0",
				"extlib"                      => "1.7.8",
				"conf-perl-string-shellquote" => "1",
				"conf-perl-ipc-system-simple" => "1",
				"uchar"                       => "0.0.2",
				"ptmap"                       => "2.0.5",
				"ppxlib"                      => "0.22.0",
				"dune-configurator"           => "2.8.2",
				"camlp5"                      => "8.00~alpha05",
				"gen"                         => "0.5.3",
				"sedlex"                      => "2.3"
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

		// Correct path to `compile-fs.hxml`
		script = matchCompileFs.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var path = reg.matched(2);
			var tail = reg.matched(3);
			return '$head../../../tests/$path$tail';
		});

		// Replace Haxe checkout with `checkout-haxe.yml` and `checkout-ecso.yml`
		script = matchHaxeCheckout.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var action = reg.matched(2);

			if (matched.contains('    repository:')) {
				return matched;
			}

			final templateHaxe = File.getContent('./checkout-haxe.yml').replace('::HAXE_VERSION::', HAXE_VERSION);
			final templateEcso = File.getContent('./checkout-ecso.yml');

			return align(templateHaxe, head) + align(templateEcso, head);
		});

		// Lock Ocaml setup
		script = matchOpamUpdateHaxe.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var update = reg.matched(1);
			var lock = locks[getOS(reg)];
			var libs = [for(lib => version in lock) '"$lib=$version"'].join(" ");
			return matched + "\n" + matched.replace(update, 'opam install $libs --yes ');
		});

		// Build ecso as plugin (after haxelib)
		script = matchMakeHaxelib.map(script, function(reg:EReg) {
			var pos = reg.matchedPos();
			var makeEcso = "";
			matchMakeHaxe.map(script.substring(0, pos.pos), function(reg:EReg) {
				var matched = reg.matched(0);
				var make = reg.matched(1);
				var haxe = reg.matched(2);
				makeEcso = matched.replace(haxe, "PLUGIN=ecso plugin");
				return "";
			});
			if(makeEcso == "") throw "Fail to find haxe make";
			var matched = reg.matched(0);
			return matched + "\n" + makeEcso;
		});

		// Cygcheck ecso cmxs
		script = matchCygcheckExe.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var cygcheck = reg.matched(2);
			var haxeExe = reg.matched(3);
			return matched
				+ matched.replace(cygcheck, "mv -T").replace(haxeExe, "./plugins/ecso/cmxs/Windows ./plugins/ecso/cmxs/Windows${ARCH}") // add architecture
				+ matched.replace(haxeExe, "./plugins/ecso/cmxs/Windows${ARCH}/plugin.cmxs"); // check result
		});

		// Upload artifact
		script = matchUploadArtifact.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var action = reg.matched(2);
			var name = reg.matched(3);

			var template = File.getContent('./upload-ecso.yml').replace('::ARTIFACT_NAME::', name);

			return align(template, head) + matched;
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

		// Redirect tests
		script = matchHaxeTests.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var cmd = reg.matched(1);
			var head = reg.matched(2);
			var cwd = reg.matched(3);
			return matched.replace(cwd, "${{github.workspace}}/plugins/ecso/tests");
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
			File.saveContent('$output/haxe-$HAXE_VERSION.yml', script);
		}
	}

	static function align(value:String, head:String):String {
		final spaces = head.substr(head.lastIndexOf("\n") + 1);
		return head + value.replace('\n', '\n$spaces');
	}
}
