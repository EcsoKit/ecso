import sys.FileSystem;
import sys.Http;
import sys.io.File;

using StringTools;

/**
	Commit ID used to download CI configuration.
 */
final HAXE_VERSION = "4.2.0";

/**
	URL to the CI logs used to lock the version of OCaml packages.
 */
final LOGS_URL = "https://pipelines.actions.githubusercontent.com/MJ6K1GzHcYdmV4YAWtXY6PLIO7NsPCK0yIt1FZ7mlzuwAhLPxn/_apis/pipelines/1/runs/1668/signedlogcontent/7?urlExpires=2021-02-20T13%3A32%3A01.9315645Z&urlSigningMethod=HMACV1&urlSignature=7cZkT0hUbBiupmzv4POAdKgBiXEtaHAv2rFYzK%2FzqzI%3D";
final HAXE_RUNS = [
	"windows" => 1861695868,
	"ubuntu" => 1861695837,
	"macos" => 1861695889
];

/**
	Workflow ID of the ECSO's CI. 
 */
final WORKFLOW_ID = "1610708";

/**
	Version locks of OCaml packages.
	Will be filled from `LOGS_URL` for the non-existing entries.
 */
final LIB_LOCKS : Map<String,String> = [];
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

	static function main() {
		var script = Http.requestUrl('https://raw.githubusercontent.com/HaxeFoundation/haxe/$HAXE_COMMIT_SHA/.github/workflows/main.yml');
		var output = '../../.github/workflows';
		var locks = new Map<String,Map<String,String>>();
		for(runOS => runID in HAXE_RUNS) {
		}


		gen( script, output, true );
		gen( script, output, false );
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
			File.saveContent('$output/haxe-$HAXE_VERSION.yml', script);
		}
	}

	static function align(value:String, head:String):String {
		final spaces = head.substr(head.lastIndexOf("\n") + 1);
		return head + value.replace('\n', '\n$spaces');
	}
}
