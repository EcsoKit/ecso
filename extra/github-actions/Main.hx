import sys.FileSystem;
import sys.Http;
import sys.io.File;

using StringTools;

final HAXE_VERSION = "83ad82b8718986bd211552e6202059ef398290ef";
final WORKFLOW_ID = "1610708";

class Main {
	static final matchHaxeCheckout = ~/([\r\n]\s*)-\s*uses\s*:\s*(actions\/checkout@[A-Za-z0-9.]+)\s*[\r\n](.|\r|\n)+?(?=(\r|\n)\s*-)/gm;
	static final matchCancelPrevious = ~/([\r\n]\s*)-\s*uses\s*:\s*(styfle\/cancel-workflow-action@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\workflow_id:)\workflow_id:\s([0-9]+)/gm;
	static final matchUploadArtifact = ~/([\r\n]\s*)-\s*name:[\w\s]+\s*:\s*(actions\/upload-artifact@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\sname:)\sname:\s([a-zA-Z${}.]+)/gm;
	static final matchDownloadArtifact = ~/([\r\n]\s*)-\s*uses\s*:\s*(actions\/download-artifact@[A-Za-z0-9.]+)\s*[\w\W\r\n]+?(?=\sname:)\sname:\s([a-zA-Z${}.]+)/gm;
	static final matchHaxeTests = ~/[\r\n\s]*(haxe RunCi\.hxml)([\w\W\r\n]+?(?=\sworking-directory:)\s)working-directory:\s([\w${}.\/ ]+)/gm;
	static final matchHaxeTargets = ~/[\r\n\s]target:\s*\[([\w,\s'"]*)\]/gm;
	static final matchMakeHaxe = ~/.* (make) .* (haxe)(?= |\n).*/g;
	static final matchCygcheckExe = ~/([\r\n]\s*).* (cygcheck) (\.\/haxe\.exe)(?=').*/g;
	static final matchCompileFs = ~/( |\/)(sys\/compile-fs\.hxml)( *)$/gm;
	static final matchWin32Test = ~/\s+windows-test\s*:(\s*)/gm;

	static function main() {
		var script = Http.requestUrl('https://raw.githubusercontent.com/HaxeFoundation/haxe/$HAXE_VERSION/.github/workflows/main.yml');
		var output = '../../.github/workflows';

		// Replace workflow id for cancelling previous run
		script = matchCancelPrevious.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var head = reg.matched(1);
			var action = reg.matched(2);
			var workflowId = reg.matched(3);
			return matched.replace(workflowId, WORKFLOW_ID);
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

		// Build ecso as plugin
		script = matchMakeHaxe.map(script, function(reg:EReg) {
			var matched = reg.matched(0);
			var make = reg.matched(1);
			var haxe = reg.matched(2);
			return matched + "\n" + matched.replace(haxe, "PLUGIN=ecso plugin");
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

			var downloadEcso = File.getContent('./download-ecso.yml').replace('::ARTIFACT_NAME::', name);
			var deleteHaxe = File.getContent('./delete-artifact.yml').replace('::ARTIFACT_NAME::', name);

			return align(downloadEcso, head) + matched + align(deleteHaxe, head);
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
		File.saveContent('$output/main.yml', script);
	}

	static function align(value:String, head:String):String {
		final spaces = head.substr(head.lastIndexOf("\n") + 1);
		return head + value.replace('\n', '\n$spaces');
	}
}
