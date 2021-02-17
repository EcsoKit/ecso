import haxe.io.Path;
import haxe.Exception;
import runci.System;
import runci.Config;
import runci.TestTarget;

using StringTools;

function main() {
	final tests = getTests();

	haxelibInstallDev('ecso', repoDir);
	infoMsg('Going to test: $tests');

	function testHaxe(target:TestTarget) {
		if (isCi()) {
			Sys.putEnv("TEST", Std.string(target));
			changeDirectory(Path.join([repoDir, "../../tests"]));
			runCommand("haxe", ["RunCi.hxml"]);
		}
	}

	function runIssues(target:TestTarget, args:Array<String>, ?run:String->Void) {
		if (!testIssues)
			return;
		changeDirectory(unitsDir);
		final issues = getIssues();
		infoMsg('Going to test issues $issues');
		for (issue in issues) {
			runCommand("haxe", ['compile-$target.hxml'].concat(args.concat(['-D', 'issue=$issue'])));
			if (run != null)
				run("units");
		}
	}

	function runSpecs(target:TestTarget, args:Array<String>, ?run:String->Void) {
		if (!testSpecs)
			return;
		changeDirectory(specsDir);
		runCommand("haxe", ['compile-$target.hxml'].concat(args));
		if (run != null)
			run("specs");
	}

	function runUnits(target:TestTarget, args:Array<String>, ?run:String->Void) {
		if (!testUnits)
			return;
		changeDirectory(unitsDir);
		runCommand("haxe", ['compile-$target.hxml'].concat(args));
		if (run != null)
			run("units");
	}

	for (test in tests) {
		switch systemName {
			case "Windows":
				// change codepage to UTF-8
				runCommand("chcp", ["65001"]);
			case _:
				// pass
		}

		infoMsg('test $test');

		final success = try {
			haxelibInstallGit("haxe-utest", "utest", "master");
			haxelibInstall("buddy");

			var args = switch ci {
				case null:
					[];
				case GithubActions:
					["-D", "github"];
			}

			args = args.concat(["-D", systemName]);

			switch test {
				case Macro | Neko | Php | Python | Lua | Cpp | Cppia | Java | Jvm | Cs | Flash9:
					infoMsg("skip tests");
				case Server:
					haxelibInstall("hxnodejs");
					haxelibInstallGit("Simn", "haxeserver");
					changeDirectory(serverDir);
					runCommand("haxe", ["run.hxml"].concat(args));
				case Interp:
					runIssues(Interp, args);
					runUnits(Interp, args);
					runSpecs(Interp, args);
				case Hl:
					testHaxe(Hl);

					function runHl(name:String) {
						final hlBinary = switch [ci, systemName] {
							case [GithubActions, "Windows"]: "C:\\hashlink_build\\bin\\hl.exe";
							case [GithubActions, _]: Path.join([Sys.getEnv("HOME"), "hashlink_build", "bin", "hl"]);
							case _: "hl";
						};
						runCommand(hlBinary, ['bin/$name.hl']);
					}

					runIssues(Hl, args, runHl);
					runUnits(Hl, args, runHl);
					runSpecs(Hl, args, runHl);
				case Js:
					testHaxe(Js);
					for (es_ver in    [[], ["-D", "js-es=3"], ["-D", "js-es=6"]])
					for (unflatten in [[], ["-D", "js-unflatten"]])
					for (classic in   [[], ["-D", "js-classic"]])
					for (enums_as_objects in [[], ["-D", "js-enums-as-arrays"]])
					{
						final args = args.concat(es_ver).concat(unflatten).concat(classic).concat(enums_as_objects);

						function runJs(name:String) {
							final output = if (args.length > 0) {
								'bin/js/${args.join("")}/$name.js';
							} else {
								'bin/js/default/$name.js';
							}
							var outputDir = Path.directory(output);
							if (!sys.FileSystem.exists(outputDir))
								sys.FileSystem.createDirectory(outputDir);
							sys.FileSystem.rename('bin/$name.js', output);
							if (sys.FileSystem.exists('bin/$name.js.map'))
								sys.FileSystem.rename('bin/$name.js.map', output + ".map");
							runCommand("node", ["-e", 'require("./$output")']);
						}
		
						runIssues(Js, args, runJs);
						runUnits(Js, args, runJs);
						runSpecs(Js, args, runJs);
					}
				case t:
					throw new Exception("unknown target: " + t);
			}
			true;
		} catch (_:Failure) {
			false;
		}

		if (success) {
			successMsg('test ${test} succeeded');
		} else {
			failMsg('test ${test} failed');
			break;
		}
	}

	if (!success) {
		Sys.exit(1);
	}
}
