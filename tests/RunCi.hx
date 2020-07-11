import sys.FileSystem;
import haxe.io.Path;
import haxe.Exception;
import runci.System;
import runci.Config;
import runci.TestTarget;

using StringTools;

function main() {
	final args = Sys.args();
	final tests:Array<TestTarget> = switch (args.length == 1 ? args[0] : Sys.getEnv("TEST")) {
		case null:
			[Macro];
		case env:
			[for (v in env.split(",")) v.trim().toLowerCase()];
	}

	haxelibInstallDev('ecso', repoDir);
	infoMsg('Going to test: $tests');

	function testHaxe(target:TestTarget) {
		if (isCi()) {
			Sys.putEnv("TEST", Std.string(target));
			changeDirectory(Path.join([repoDir, "../../tests"]));
			runCommand("haxe", ["RunCi.hxml"]);
		}
	}

	function testIssues(target:TestTarget, args:Array<String>, ?run:String->Void) {
		changeDirectory(unitsDir);
		for (file in FileSystem.readDirectory("units/issues")) {
			if (!file.endsWith(".hx") || !file.startsWith("Issue")) {
				continue;
			}
			var issue = file.substring(5, file.length - 3);
			runCommand("haxe", ['compile-$target.hxml'].concat(args.concat(['-D', 'issue=$issue'])));
			if (run != null) {
				run("units");
			}
		}
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
				case Macro | Neko | Php | Python | Lua | Cpp | Cppia | Js | Java | Jvm | Cs | Flash9:
					infoMsg("skip tests");
				case Server:
					haxelibInstall("hxnodejs");
					haxelibInstallGit("Simn", "haxeserver");
					changeDirectory(serverDir);
					runCommand("haxe", ["run.hxml"].concat(args));
				case Interp:
					testIssues(Interp, args);

					changeDirectory(unitsDir);
					runCommand("haxe", ["compile-interp.hxml"].concat(args));

					changeDirectory(specsDir);
					runCommand("haxe", ["compile-interp.hxml"].concat(args));
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

					testIssues(Hl, args, runHl);

					changeDirectory(unitsDir);
					runCommand("haxe", ["compile-hl.hxml"].concat(args));
					runHl("units");

					changeDirectory(specsDir);
					runCommand("haxe", ["compile-hl.hxml"].concat(args));
					runHl("specs");
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
