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

	if (isCi() && tests.length > 1)
		tests.unshift(Interp);

	haxelibInstallDev('ecso', repoDir);
	infoMsg('Going to test: $tests');

	function testHaxe(target:TestTarget) {
		if (isCi()) {
			Sys.putEnv("TEST", Std.string(target));
			changeDirectory(Path.join([repoDir, "../../tests"]));
			runCommand("haxe", ["RunCi.hxml"]);
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

			switch (test) {
				case Macro | Neko | Php | Python | Lua | Cpp | Cppia | Js | Java | Jvm | Cs | Flash9:
					infoMsg("skip tests");
				case Interp:
					changeDirectory(specsDir);
					runCommand("haxe", ["compile-interp.hxml"].concat(args));
					changeDirectory(unitsDir);
					runCommand("haxe", ["compile-interp.hxml"].concat(args));
				case Hl:
					testHaxe(Hl);
					final hlBinary = switch [ci, systemName] {
						case [GithubActions, "Windows"]: "C:\\hashlink_build\\bin\\hl.exe";
						case [GithubActions, _]: Path.join([Sys.getEnv("HOME"), "hashlink_build", "bin", "hl"]);
						case _: "hl";
					};
					changeDirectory(specsDir);
					runCommand("haxe", ["compile-hl.hxml"].concat(args));
					runCommand(hlBinary, ["bin/specs.hl"]);
					changeDirectory(unitsDir);
					runCommand("haxe", ["compile-hl.hxml"].concat(args));
					runCommand(hlBinary, ["bin/units.hl"]);
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
