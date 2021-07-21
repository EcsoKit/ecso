import haxe.io.Path;
import haxe.Exception;
import runci.System;
import runci.Config;
import runci.TestTarget;
import sys.FileSystem;

using StringTools;

function main() {
	final tests = getTests();

	haxelibInstallDev('ecso', repoDir);
	infoMsg('Going to test: $tests');

	function testHaxe(target:TestTarget) {
		if (isCi()) {
			Sys.putEnv("TEST", Std.string(target));
			changeDirectory(Path.join([repoDir, "../../tests"]));
			runCommand("haxe", ["RunCi.hxml"], 0xFFFFFF);
		}
	}

	function runIssues(target:TestTarget, args:Array<String>, ?run:String->Void) {
		if (!testIssues)
			return;
		changeDirectory(unitsDir);
		final issues = getIssues();
		infoMsg('Going to test issues $issues');
		final timeout = switch target {
			case Cpp | Cs: 0xFFFFFF;
			case _: 5.0;
		}
		for (issue in issues) {
			runCommand("haxe", ['compile-$target.hxml'].concat(args.concat(['-D', 'issue=$issue'])), timeout);
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
			#if ecso_times
			args = args.concat(["--times", "-D", "ecso-times"]);
			#end

			switch test {
				case Macro | Neko | Php | Python | Lua | Cppia | Java | Flash9:
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
				case Jvm:
					haxelibInstall("hxjava");
					testHaxe(Jvm);
					function runJvm(name:String) {
						runCommand("java", ["-jar", 'bin/$name.jar']);
					}
					for (level in 0...3) {
						final args = args.concat(["-D", "jvm.dynamic-level=" + level]);
						runIssues(Jvm, args, runJvm);
						runUnits(Jvm, args, runJvm);
						runSpecs(Jvm, args, runJvm);
					}
				case Cpp:
					testHaxe(Cpp);
					if(FileSystem.exists("bin/cpp")) try {
						if(systemName == "Windows")
							runCommand("rmdir", ["/Q/S", "bin/cpp"]);
						else
							runCommand("rm", ["-rf", "bin/cpp"]);
					} catch (_) {}
					function runCpp(name:String) {
						runCommand(FileSystem.fullPath('bin/cpp/$name/Main-debug'), []);
					}
					final archFlag = if (systemName == "Windows") "HXCPP_M32" else "HXCPP_M64";
					final args = ["-D", archFlag].concat(args);
					runIssues(Cpp, args, runCpp);
					runUnits(Cpp, args, runCpp);
					runSpecs(Cpp, args, runCpp);
				case Cs:
					testHaxe(Cs);
					for (unsafe in        [[], ["-D", "unsafe"]])
					for (fastcast in      [[], ["-D", "fast_cast"]])
					for (noroot in        [[], ["-D", "no_root"]])
					for (erasegenerics in [[], ["-D", "erase_generics"]])
					{
						final args = args.concat(unsafe).concat(fastcast).concat(erasegenerics).concat(noroot);
						function runCs(name:String) {
							final exe = FileSystem.fullPath('bin/cs/$name/bin/Main-Debug.exe');
							switch systemName {
								case "Linux", "Mac":
									runCommand("mono", [exe]);
								case "Windows":
									runCommand(exe, []);
							}
						}
						runIssues(Cs, args, runCs);
						runUnits(Cs, args, runCs);
						runSpecs(Cs, args, runCs);
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
	} else if(tests.length > 1) {
		successMsg('all tests succeeded');
	}
}
