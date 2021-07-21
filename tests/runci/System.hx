package runci;

import haxe.Timer;
import sys.io.Process;
import runci.Config;

using StringTools;

enum Failure {
	Fail;
}

var success(default, null) = true;

function successMsg(msg:String):Void {
	Sys.println(colorSupported ? '\x1b[32m' + msg + '\x1b[0m' : msg);
}

function failMsg(msg:String):Void {
	Sys.println(colorSupported ? '\x1b[31m' + msg + '\x1b[0m' : msg);
}

function infoMsg(msg:String):Void {
	Sys.println(colorSupported ? '\x1b[36m' + msg + '\x1b[0m' : msg);
}

function commandSucceed(cmd:String, args:Array<String>):Bool {
	return try {
		var p = new Process(cmd, args);
		var succeed = p.exitCode() == 0;
		p.close();
		succeed;
	} catch (e:Dynamic) false;
}

function commandResult(cmd:String, args:Array<String>):{
	stdout:String,
	stderr:String,
	exitCode:Int
} {
	var p = new Process(cmd, args);
	var out = {
		stdout: p.stdout.readAll().toString(),
		stderr: p.stderr.readAll().toString(),
		exitCode: p.exitCode()
	}
	p.close();
	return out;
}

/**
	Run a command using `Sys.command()`.
	If the command exits with non-zero code, exit the whole script with the same code.
	If `useRetry` is `true`, the command will be re-run if it exits with non-zero code (3 trials).
	It is useful for running network-dependent commands.

	@param timeout Default timeout of 300 seconds (5 minutes).
 */
function runCommand(cmd:String, ?args:Array<String>, useRetry:Bool = false, allowFailure:Bool = false, timeout = 300.000):Void {
	var trials = useRetry ? 3 : 1;
	var exitCode:Int = 1;
	var cmdStr = cmd + (args == null ? '' : ' $args');

	while (trials-- > 0) {
		infoMsg('Command: $cmdStr');

		var t = Timer.stamp();
		exitCode = Sys.command(cmd, args);
		var fdt = Timer.stamp() - t;
		var dt = Math.round(fdt);

		if (fdt > timeout) {
			failMsg('Command timeout that exited with $exitCode in ${fdt}s (max ${timeout}s): $cmdStr');
			trials = 0;
		} else if (exitCode == 0) {
			successMsg('Command exited with $exitCode in ${dt}s: $cmdStr');
			return;
		} else
			failMsg('Command exited with $exitCode in ${dt}s: $cmdStr');

		if (trials > 0) {
			infoMsg('Command will be re-run...');
		}
	}

	if (!allowFailure)
		fail();
}

/**
 * Recursively delete a directory.
 * @return Int Exit code of a system command executed to perform deletion.
 */
function deleteDirectoryRecursively(dir:String):Int {
	return switch (Sys.systemName()) {
		case "Windows":
			Sys.command("rmdir", ["/S", "/Q", StringTools.replace(sys.FileSystem.fullPath(dir), "/", "\\")]);
		case _:
			Sys.command("rm", ["-rf", dir]);
	}
}

function fail():Void {
	success = false;
	throw Fail;
}

function addToPATH(path:String):Void {
	infoMsg('Prepending $path to PATH.');
	switch (systemName) {
		case "Windows":
			Sys.putEnv("PATH", path + ";" + Sys.getEnv("PATH"));
		case "Mac", "Linux":
			Sys.putEnv("PATH", path + ":" + Sys.getEnv("PATH"));
	}
}

function haxelibInstallGit(account:String, repository:String, ?branch:String, ?srcPath:String, useRetry:Bool = false, ?altName:String):Void {
	var name:String = (altName == null) ? repository : altName;
	try {
		getHaxelibPath(name);
		infoMsg('$name has already been installed.');
	} catch (e:Dynamic) {
		var args:Array<String> = ["git", name, 'https://github.com/$account/$repository'];
		if (branch != null) {
			args.push(branch);
		}
		if (srcPath != null) {
			args.push(srcPath);
		}

		runCommand("haxelib", args, useRetry);
	}
}

function haxelibInstall(library:String):Void {
	try {
		getHaxelibPath(library);
		infoMsg('$library has already been installed.');
	} catch (e:Dynamic) {
		runCommand("haxelib", ["install", library]);
	}
}

function haxelibInstallDev(library:String, path:String):Void {
	try {
		getHaxelibPath(library);
		infoMsg('$library has already been installed.');
	} catch (e:Dynamic) {
		runCommand("haxelib", ["dev", library, path]);
	}
}

function haxelibRun(args:Array<String>, useRetry:Bool = false):Void {
	runCommand("haxelib", ["run"].concat(args), useRetry);
}

function getHaxelibPath(libName:String) {
	var proc = new Process("haxelib", ["path", libName]);
	var result;
	var code = proc.exitCode();
	do {
		result = proc.stdout.readLine();
		if (!result.startsWith("-L")) {
			break;
		}
	} while (true);
	proc.close();
	if (code != 0) {
		throw 'Failed to get haxelib path ($result)';
	}
	return result;
}

function changeDirectory(path:String) {
	Sys.println('Changing directory to $path');
	Sys.setCwd(path);
}

macro function getTests() {
	final test = switch haxe.macro.Context.definedValue("test") {
		case null | "" | "1" | "0" | "true" | "false": macro Sys.getEnv("TEST");
		case test: macro $v{test};
	}
	return macro {
		final args = Sys.args();
		switch (args.length == 1 ? args[0] : $test) {
			case null:
				[Macro];
			case env:
				[for (v in env.split(",")) v.trim().toLowerCase()];
		}
	}
}

macro function getIssues() {
	final issues = switch haxe.macro.Context.definedValue("issues") {
		case null | "" | "1" | "0" | "true" | "false":
			[];
		case issues:
			[for (v in issues.split(",")) v.trim()];
	}
	switch haxe.macro.Context.definedValue("issue") {
		case null | "" | "1" | "0" | "true" | "false":
		case issue:
			for (v in issue.split(",")) {
				final v = v.trim();
				issues.remove(v);
				issues.push(v);
			}
	}
	if (issues.length == 0) {
		return macro [for (file in sys.FileSystem.readDirectory(unitsDir + "units/issues")) {
			if (!file.endsWith(".hx") || !file.startsWith("Issue"))
				continue;
			file.substring(5, file.length - 3);
		}];
	} else {
		return macro $v{issues};
	}
}