package runci;

import sys.FileSystem;

final systemName = Sys.systemName();
final cwd = Sys.getCwd();
final repoDir = FileSystem.fullPath("..") + "/";
final specsDir = cwd + "specs/";
final unitsDir = cwd + "units/";
final serverDir = cwd + "server/";

enum Ci {
	GithubActions;
}

final ci:Null<Ci> = {
	if (Sys.getEnv("GITHUB_WORKSPACE") != null)
		GithubActions;
	else
		null;
}

function isCi():Bool {
	return ci != null;
}

final colorSupported = switch [ci, systemName] {
	case [GithubActions, _]: true;
	case [_, "Linux" | "Mac"]: true;
	case [_, "Windows"]: false;
	case _: false;
}

private final defIssues = #if (issues || issue) true #else false #end;
private final defSpecs = #if (specs || spec) true #else false #end;
private final defUnits = #if (units || unit) true #else false #end;
private final testAll = !(defIssues || defSpecs || defUnits);
final testIssues = testAll || defIssues;
final testSpecs = testAll || defSpecs;
final testUnits = testAll || defUnits;