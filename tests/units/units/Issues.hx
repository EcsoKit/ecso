package units;

import haxe.macro.Context;
import sys.FileSystem;

using StringTools;

macro function instantiateFrom(pack:String) {
	final issue = Context.definedValue("issue");
	final pack = pack.split(".");
	final path = pack.join("/");
	final project = 'issue$issue';
	final name = if (FileSystem.exists('$path/$project') && FileSystem.isDirectory('$path/$project')) {
		pack.push(project);
		'Main';
	} else {
		'Issue$issue';
	}
	final tp = {
		pack: pack,
		name: name,
		sub: null,
		params: null
	}
	return macro new $tp();
}
