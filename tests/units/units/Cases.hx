package units;

import haxe.macro.Context;
import sys.FileSystem;

using StringTools;

macro function instantiateFrom(pack:String) {
	final project = Context.definedValue("case");
	final pack = pack.split(".");
	final path = pack.join("/");
	final name = if (FileSystem.exists('$path/$project') && FileSystem.isDirectory('$path/$project')) {
		pack.push(project);
		'Main';
	} else {
		project;
	}
	final tp = {
		pack: pack,
		name: name,
		sub: null,
		params: null
	}
	return macro new $tp();
}
