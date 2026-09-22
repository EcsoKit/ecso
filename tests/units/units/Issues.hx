package units;

import haxe.macro.Context;
import sys.FileSystem;

using StringTools;

// TODO open issue: use a component typed `Some<T>` which is defined as  `typedef Some<T> = ds.Some<T>` AND ds.Some<T> is annotated with @:generic

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
