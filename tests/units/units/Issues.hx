package units;

import haxe.macro.Context;

using StringTools;

macro function instantiateFrom(pack:String) {
	var tp = {
		pack: pack.split("."),
		name: 'Issue${Context.definedValue("issue")}',
		sub: null,
		params: null
	}
	return macro new $tp();
}
