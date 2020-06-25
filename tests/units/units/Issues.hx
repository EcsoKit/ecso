package units;

using StringTools;

macro function addClasses(dir:String, pack:String) {
	var dir = sys.FileSystem.readDirectory(dir);
	var el = [];
	function add(className:String) {
		var tp = {
			pack: pack.split("."),
			name: className,
			sub: null,
			params: null
		}
		el.push(macro classes.push(new $tp()));
	}
	for (file in dir) {
		if (!file.endsWith(".hx")) {
			continue;
		}
		add(file.substr(0, -3));
	}
	return macro $b{el};
}
