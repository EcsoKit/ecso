package ecso._core;

import haxe.macro.TypeTools;
import haxe.ds.ReadOnlyArray;
import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.PosInfos;

using haxe.io.Path;

typedef EcsoPluginApi = {
	function init ():Void;
}

class Plugin {

	static var _plugin:EcsoPluginApi;
	static var plugin (get, null):EcsoPluginApi;
	static function get_plugin ():EcsoPluginApi {
		if(_plugin == null) {
			try {
				_plugin = eval.vm.Context.loadPlugin(getPluginPath());
			} catch(e:Dynamic) {
				throw '[ECSO] Failed to load plugin: $e';
			}
		}
		return _plugin;
	}

	static function getPluginPath ():String {
		var currentFile = (function(?p:PosInfos) return p.fileName)();
		var srcDir = currentFile.directory().directory().directory().directory();
		var p = Path.join([srcDir, 'cmxs', Sys.systemName(), 'plugin.cmxs']);
		return p;
	}

	public static function init ():Void {
		ecso._core.RComponent;
		plugin.init();
	}

}