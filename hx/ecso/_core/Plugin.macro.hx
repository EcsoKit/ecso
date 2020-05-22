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

	@:persistent static var plugin:EcsoPluginApi;

	public static function init ():Void {
		if( plugin == null ) {
			try {
				plugin = eval.vm.Context.loadPlugin(getPluginPath());
				if( plugin != null )
					plugin.init();
			} catch (e:String) {
				if( e == null || e.indexOf("is already loaded") < 0 )
					throw '[ECSO] Failed to load plugin: $e';
			}
		}
	}

	static function getPluginPath ():String {
		final currentFile = (function(?p:PosInfos) return p.fileName)();
		final srcDir = currentFile.directory().directory().directory().directory();
		return Path.join([srcDir, 'cmxs', Sys.systemName(), 'plugin.cmxs']);
	}

}