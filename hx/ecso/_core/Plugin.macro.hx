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

	@:persistent static var plugin (get,default):EcsoPluginApi;

	public static function init ():Void {

		if( #if display true || #end Context.defined('display') )
			return;

		plugin.init();
	}

	static function get_plugin ():EcsoPluginApi {
		return if( plugin == null ) {
			plugin = try {
				eval.vm.Context.loadPlugin( getPluginPath() );
			} catch (e:String) {
				throw '[ECSO] Failed to load plugin: $e';
			}
		} else {
			plugin;
		}
	}

	static function getPluginPath ():String {
		final currentFile = (function(?p:PosInfos) return p.fileName)();
		final srcDir = currentFile.directory().directory().directory().directory();
		return Path.join([srcDir, 'cmxs', Sys.systemName(), 'plugin.cmxs']);
	}

}