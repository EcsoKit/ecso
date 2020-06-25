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
				if (e.indexOf("Ecso__' is already loaded") > 0)
					throw "[ECSO] Some errors are preventing the compilation. Try to restart the Completion Server.";
				else
					throw '[ECSO] Failed to load plugin: $e';
			}
		} else {
			plugin;
		}
	}

	static function getPluginPath ():String {
		final currentFile = (function(?p:PosInfos) return p.fileName)();
		final srcDir = currentFile.directory().directory().directory().directory();
		final system = Sys.systemName();
		final system = switch Sys.systemName() {
			case "Windows":
				final arch = Sys.environment()["processor_architecture"];
				"Windows" + (arch == null || arch.indexOf("64") >= 0 ? "64" : "32");
			case s:
				s;
		}
		return Path.join([srcDir, 'cmxs', system, 'plugin.cmxs']);
	}

}