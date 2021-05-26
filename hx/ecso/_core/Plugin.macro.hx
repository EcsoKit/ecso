package ecso._core;

import haxe.macro.Context;
import haxe.PosInfos;

using haxe.io.Path;

private typedef EcsoPluginApi = {
	function init():Void;
	function registerContext(path:String, fields:Array<String>, isStatic:Bool):Void;
}

class Plugin {
	@:persistent static var plugin(get, default):EcsoPluginApi;

	static function init():Void {
		if (#if display true || #end Context.defined('display'))
			return;

		#if (haxe > "4.2.2")
		Context.fatalError('[ECSO] The alpha version only supports Haxe 4.2.2. Please update ECSO to a newer version when available at https://lib.haxe.org/p/ecso/', Context.currentPos());
		#elseif (haxe < "4.2.2")
		Context.fatalError('[ECSO] The alpha version only supports Haxe 4.2.2. Please update Haxe at https://haxe.org/', Context.currentPos());
		#end
		if (!Context.defined("hl") && !Context.defined("js") && !Context.defined("interp") && !Context.defined("cpp") && !Context.defined("cs"))
			Sys.println('\n[ECSO] Warning : Be aware the alpha version has not been thoroughly tested on other targets than HashLink, JavaScrip, CPP, C# or Eval (interp).\n                 You might hit unfriendly issues.\n');

		plugin.init();
	}

	static function get_plugin():EcsoPluginApi {
		return if (plugin == null) {
			try {
				plugin = eval.vm.Context.loadPlugin(getPluginPath());
			} catch (e:String) {
				if (e.indexOf("The operation completed successfully") < 0)
					throw '[ECSO] Failed to load plugin: $e';
				else
					Sys.println("[ECSO] The plugin seems to be loaded several times. If you encounter any error after seeing this message, please file an issue at https://github.com/EcsoKit/ecso/issues.");
				{ init: function(){}, registerContext: function(a,b,c){} }
			}
		} else {
			plugin;
		}
	}

	static function getPluginPath():String {
		final here = ((?p:PosInfos) -> p.fileName)();
		final src = here.directory().directory().directory().directory();
		final system = switch Sys.systemName() {
			case "Windows":
				final wmic = new sys.io.Process("WMIC OS GET osarchitecture /value");
				final arch = switch wmic.stdout.readAll().toString() {
					case v if (v.indexOf("64") >= 0):
						"64";
					case v if (v.indexOf("32") >= 0):
						"32";
					case _:
						Context.warning("{ECSO} failed to resolve your processor architecture - please report this at https://github.com/EcsoKit/ecso/issues", Context.currentPos());
						"64";
				}
				wmic.close();
				'Windows$arch';
			case s:
				s;
		}
		final hx = Context.definedValue("haxe");
		return Path.join([src, 'cmxs', 'hx-$hx', system, 'plugin.cmxs']);
	}
}
