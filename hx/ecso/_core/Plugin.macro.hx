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

		if (!Context.defined("hl") && !Context.defined("js") && !Context.defined("interp") && !Context.defined("cpp") && !Context.defined("cs"))
			Sys.println('\n[ECSO] Warning : Be aware the alpha version has not been thoroughly tested on other targets than HashLink, JavaScrip, CPP, C# or Eval (interp).\n                 You might hit unfriendly issues.\n');

		plugin.init();
	}

	static function get_plugin():EcsoPluginApi {
		final path = getPluginPath();
		return if (plugin == null) {
			try {
				plugin = eval.vm.Context.loadPlugin(path);
			} catch (e:String) {
				if (!sys.FileSystem.exists(path))
					Context.fatalError('[ECSO] No support for Haxe ${Context.definedValue("haxe")}.', Context.currentPos());
				throw '[ECSO] Failed to load plugin: $e';
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
				final wmic = new sys.io.Process("sigcheck.exe " + Sys.executablePath());
				final arch = switch wmic.stdout.readAll().toString() {
					case v if (v.indexOf("64-bit") >= 0):
						"64";
					case v if (v.indexOf("32-bit") >= 0):
						"32";
					case v:
						Context.warning('{ECSO} failed to resolve your processor architecture - please report this at https://github.com/EcsoKit/ecso/issues with the following info:\n$v', Context.currentPos());
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
