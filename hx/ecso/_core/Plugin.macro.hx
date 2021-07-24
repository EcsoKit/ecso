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
				final hint = switch Sys.systemName() {
					case "Windows" if (!try32):
						try32 = true;
						return get_plugin();
					case "Windows":
						'Haxe 32-bit is not yet supported, make sure to use an official 64-bit version of Haxe from https://haxe.org/download/.';
					case "Linux":
						'On Linux distributions, the installed Haxe package may not be supported by Ecso, make sure to use the official Linux Haxe Binaries from https://haxe.org/download/.';
					case _:
						null;
				}
				// Linux Software Packages
				throw '[ECSO] Failed to load plugin: $e' + (hint != null ? '\nHint: $hint' : '');
			}
		} else {
			plugin;
		}
	}
	static var try32 = false;
	static function getPluginPath():String {
		final here = ((?p:PosInfos) -> p.fileName)();
		final src = here.directory().directory().directory().directory();
		final system = switch Sys.systemName() {
			case "Windows":
				'Windows${try32 ? "32" : "64"}';
			case s:
				s;
		}
		final hx = Context.definedValue("haxe");
		return Path.join([src, 'cmxs', 'hx-$hx', system, 'plugin.cmxs']);
	}
}
