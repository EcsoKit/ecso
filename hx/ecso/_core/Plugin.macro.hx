package ecso._core;

import haxe.macro.Context;
import haxe.PosInfos;

using haxe.io.Path;

private typedef EcsoPluginApi = {
	function init():Void;
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
				var exists = sys.FileSystem.exists(path);
				final hint = switch getSystem() {
					case "Windows64":
						try32 = true;
						if (sys.FileSystem.exists(getPluginPath()))
							return get_plugin();
						null;
					case "Windows32":
						'Haxe 32-bit is not yet supported, make sure to use an official 64-bit version of Haxe from https://haxe.org/download/.';
					case "Linux":
						'On Linux distributions, the installed Haxe package may not be supported by Ecso, make sure to use the official Linux Haxe Binaries from https://haxe.org/download/.';
					case "Mac":
						tryArm = true;
						if (sys.FileSystem.exists(getPluginPath()))
							return get_plugin();
					case _:
						null;
				}
				if (!exists)
					Context.fatalError('[ECSO] No support for Haxe ${Context.definedValue("haxe")} on ${Sys.systemName()}.', Context.currentPos());
				throw '[ECSO] Failed to load plugin: $e' + (hint != null ? '\nHint: $hint' : '');
			}
		} else {
			plugin;
		}
	}
	static var try32 = false;
	static var tryArm = false;
	static function getPluginPath():String {
		final here = ((?p:PosInfos) -> p.fileName)();
		final src = here.directory().directory().directory().directory();
		final hx = Context.definedValue("haxe");
		return Path.join([src, 'cmxs', 'hx-$hx', getSystem(), 'plugin.cmxs']);
	}
	static function getSystem():String {
		return switch Sys.systemName() {
			case "Windows":
				'Windows${try32 ? "32" : "64"}';
			case "Mac":
				'Mac${tryArm ? "-arm64" : ""}';
			case s:
				s;
		}
	}
}
