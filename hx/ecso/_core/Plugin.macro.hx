package ecso._core;

import haxe.macro.Context;
import haxe.PosInfos;

using haxe.io.Path;
using StringTools;

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
					case "windows-x86":
						'Haxe 32-bit is not yet supported, make sure to use an official 64-bit version of Haxe from https://haxe.org/download/.';
					case _.startsWith('l') => true:
						'On Linux distributions, the installed Haxe package may not be supported by Ecso, make sure to use the official Linux Haxe Binaries from https://haxe.org/download/.';
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
	static var tryArm = false;
	static function getPluginPath():String {
		final here = ((?p:PosInfos) -> p.fileName)();
		final src = here.directory().directory().directory().directory();
		final hx = getCompilerVersion();
		return Path.join([src, 'cmxs', 'hx-$hx', getSystem(), 'plugin.cmxs']);
	}
	static function getSystem():String {
		return Sys.systemName().toLowerCase() + "-" + getArch();
	}
	static function getArch():String {
		var uname = new sys.io.Process('uname -m');
		var arch = uname.stdout.readLine();
		uname.close();
		return arch;
	}
	static function getCompilerVersion():String {
		return Context.definedValue("haxe");
	}
}
