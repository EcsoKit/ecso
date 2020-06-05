package ecso._core;

import haxe.macro.TypeTools;
import haxe.ds.ReadOnlyArray;
import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.PosInfos;

using haxe.io.Path;

typedef EcsoPluginApi = {
	function hello():Void;
	function stringifyPosition(p:Position):String;
	function hijackStaticTest():Void;

	function registerComponent (name:String, p:Position):Void;
	function init ():Void;
}

class Plugin {

	static function buildEntity ():ComplexType {

		switch (Context.getLocalType()) {
			case TInst(_, params):
				trace("-----TIsnt bb");
				for(p in params)
					trace(p);
				
				trace("--------------");
			case t:
				Context.error("Class expected", Context.currentPos());
		}

		if (Math.random() > .5) return macro:Toto<Int>;
		return macro:Toto<String, Float>;

		return null;
	}

	public static function registerComponent (t:Type, p:Position):Void {

		final tname = switch t {
			case TType(_, _):
				TypeTools.toString(t);
				// t.get().name;
			case _:
				Context.fatalError( "TType expected but has " + t.getName(), p );
		}

		plugin.registerComponent( tname, p );

	}

	public static function registerSystem (t:Type, fname:String, p:Position):Void {

	}

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
		trace("TRY TO LOAD", p);
		return p;
	}

	static function init ():Void {
		ecso._core.RComponent;
		new ecso._core.RComponent<Int>(1).addAt(0, 0);
		// plugin.init();
	}

}