package ecso;

import haxe.macro.Expr;
import haxe.macro.MacroStringTools;
import haxe.macro.ExprTools;
import haxe.macro.TypeTools;
import haxe.macro.ComplexTypeTools;
#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import ecso._core.Plugin;
#end
import haxe.extern.Rest;

final class Entity { } 

final class EntityGroup {

    public inline function new () { }

    public extern function createEntity <T> (def:T):Void;

    public extern function deleteEntity <T> (def:T):Void;

    public extern function process (systems:Rest<Any>):Void;

}