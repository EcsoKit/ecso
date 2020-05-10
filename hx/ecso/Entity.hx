package ecso;

import haxe.extern.Rest;

final class Entity { }

final class EntityGroup {

    public inline function new () {}

    public extern function createEntity <T> (def:T):Void;

    public extern function deleteEntity <T> (def:T):Void;

    public extern function foreachEntity (systems:Rest<Any>):Void;

}