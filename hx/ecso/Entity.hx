package ecso;


final class Entity { }

final class EntityGroup {

    public inline function new () {}

    @:ecso.create public extern function createEntity <T> (def:T):Void;

    @:ecso.delete public extern function deleteEntity <T> (def:T):Void;

    @:ecso.foreach public extern function foreachEntity (...systems:Any):Void;

}