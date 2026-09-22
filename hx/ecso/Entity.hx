package ecso;

/**
	This type unifies with any entity archetype definition.
	It is intended to be used as a type parameter constraint. If used as a real
	type, the underlying type will be `Dynamic`.
**/
@:ecso.entity_constraint
// abstract Entity<T={}>(T) from T {}
// abstract Entity<Archetype={}, C:Archetype&{}={}>(Archetype) {}
abstract Entity(Dynamic) {}

final class EntityGroup {

    public inline function new () {}

    @:ecso.create public extern function createEntity <T> (def:T):Void;

    @:ecso.delete public extern function deleteEntity <T:Entity> (def:T):Void;

    @:ecso.foreach public extern function foreachEntity <T:Entity> (...systems:(T)->Void):Void;

}