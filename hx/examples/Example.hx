
// typedef Player = User & UserReader;
typedef Player = {
    var age : Int;
    var xp : Int;
    var data : String;
}
 
typedef User = {
    var age : Int;
    var xp : Int;
}
typedef UserReader = {
    var data : String;
}
 
typedef Pos = { x:Int, y:Int, fixed:Bool }
typedef Target = { target:Pos }
typedef Vel = { x:Int, y:Int }
abstract Entity<A,B> (Any) {
    
}
@:keep class Systems {

    public static function failsHere (entity:Entity<Pos,Vel>) : Void {}
    public static function fails (entity:Pos & Vel) : Void
    
        if (!entity.fixed) {
            entity.x += entity.x;
            entity.y += entity.y;
        }
    
    public static function updateEntity (entity : User & UserReader) : Void {
 
        // Called once for every entity that own both components `User` and `UserReader`.
 
    }
    #if redefine_component_references
    public static function redefineEntity (entity : User & UserReader & Pos) : User & Pos {
 
        // Same as `updateEntity`, but returns a new version for both the components `User` and `Pos`. 
 
        return entity;
    }
    #end
    public static function competeEntities (entity : User & Pos, against : Target) : Void {
 
        // Compete every entity that own both `User` and `Pos` once with every entity that own `Target`, excluding itself (`entity == against` will always be false).
 
        // If annotated with `@:self`, an entity owning `User`, `Pos` and `Target` will be called against itself (in such cases, `entity == against` will be true).
 
        // The entities order is unspecified.
    }
 
    public static function updateEntities (entities : Array<User & Pos>) : Void {
 
        // Called only once per group of entity.
 
    }
}
 
class Example {
    static function main() {
        
        // g.createEntity([
        //     (null:Pos),
        //     (null:Vel), // Error: Component type 'Vel' intersects 'Pos' with the field 'x'
        //     (null:Pos & Vel) // Valid: `Pos` & `Vel` are considered as a single component type `(Pos & Vel)` (Haxe specs)
        // ]);

        var g = new G();

        g.createEntity(({
            x: 0,
            y: 0,
            fixed: true
        }:Pos & Vel));

        g.createEntity({
            x: 0,
            y: 0,
            fixed: true
        });

        g.createEntity({
            position:{ x:Int, y:Int },
            velocity:{ x:Int, y:Int }
        });

        g.createEntity(({
            x: 0,
            y: 0,
            fixed: true
        }:Union<Pos & Vel>)); // Error: Component type 'Vel' intersects 'Pos' with the field 'x'
        
        g.process( Systems.fails );
    }
}

class G {
    public function new () {}
    public function process (s:Any) {}
    public function createEntity <T> (def:T) : Void
        trace("createEntity", def);
}

abstract Union <T> (T) from T { }