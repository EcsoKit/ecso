package tests.systems;

import ecso.System;
import tests.components.PositionComponent;
import tests.components.VelocityComponent;

// TODO optional @systemGroup(4) `4` can be enum value, string, int, or type (typedef, enum, abstract, class etc...)
// to later on in the user side call Ecso.runSystems() to run all systems 
// or Ecso.runSystems(4) to run a specific list of system that are annotated with @systemGroup(4)
// by default a system is @systemGroup(0) 
class MovementSystem implements System {
    
    /*
    
    FOR: function run (b:B, a:A):Void
    
    1. safly sort args: (a:A, b:B)
    2. store MovementSystem: list_of_`(A, B)`.push( MovementSystem.run )
    
    RUNTIME: if (entity.has(A) && entity.get(B)) for (system in list_of_`(A, B)`) f.run(entity.get(A), entity.get(B));
    
    
    FOR: function run (b:B, a:A, ?c:C):Void // todo
    
    1. safly sort args: (a:A, b:B, ?c:C)
    2. store MovementSystem: list_of_`(a:A, b:B, ?c:C)`.push( MovementSystem.run )
    
    RUNTIME: if (entity.has(A) && entity.get(B)) for (system in list_of_`(A, B)`) f.run(entity.get(A), entity.get(B), entity.tryGet(C));
    
    */
    
    public static function compute (position:PositionComponent, velocity:VelocityComponent):Void {
        
        position.x += velocity.x;
        position.y += velocity.y;
    }
    
}