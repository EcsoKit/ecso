package tests.systems;

import ecso.System;
import tests.components.AbstractPositionComponent;

class AbstractSystem implements System {
    
    public static function run (position:AbstractPositionComponent):Void {
        
        position.add(3, 6);
        
    }
    
}