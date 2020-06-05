package tests.systems;

import ecso.System;
import tests.components.FixedComponent;

class FixedSystem implements System {

    public static var executionResult:Array<Int> = [];
    
    public static function run (component:FixedComponent):Void {
        
        component.value += 1;

        executionResult.push(component.value);
        
    }
    
}