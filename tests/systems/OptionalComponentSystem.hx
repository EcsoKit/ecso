package tests.systems;

import ecso.System;
import tests.components.PositionComponent;
import tests.components.VelocityComponent;
import tests.components.CollisionBox;
import tests.components.FixedComponent;

class OptionalComponentSystem implements System {

    public static function compute (a:VelocityComponent) {

        a.x += 1;
        
    }

}

class OptionalComponentSystem1 implements System {

    public static function compute (a:VelocityComponent, ?o1:PositionComponent) {

        a.x += 10;

        if (o1 != null) o1.x += 10;
        
    }

}

class OptionalComponentSystem2 implements System {

    public static function compute (a:VelocityComponent, ?o1:PositionComponent, ?o2:CollisionBox) {

        a.x += 100;

        if (o1 != null) o1.x += 100;
        if (o2 != null) a.y += 100;
        
    }

}

class OptionalComponentSystem3 implements System {

    public static function compute (a:VelocityComponent, ?o1:PositionComponent, ?o2:CollisionBox, ?o3:FixedComponent) {

        a.x += 1000;

        if (o1 != null) o1.x += 1000;
        if (o2 != null) o1.y += 1000;
        if (o3 != null) o3.value += 1000;
        
    }

}