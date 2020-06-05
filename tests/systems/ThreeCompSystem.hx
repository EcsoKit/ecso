package tests.systems;

import tests.components.CollisionBox;
import tests.components.PositionComponent;
import tests.components.VelocityComponent;
import ecso.System;

class ThreeCompSystem implements System {

    public static inline function compute (position:PositionComponent, collision:CollisionBox, velocity:VelocityComponent):Void {

        position.x = position.y = position.z = 3;
        collision.width = collision.height = collision.depth = 3;
        velocity.x = velocity.y = 3;

    }

}