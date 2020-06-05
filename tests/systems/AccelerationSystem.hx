package tests.systems;

import ecso.System;
import tests.components.VelocityComponent;

class AccelerationSystem implements System {

    public static function run (velocity:VelocityComponent) {

        velocity.x *= 2;
        velocity.y *= 2;

    }

    public function new () {

    }

}