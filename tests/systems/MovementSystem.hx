package tests.systems;

import ecso.Entity;
import tests.components.*;

class MovementSystem {

    public static function move (e:PositionComponent & VelocityComponent):Void {
        e.x += e.vx;
        e.y += e.vy;
    }

    public static function decelerate (e:VelocityComponent):Void {

        e.vx -= 1;
        e.vy -= 1;
    }

    public static function moveAndStop (e:PositionComponent & VelocityComponent & { spy:PositionComponent }, g:EntityGroup):Void {

        move(e);
        e.spy.x = e.x;
        e.spy.y = e.y;
        g.deleteEntity( e );
    }

    public static function spy (e:PositionComponent & VelocityComponent & { spy:PositionComponent&VelocityComponent }):Void {

        e.spy.x = e.x;
        e.spy.y = e.y;
        e.spy.vy = e.vy;
        e.spy.vx = e.vx;
    }
    
}