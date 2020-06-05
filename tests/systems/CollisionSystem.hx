package tests.systems;

import tests.components.CollisionBox;
import tests.components.PositionComponent;
import ecso.System;

class CollisionSystem implements System {

    public static inline function compute (boxes:Array<CollisionBox>, locs:Array<PositionComponent>, position:PositionComponent, collision:CollisionBox):Void {

        inline function inside (x:Float, a:Float, b:Float):Bool {
            return x > a && x < b;
        }

        for (i in 0...boxes.length) {

            var box = boxes[i];
            var loc = locs[i];
            box.collider = null;

            if (box.getComponentID() != collision.getComponentID()) {
                
                if (
                       inside(position.x, loc.x, loc.x + box.width) || inside(position.x + collision.width, loc.x, loc.x + box.width)
                    && inside(position.y, loc.y, loc.y + box.height) || inside(position.y + collision.height, loc.y, loc.y + box.height)
                    && inside(position.z, loc.z, loc.z + box.depth) || inside(position.z + collision.depth, loc.z, loc.z + box.depth)
                ) {
                    box.collider = collision;
                    collision.collider = box;
                }

            }
        }

    }

}