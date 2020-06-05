package tests.components;

import ecso.Component;

class CollisionBox implements Component {

    public var width:Float = -1;
    public var height:Float = -1;
    public var depth:Float;
    public var enabled:Bool;
    public var collider:Null<CollisionBox>;

    public function new (w = 1, h = 1, ?d) {
        width = w;
        height = h;
        if (d != null) depth = d;
    }

}