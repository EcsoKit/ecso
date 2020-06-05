package tests.components;

import ecso.Component;

class PositionComponent implements Component {

    public var x:Int = 0;
    public var y:Int = -1;
    public var z:Int; 

    public inline function new (?x, ?y, ?z) {
        if (x != null) this.x = x;
        if (y != null) this.y = y;
        if (z != null) this.z = z;
    }
    
}