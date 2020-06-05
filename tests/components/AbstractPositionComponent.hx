package tests.components;

import ecso.Component;

abstract AbstractPositionComponent (PositionComponent) from PositionComponent {

    public function add (x, y):Void {

        if (this.x != 3 || this.y != 3) return;

        this.x += x;
        this.y += y;
    }
    
}