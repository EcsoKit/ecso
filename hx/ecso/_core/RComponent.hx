package ecso._core;

@:keep abstract RComponent<T> (Array<T>) {

    public inline function new (capacity:Int) {
        this = [];
    }

    @:keep public inline function addAt (v:T, i:Int):Void {
        this.push(v);
        new RComponent(24);
    }

    public inline function removeAt (i:Int):Void {
        this[i] = null;
    }

    public inline function getAt (i:Int):T {
        return this[i];
    }

    public inline function indexOf (v:T):Int {
        return this.indexOf(v);
    }
}