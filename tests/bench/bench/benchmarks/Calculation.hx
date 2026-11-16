package bench.benchmarks;

import bench.Bench;

private typedef Inertia = {
	var mass:Float;
	var force:Float;
	var velocity:Float;
}

private class Group {
	public function new() {}
	@:ecso.create public extern function createEntity<T>(def:T):Void;
	@:ecso.delete public extern function deleteEntity<T>(def:T):Void;
	@:ecso.foreach public extern function foreachEntity(...systems:Any):Void;
}

class Calculation extends Bench {

	final entities = new Group();

	public function new() {
		super('foreach on dedicated calculation components', 10000, 2000);
	}

	function setup() {
		for (i in 0...entityCount)
			entities.createEntity({ mass: 500.0, force: 20.0,  velocity: 0.0 });
	}

	var dt = 0.1;
	function update() {
		entities.foreachEntity(e -> move(e, dt));
	}

	static function move(entity:Inertia, dt:Float) {
		entity.velocity += entity.force * dt / entity.mass; // could pre-calculate `F/m` in `v += dt / (F/m)`
	}
}
