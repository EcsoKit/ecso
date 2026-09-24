package bench.benchmarks;

import bench.Bench;

private typedef Position = {
	var x:Int;
	var y:Int;
}

private typedef Velocity = {
	var vx:Int;
	var vy:Int;
}

private class Group {
	public function new() {}
	@:ecso.create public extern function createEntity<T>(def:T):Void;
	@:ecso.delete public extern function deleteEntity<T>(def:T):Void;
	@:ecso.foreach public extern function foreachEntity(...systems:Any):Void;
}

class FullArchetype extends Bench {

	final entities = new Group();

	public function new(details:String, entityCount:Int, updateCount:Int) {
		super('foreach with a full entity archetype ($details)', entityCount, updateCount);
	}

	function setup() {
		for (i in 0...entityCount)
			entities.createEntity({ x: i, y: i, vx: 1, vy: 1 });
	}

	function update() {
		entities.foreachEntity(move);
	}

	static function move(entity:Position & Velocity) {
		entity.x += entity.vx;
		entity.y += entity.vy;
	}
}
