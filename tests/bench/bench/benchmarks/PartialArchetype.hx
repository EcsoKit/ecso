package bench.benchmarks;

import bench.Bench;

private typedef Position = {
	var x:Int;
	var y:Int;
}

private typedef Mass = {
	var mass:Int;
}

private class Group {
	public function new() {}
	@:ecso.create public extern function createEntity<T>(def:T):Void;
	@:ecso.delete public extern function deleteEntity<T>(def:T):Void;
	@:ecso.foreach public extern function foreachEntity(...systems:Any):Void;
}

class PartialArchetype extends Bench {

	final entities = new Group();

	public function new(details:String, entityCount:Int, updateCount:Int) {
		super('foreach with a partial entity archetype ($details)', entityCount, updateCount);
	}

	function setup() {
		for (i in 0...entityCount)
			entities.createEntity({ x: i, y: i, mass: i % 2 == 0 ? 1 : null });
	}

	function update() {
		entities.foreachEntity(move);
		entities.foreachEntity(feed);
	}

	static function move(entity:Position) {
		entity.x += 1;
		entity.y += 1;
	}

	static function feed(entity:Mass) {
		entity.mass += 1;
	}
}
