package bench.benchmarks;

import bench.Bench;

private typedef Position = {
	var x:Int;
	var y:Int;
}

private typedef Initialised = {
	var initialised:Bool;
}

private class Group {
	public function new() {}
	@:ecso.create public extern function createEntity<T>(def:T):Void;
	@:ecso.delete public extern function deleteEntity<T>(def:T):Void;
	@:ecso.foreach public extern function foreachEntity(...systems:Any):Void;
}

class Initialisation extends Bench {

	final entities = new Group();

	public function new() {
		super('foreach with a heavy one-time initialisation', 1000000, 20);
	}

	function setup() {
		for (i in 0...entityCount)
			entities.createEntity({ x: i, y: i, initialised: false });
	}

	override function warmup():Void {
		for (_ in 0...warmupCount)
			entities.foreachEntity(e -> { return; initialise(e); });
	}

	function update() {
		entities.foreachEntity(initialise);
	}

	static function initialise(entity:Position & Initialised) {
		if (!entity.initialised) {
			Sys.sleep(0.000001);
			entity.initialised = true;
		}
	}
}
