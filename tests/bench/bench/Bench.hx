package bench;

abstract class Bench {

	final name:String;
	final entityCount:Int;
	final updateCount:Int;
	final warmupCount:Int;

	function new(name:String, entityCount:Int, updateCount:Int, warmupCount:Int = 10) {
		this.name = name;
		this.entityCount = entityCount;
		this.updateCount = updateCount;
		this.warmupCount = warmupCount;
	}

	abstract function setup():Void;

	abstract function update():Void;

	public function run():Void {
		setup();
		for (_ in 0...warmupCount) update();
		final startTime = Sys.time();
		for (_ in 0...updateCount) update();
		final duration = Sys.time() - startTime;
		final updatesPerSecond = updateCount / duration;
		Sys.println('$name: ${Math.round(updatesPerSecond)} updates/s ($updateCount updates on $entityCount entities in ${Math.round(duration * 100) / 100}s)');
	}
}
