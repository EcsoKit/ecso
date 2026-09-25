package bench;

import bench.benchmarks.FullArchetype;
import bench.benchmarks.Initialisation;
import bench.benchmarks.OptionalComponents;
import bench.benchmarks.PartialArchetype;

class Main {

	static function main() {
		final benches:Array<Bench> = [
			new FullArchetype("small chunks", 10000, 1000),
			new FullArchetype("big chunks", 2000000, 3),
			new PartialArchetype("small chunks", 10000, 1000),
			new PartialArchetype("big chunks", 2000000, 3),
			new OptionalComponents("small chunks", 10000, 1000),
			new OptionalComponents("big chunks", 2000000, 3),
			new Initialisation(),
		];
		for (bench in benches)
			bench.run();
	}
}
