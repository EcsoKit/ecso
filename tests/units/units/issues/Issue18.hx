package units.issues;

import ecso.Entity;

using Lambda;

class Issue18 extends Test {
	function testIssue18() {
		var a = "";

		a = makeBranches(100);

		#if !macro
		final g = new EntityGroup();
		g.createEntity({
			a: a
		});
		#end

		pass();
	}

	static macro function makeBranches(amount:Int) {
		final e = macro if (true) "a" else "b";
		for (i in 0...amount) {
			e.expr = (macro if (Math.random() > .5) $v{"" + i} else ${{expr: e.expr, pos: e.pos}}).expr;
		}
		return e;
	}
}
