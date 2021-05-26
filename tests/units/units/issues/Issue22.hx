package units.issues;

import ecso.Entity;

class Issue22 extends Test {
	function testIssue22() {

		final g = new EntityGroup();

		g.createEntity({ a: 0 });

		var system = e -> { e.a--; fail(); }
		g.foreachEntity( {
			system = e -> { e.a++; pass(); }
			e -> e.a = 100;
		} );
		g.foreachEntity( system );
	}
}
