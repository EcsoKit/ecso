package units.issues;

import ecso.Entity;

class Issue19 extends Test {
	function testIssue19() {

		final g = new EntityGroup();

		g.createEntity({
			a: makeExpr()
		});
		
		g.foreachEntity(function(e:{a:String}) {
			pass();
		});
	}
	static macro function makeExpr() {
		return macro "string";
	}
}
