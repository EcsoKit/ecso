package units.issues;

import ecso.Entity;

class Issue29 extends Test {
	function testIssueX() {
		final g = new EntityGroup();
		g.createEntity({ z: "" });
		g.foreachEntity(addB);
		g.foreachEntity(runB);
	}
}

function addB(e : { ?b:String }) {
	e.b = "";
}

function runB(e: { b:String, ?c:String }) {
	pass();
}