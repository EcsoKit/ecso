package units.issues;

import ecso.Entity;


function a2ab(e : { a:Int, ?b:Int }) {
	e.b = 0;
}
function ab2abc(e : { a:Int, b:Int, ?c:Int }) {
	e.c = 0;
}
function abc2abcd(e : { a:Int, b:Int, c:Int, ?d:Int }) {
	e.d = 0;
}


class Issue21 extends Test {
	function testIssue21() {

		final g = new EntityGroup();

		g.createEntity({
			a: 0,
		});

		g.foreachEntity( a2ab );
		g.foreachEntity( ab2abc );
		g.foreachEntity( abc2abcd );

		g.foreachEntity(function (e : { d:Int, ?e : Int }) {
			pass();
		});
	}
}
