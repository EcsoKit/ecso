package units.issues;

class Issue24 extends Test {
	function testIssue24() {
		var g = new ecso.Entity.EntityGroup();

		g.createEntity({});

		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.a = n;
			e.b = n;
			e.c = n;
			e.d = n;
			e.e = n;
			e.f = n;
			e.g = n;
			e.h = n;
			e.i = n;
			e.j = n;
			e.k = n;
			e.l = n;
			e.m = n;
			e.n = n;
			e.o = n;
			e.p = n;
			e.q = n;
			e.r = n;
			e.s = n;
			e.t = n;
			e.u = n;
			e.v = n;
			e.w = n;
			e.x = n;
			e.y = n;
			e.z = n;
			pass();
		});
	}
}
