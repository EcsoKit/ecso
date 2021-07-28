package units.issues;

class Issue27 extends Test {
	function testIssue27() {
		var g = new ecso.Entity.EntityGroup();

		g.createEntity({});

		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.a = n;
			e.b = n;
		});

		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.c = n;
			e.d = n;
		});

		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.e = n;
			e.f = n;
		});

		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.g = n;
			e.h = n;
		});

		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.i = n;
			e.j = n;
		});

		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.k = n;
			e.l = n;
		});
		
		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.m = n;
			e.n = n;
		});
		
		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.o = n;
			e.p = n;
		});
		
		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.q = n;
			e.r = n;
		});
		
		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.s = n;
			e.t = n;
		});

		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.u = n;
			e.v = n;
		});
		
		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.w = n;
			e.x = n;
		});
		
		g.foreachEntity(function(e) {
			final n:Null<Int> = 1;
			e.y = n;
			e.z = n;
		});

		pass();
	}
}
