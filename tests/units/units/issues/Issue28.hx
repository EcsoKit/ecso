package units.issues;

typedef Single = { single:Int }
typedef Double = { double:Int }

class Issue28 extends Test {
	function testIssue28() {
		var g = new ecso.Entity.EntityGroup();

		g.createEntity({ single: 1 });
		g.createEntity({ double: 2 });

		// Singles

		g.foreachEntity(function(e:Single & { ?a:Int, ?b:Int }) {
			final n:Null<Int> = 1;
			e.a = n;
			e.b = n;
		});

		g.foreachEntity(function(e:Single & { ?c:Int, ?d:Int }) {
			final n:Null<Int> = 1;
			e.c = n;
			e.d = n;
		});

		g.foreachEntity(function(e:Single & { ?e:Int, ?f:Int }) {
			final n:Null<Int> = 1;
			e.e = n;
			e.f = n;
		});

		g.foreachEntity(function(e:Single & { ?g:Int, ?h:Int }) {
			final n:Null<Int> = 1;
			e.g = n;
			e.h = n;
		});

		g.foreachEntity(function(e:Single & { ?i:Int, ?j:Int }) {
			final n:Null<Int> = 1;
			e.i = n;
			e.j = n;
		});

		g.foreachEntity(function(e:Single & { ?k:Int, ?l:Int }) {
			final n:Null<Int> = 1;
			e.k = n;
			e.l = n;
		});
		
		g.foreachEntity(function(e:Single & { ?m:Int, ?n:Int }) {
			final n:Null<Int> = 1;
			e.m = n;
			e.n = n;
		});
		
		g.foreachEntity(function(e:Single & { ?o:Int, ?p:Int }) {
			final n:Null<Int> = 1;
			e.o = n;
			e.p = n;
		});
		
		g.foreachEntity(function(e:Single & { ?q:Int, ?r:Int }) {
			final n:Null<Int> = 1;
			e.q = n;
			e.r = n;
		});
		
		g.foreachEntity(function(e:Single & { ?s:Int, ?t:Int }) {
			final n:Null<Int> = 1;
			e.s = n;
			e.t = n;
		});
		
		g.foreachEntity(function(e:Single & { ?u:Int, ?v:Int }) {
			final n:Null<Int> = 1;
			e.u = n;
			e.v = n;
		});
		
		g.foreachEntity(function(e:Single & { ?w:Int, ?x:Int }) {
			final n:Null<Int> = 1;
			e.w = n;
			e.x = n;
		});
		
		g.foreachEntity(function(e:Single & { ?y:Int, ?z:Int }) {
			final n:Null<Int> = 1;
			e.y = n;
			e.z = n;
		});

		// Doubles

		g.foreachEntity(function(e:Double & { ?aa:Int, ?bb:Int }) {
			final n:Null<Int> = 1;
			e.aa = n;
			e.bb = n;
		});

		g.foreachEntity(function(e:Double & { ?cc:Int, ?dd:Int }) {
			final n:Null<Int> = 1;
			e.cc = n;
			e.dd = n;
		});

		g.foreachEntity(function(e:Double & { ?ee:Int, ?ff:Int }) {
			final n:Null<Int> = 1;
			e.ee = n;
			e.ff = n;
		});

		g.foreachEntity(function(e:Double & { ?gg:Int, ?hh:Int }) {
			final n:Null<Int> = 1;
			e.gg = n;
			e.hh = n;
		});

		g.foreachEntity(function(e:Double & { ?ii:Int, ?jj:Int }) {
			final n:Null<Int> = 1;
			e.ii = n;
			e.jj = n;
		});

		g.foreachEntity(function(e:Double & { ?kk:Int, ?ll:Int }) {
			final n:Null<Int> = 1;
			e.kk = n;
			e.ll = n;
		});
		
		g.foreachEntity(function(e:Double & { ?mm:Int, ?nn:Int }) {
			final n:Null<Int> = 1;
			e.mm = n;
			e.nn = n;
		});
		
		g.foreachEntity(function(e:Double & { ?oo:Int, ?pp:Int }) {
			final n:Null<Int> = 1;
			e.oo = n;
			e.pp = n;
		});
		
		g.foreachEntity(function(e:Double & { ?qq:Int, ?rr:Int }) {
			final n:Null<Int> = 1;
			e.qq = n;
			e.rr = n;
		});
		
		g.foreachEntity(function(e:Double & { ?ss:Int, ?tt:Int }) {
			final n:Null<Int> = 1;
			e.ss = n;
			e.tt = n;
		});

		g.foreachEntity(function(e:Double & { ?uu:Int, ?vv:Int }) {
			final n:Null<Int> = 1;
			e.uu = n;
			e.vv = n;
		});
		
		g.foreachEntity(function(e:Double & { ?ww:Int, ?xx:Int }) {
			final n:Null<Int> = 1;
			e.ww = n;
			e.xx = n;
		});
		
		g.foreachEntity(function(e:Double & { ?yy:Int, ?zz:Int }) {
			final n:Null<Int> = 1;
			e.yy = n;
			e.zz = n;
		});

		pass();
	}
}
