package units.issues;

class A {}

class B extends A {
	public function new() {}
}

class Issue9 extends Test {
	function testIssue9() {
		var entities = new ecso.Entity.EntityGroup();
		entities.createEntity({x: "", a: ""});
		entities.foreachEntity(function(e:{x:String, ?b:A}) {
			e.b = new B();
		});
		entities.foreachEntity(function(e:{x:String, a:String}) {
			pass();
		});
	}
}
