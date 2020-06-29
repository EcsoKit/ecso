package units.issues;

class A {}

class B extends A {
	public function new() {}
}

class Issue8 extends Test {
	function testIssue8() {
		var success = false;
		final entities = new ecso.Entity.EntityGroup();
		entities.createEntity({x: ""});
		entities.foreachEntity(function(e:{x:String, ?b:A}) {
			e.b = new B();
		});
		entities.foreachEntity(function(e:{x:String}) {
			pass();
		});
	}
}
