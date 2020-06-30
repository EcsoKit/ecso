package units.issues;

class Issue10 extends Test {

	function testIssue10() {
		var entities = new ecso.Entity.EntityGroup();
		entities.createEntity({
			y: 0
		});
		entities.foreachEntity(function(e:{ ?y:Int }) {
			e.y = null;
		}, function(e:{ y:Int }):Void {
			fail();
		});
		pass();
	}
}