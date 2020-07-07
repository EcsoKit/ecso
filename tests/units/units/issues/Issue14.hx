package units.issues;

class Issue14 extends Test {
	function testIssue14() {
		var entities = new ecso.Entity.EntityGroup();
		entities.createEntity({x: 0}); // keep system
		entities.foreachEntity(function(e:{x:Int}) {
			e.x = {
				var v = 3;
				v;
			}
		});
		pass();
	}
}
