import ecso.Entity;

function main() {
	var g = new EntityGroup();
	Base.build(g);
	g.foreachEntity(function(e:{x:Int}) {
		trace("x", e.x);
	});
	trace("foo");
}