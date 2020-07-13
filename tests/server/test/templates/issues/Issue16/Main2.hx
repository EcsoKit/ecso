import ecso.Entity;

function main() {
	var g = new EntityGroup();
	Base.build(g);
	var ran = false;
	g.foreachEntity(function(e:{x:Int}) {
		trace("x", e.x);
		ran = true;
	});
	trace("foobar");
	if(!ran) throw "system didn't run";
}