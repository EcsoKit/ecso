package units.issues.issue31;

import ecso.Entity;
import haxe.macro.Expr;

class Macro {
	public static function build():Array<Field> {
		var fields = [];
		final g = new EntityGroup();
		g.createEntity({ expected: true });
		g.createEntity({ noise: 0 });
		g.foreachEntity(e -> {
			fields = (macro class T {
				public static final success = $v{e.expected};
			}).fields;
			g.deleteEntity(e);
		});
		g.foreachEntity(e -> {
			fields = (macro class T {
				public static final success = $v{!e.expected};
			}).fields;
		});
		return fields;
	}
}