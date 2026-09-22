package units.issues.issue34;

import units.issues.issue34.B;
import ecso.Entity;

function main() {
	new EntityGroup().createEntity({x: 1});
}

macro function someMacroFunction() {
	return macro null;
}
