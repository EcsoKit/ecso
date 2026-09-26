package units;

import utest.ui.Report;
import utest.Runner;
import units.cases.*;

function main() {

	var classes = #if issue [
		Issues.instantiateFrom('units.issues')
	] #elseif case [
		Cases.instantiateFrom('units.cases')
	] #else [
		return // no tests atm.
	] #end;

	var runner = new Runner();

	for (c in classes) {
		runner.addCase(c);
	}

	var report = Report.create(runner);
	report.displayHeader = AlwaysShowHeader;
	report.displaySuccessResults = NeverShowSuccessResults;
	runner.run();
}
