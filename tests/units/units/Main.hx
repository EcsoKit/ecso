package units;

import utest.ui.Report;
import utest.Runner;

function main() {

	var classes = #if issue [
		Issues.instantiateFrom('units.issues')
	] #else [
		new Constraints()
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
