package units;

import utest.ui.Report;
import utest.Runner;

function main() {
	var classes = [];

	Issues.addClasses('units/issues', 'units.issues');

	var runner = new Runner();

	for (c in classes) {
		runner.addCase(c);
	}

	var report = Report.create(runner);
	report.displayHeader = AlwaysShowHeader;
	report.displaySuccessResults = NeverShowSuccessResults;
	runner.run();
}
