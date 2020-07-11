package server;

import utest.ui.Report;
import utest.Runner;
import utils.Vfs;

function main() {
	Vfs.removeDir("test/cases");
	
	var runner = new Runner();
	runner.addCases("server.issues");
	var report = Report.create(runner);
	report.displayHeader = AlwaysShowHeader;
	report.displaySuccessResults = NeverShowSuccessResults;
	runner.run();
}