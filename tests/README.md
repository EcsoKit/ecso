# Tests

We have a number of test suites, which are placed in their own folders in this directory.

`RunCi.hx` is the script used by our CIs to run all the test suites.

## Local testing

1. Change to this directory.
2. Run `haxe RunCi.hxml -D test=$TARGET` to select specific targets, where `$TARGET` should be a comma-separated list of targets, e.g. `js,hl`. Possible targets are interp, macro, server, cpp, cppia, hl, neko, js, lua, php, cs, java, jvm, python and flash9. However, flash9 is not likely to work on local machines.
3. Run with `-D issues`, `-D units`, or `-D specs` to select specific test suites. If nothing is defined, all suites will run. See below for details. Benchmarks with `-D bench` are not part of the default run.

Note that the script will try to look for test dependencies and install them if they are not found. Look at the `getXXXDependencies` functions for the details.

### Issue tests

1. Change to this directory.
2. Run with `-D issues` to test all issue tests.
    - select specific issues with `-D issues=$ID` to , where `$ID` should be a comma-separated list of issue ids, e.g. `19,21`.
    - ignore specific issues with `-D issues=-$ID` to , e.g. `-21` will ignore `Issue21.hx`.

### Unit tests

Unit tests include issue tests.

1. Change to this directory.
2. Run with `-D units`.

#### Cpp

Cpp unit tests are compiled with `-D HXCPP_NO_DEBUG_LINK` (removes debug symbols) to speed up compilation times. You can remove this from `units/compile-cpp.hxml` to be able to open the generated Test-debug.exe in Visual Studio and debug it. This is useful if it's difficult to figure out why a test is failing, or also which test is failing (for instance with a segmentation fault).

### Spec tests

1. Change to this directory.
2. Run with `-D specs`.

### Bench tests

Bench tests measure the runtime performance of systems (e.g. with a full entity archetype, a partial entity archetype, or optional components). They print timing results and do not assert anything, so they are opt-in and not part of the default test run.

1. Change to this directory.
2. Run with `-D bench`.
3. Add new benchmarks in `bench/bench/benchmarks/` as classes extending `bench.Bench`, which provide an entity group `setup` and a timed `update`, then register them in `bench/bench/Main.hx`.

## Server

The "server" folder contains a set of unit tests for the Haxe completion server.

1. Change to this directory.
2. Run `haxe RunCi.hxml -D test=server`.


