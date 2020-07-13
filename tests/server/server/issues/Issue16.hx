package server.issues;

class Issue16 extends TestCase {
	function test(_) {
		vfs.putContent("Base.hx", getTemplate("issues/Issue16/Base.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue16/Main1.hx"));
		var args = ["--main", "Main", "--interp", "--library", "ecso"];
		runHaxe(args);
		assertSuccess();
		vfs.putContent("Main.hx", getTemplate("issues/Issue16/Main2.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertSuccess();
	}
}