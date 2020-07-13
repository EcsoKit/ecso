package server.issues;

class Issue15 extends TestCase {
	function test(_) {
		vfs.putContent("Base.hx", getTemplate("issues/Issue15/Base1.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue15/Main1.hx"));
		var args = ["--main", "Main", "--hl", "test.hl", "--library", "ecso"];
		runHaxe(args);
		assertSuccess();
		vfs.putContent("Base.hx", getTemplate("issues/Issue15/Base2.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Base.hx")});
		runHaxe(args);
		assertSuccess();
		vfs.putContent("Main.hx", getTemplate("issues/Issue15/Main3.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertSuccess();
	}
}