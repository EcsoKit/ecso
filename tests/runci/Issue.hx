package runci;

final class Issue {
	final id:String;
	final project:Null<String>;

	public function new(id, ?project) {
		this.id = id;
		this.project = project;
	}

	public function extraParams():String {
		return if (project == null) {
			"";
		} else {
			final hxml = '$project/extraParams.hxml';
			sys.FileSystem.exists(hxml) ? hxml : "";
		}
	}

	public function toString():String {
		return id;
	}
}
