open EcsoTypes

module GlobalizeNameFilter = struct

	(* Rename a:Int, a:String into a_int:Int, a_string:String to allow cumulation *)
	let run_field registry cf : unit =
		let type_name = s_component_type cf in
		let type_id =
			if Hashtbl.mem registry type_name then begin
				Hashtbl.find registry type_name
			end else begin
				let id = string_of_int (Hashtbl.length registry) in
				Hashtbl.add registry type_name id;
				id
			end
		in
		cf.cf_name <- cf.cf_name ^ "_" ^ type_id

	let run_archetype registry (a : archetype) : unit =
		PMap.iter (fun _ cf -> run_field registry cf) a.a_components

	let run_archetypes registry (al : archetype list) : unit =
		List.iter (run_archetype registry) al

end
