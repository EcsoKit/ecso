open EcsoTypes

module GlobalizeNameFilter = struct
		
	let run ?(registry=Hashtbl.create 0) (al : archetype list) : archetype list =
		let type_counter = ref 0 in
		let fold_renamed name cf acc =
			let type_id = s_component_type cf in
			let type_preffix =
				if Hashtbl.mem registry type_id then begin
					let type_preffix = Hashtbl.find registry type_id in
					type_preffix
				end else begin
					let type_preffix = string_of_int !type_counter in
					Hashtbl.add registry type_id type_preffix;
					type_counter := !type_counter + 1;
					type_preffix
				end
			in
			let new_name = name ^ "_" ^ type_preffix in
			PMap.add new_name { cf with cf_name = new_name; } acc
		in
		List.iter 
			(fun a ->
				a.a_components <- PMap.foldi fold_renamed a.a_components PMap.empty
			) al;
		al

end