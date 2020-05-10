open Type
open Analyzer
open AnalyzerTypes
open AnalyzerTexpr
open EcsoTypes
open EcsoGraph
open EcsoFilters

module EcsoAnalyzer = struct

	open EcsoContext

	type analyze = {
		a_commit : EcsoGraph.gexpr->((string,string) Hashtbl.t)->unit;
		a_graph : EcsoGraph.gexpr;
	}

	type global = {
		gl_fields : (string,analyze) Hashtbl.t;
		gl_ectx : EvalContext.context;
		gl_debug_mutations : bool;
	}

	type t = {
		a_global : global;
		a_ctx : EcsoContext.t;
	}

	let from_module (gctx : global) (m : module_type) : t list =
		match m with
		| TClassDecl cl ->
			if Globals.s_type_path cl.cl_path = "ecso.EntityGroup" then
				let group = {
					eg_t = type_of_module_type m;
					eg_create = Some(PMap.find "createEntity" cl.cl_fields);
					eg_delete = Some(PMap.find "deleteEntity" cl.cl_fields);
					eg_process = Some(PMap.find "foreachEntity" cl.cl_fields);
				} in [{
					a_global = gctx;
					a_ctx = EcsoContext.create (make_context_id false cl) group;
				}]
			else
				[]
		| TEnumDecl en -> []
		| TTypeDecl td -> []
		| TAbstractDecl ab -> []

	let fetch (ctx : EvalContext.context) (ml : module_type list) : t list =
		let gctx = {
			gl_fields = Hashtbl.create 0 ~random:false;
			gl_ectx = ctx;
			gl_debug_mutations = true;
		} in
		List.flatten (List.map (from_module gctx) ml)

end

type uexpr =
	| UNone
	| UCreate of texpr
	| UDelete of texpr
	| UProcess of texpr list

let u_analyze (e : texpr) : uexpr =
	match e.eexpr with
	| TCall(fident,fargs) ->
		begin match fident.eexpr with
			| TField (entity_group, FInstance(cl, _, cf)) when (Globals.s_type_path cl.cl_path) = "ecso.EntityGroup" ->
				begin match cf.cf_name, fargs with
					| "createEntity", [a1] -> UCreate a1
					| "deleteEntity", [a1] -> UDelete a1
					| "foreachEntity", al -> UProcess al
					| _ -> UNone
				end
			| _ ->
				UNone
		end
	| TField (entity_group, FInstance(cl, _, cf)) when (Globals.s_type_path cl.cl_path) = "ecso.EntityGroup" ->
		Error.error "[ECSO] Cannot use ecso's core functions as value" e.epos
	| _ ->
		UNone

module EcsoFilterFields = struct

	let commit (actx : EcsoAnalyzer.t) : unit =
		let debug_buffer = Hashtbl.create 0 in

		DynArray.iter (fun id ->
			let a = Hashtbl.find actx.a_global.gl_fields id in
			a.a_commit a.a_graph debug_buffer
		) actx.a_ctx.ctx_field_ids;
		
		if actx.a_ctx.ctx_debug_gen >= EcsoContext.simple_debugging then begin
			print_endline "{ECSO} | Generation Repport";
			let key_cacke = ref "" in
			Seq.iter
				(fun k ->
					if k = !key_cacke then ()
					else begin
						print_list_br ("              | " ^ k ^ " ") (fun v -> v) (Hashtbl.find_all debug_buffer k) ~cache:true;
						key_cacke := k;
					end
				)
				(Hashtbl.to_seq_keys debug_buffer)
		end
	
	let registered (actx : EcsoAnalyzer.t) (id : string) : bool =
		Hashtbl.mem actx.a_global.gl_fields id
	
	let make_field_id cl_s cf =
		cl_s ^ "." ^ cf.cf_name

	let rec register (actx : EcsoAnalyzer.t) (id : string) (e : texpr) (commit : EcsoGraph.gexpr->((string,string) Hashtbl.t)->unit) : unit =
		DynArray.add actx.a_ctx.ctx_field_ids id;
		if not (registered actx id) then begin
			let g = EcsoGraph.run (actx.a_global.gl_ectx.curapi.get_com()) e in
			Hashtbl.add actx.a_global.gl_fields id {
				a_graph = g.gr_expr;
				a_commit = commit;
			};
			let register_extra (cl,cf,g) =
				let s_cl = Globals.s_type_path cl.cl_path in
				let id = make_field_id s_cl cf in
				DynArray.add actx.a_ctx.ctx_field_ids id;
				Hashtbl.add actx.a_global.gl_fields id {
					a_graph = g;
					a_commit = (fun e d -> cf.cf_expr <- Some(EcsoGraph.restore actx.a_global.gl_ectx actx.a_ctx d e));
				}
			in
			DynArray.iter register_extra g.gr_extra
		end

	let register_field (actx : EcsoAnalyzer.t) (cl_s : string) (cf : tclass_field) (e : texpr) _ : unit =
		let id = make_field_id cl_s cf in
		register actx id e (fun e d -> cf.cf_expr <- Some(EcsoGraph.restore actx.a_global.gl_ectx actx.a_ctx d e))

	let register_init (actx : EcsoAnalyzer.t) (cl_s : string) (cl : tclass) (e : texpr) _ : unit =
		let id = cl_s in
		register actx id e (fun e d -> cl.cl_init <- Some(EcsoGraph.restore actx.a_global.gl_ectx actx.a_ctx d e))

	let rec run_expr (actx : EcsoAnalyzer.t) (register : unit->unit) (e : texpr) : unit =
		match u_analyze e with
		| UNone -> iter (run_expr actx register) e
		| _ -> register()

	let run_field (actx : EcsoAnalyzer.t) (cl : string) (cf : tclass_field) : unit =
		match cf.cf_expr with
		| Some e -> run_expr actx (register_field actx cl cf e) e
		| None -> ()

	let run_class (actx : EcsoAnalyzer.t) (cl : tclass) : unit =
		let s_cl = Globals.s_type_path cl.cl_path in
		List.iter (run_field actx s_cl) cl.cl_ordered_statics;
		List.iter (run_field actx s_cl) cl.cl_ordered_fields;
		List.iter (run_field actx s_cl) cl.cl_overrides;
		match cl.cl_constructor with
			| Some cf -> run_field actx s_cl cf
			| None -> ();
		match cl.cl_init with
			| Some e -> run_expr actx (register_init actx s_cl cl e) e
			| None -> ()

	let run_type (actx : EcsoAnalyzer.t) (td : tdef) : unit =
		()

	let run_abstract (actx : EcsoAnalyzer.t) (ab : tabstract) : unit =
		()

	let run_module (actx : EcsoAnalyzer.t) (m : module_type) : unit =
		match m with
		| TClassDecl cl -> run_class actx cl
		| TAbstractDecl ab -> run_abstract actx ab
		| TTypeDecl td -> run_type actx td
		| TEnumDecl en -> ()

	let run (actx : EcsoAnalyzer.t) (ml : module_type list) : unit =
		List.iter (run_module actx) ml

end

(*
	FIXME: move filters into ecsoFilters.ml
*)
module CheckComponentGlobalization = struct

	open EcsoAnalyzer
	open EcsoGraph
	open TPrinting

	let run (actx : EcsoAnalyzer.t) : unit =
		let cache = Hashtbl.create 1 in
		let check_component strict _ cf =
			if Hashtbl.mem cache cf.cf_name then begin
				let cf' = Hashtbl.find cache cf.cf_name in
				if not (does_unify_component strict cf' cf) then begin
					Error.error (
						"[ECSO] Cannot redefine " ^ cf.cf_name ^ "'s type "
						^ "\n         have: " ^ Printer.s_type cf.cf_type
						^ "\n         want: " ^ Printer.s_type cf'.cf_type
						^ "\n         declared at: " ^ Printer.s_pos cf'.cf_name_pos
						) cf.cf_name_pos;
				end
			end else
				Hashtbl.add cache cf.cf_name cf
		in
		let check_archetype strict a =
			PMap.iter (check_component strict) a.a_components
		in
		let check_system s = 
			List.iter (fun r ->
				match r with
				| SREntity (v,a) -> check_archetype false a
			) s.s_requirements
		in
		let forbit_commutative_downcast name t p t' p' =
			begin match fetch_type t, fetch_type t' with
				| TInst (cl,_),TInst (cl',_) ->
					let downcast_error name decl_pos p =
						Error.error (
							"[ECSO] Cannot downcast component " ^ name
							^ "\n         declared at: " ^ Printer.s_pos decl_pos
						) p
					in
					if cl == cl' then
						()
					else if is_parent cl cl' then
						downcast_error name p p'
					else if is_parent cl' cl then
						downcast_error  name p' p
				| _ -> ()
			end
		in
		let forbit_oneway_upcast name t p t' p' =
			begin match fetch_type t, fetch_type t' with
				| TInst (cl,_),TInst (cl',_) ->
					if cl == cl' then
						()
					else if is_parent cl' cl then
						Error.error (
							"[ECSO] Cannot upcast component " ^ name ^ " declared as " ^ Globals.s_type_path cl.cl_path
							^ "\n         declared at: " ^ Printer.s_pos p
						) p'
				| _ -> ()
			end
		in
		let rec check_ecreate (g : gexpr) =
			EcsoGraph.iter check_ecreate g;
			match g.gexpr with
			| GEcsoCreate (_,a,_,ctx_id) when ctx_id = actx.a_ctx.ctx_id ->

				(* Check name globalization *)
				if actx.a_ctx.ctx_identity_mode = IGlobal then
					check_archetype true a;

				(* Forbit downcasting components *)
				PMap.iter (fun name cf ->
					try
						let cf' = Hashtbl.find cache cf.cf_name in
						forbit_commutative_downcast
							cf.cf_name
							cf.cf_type
							cf.cf_pos
							cf'.cf_type
							cf'.cf_pos
					with
					| Not_found -> ()
				) a.a_components
			| _ -> ()
		in
		let rec check_emutations (g : gexpr) =
			EcsoGraph.iter check_emutations g;
			match g.gexpr with
			| GEcsoMutation (v,fa,op,value,ctx_id) when ctx_id = actx.a_ctx.ctx_id ->
				let component_name =
					match fa with
					| FAnon (cf)
					| FClosure (_,cf)
					| FInstance (_,_,cf)
					| FStatic (_,cf) -> cf.cf_name
					| FDynamic n -> n
					| FEnum _ -> Error.error "{ECSO} unsupported entity kind - please report this" g.greal.epos
				in
				(* Forbid downcasting mutations *)
				begin
					try
						let cf' = Hashtbl.find cache component_name in
						forbit_oneway_upcast
							component_name
							cf'.cf_type
							cf'.cf_pos
							value.greal.etype
							g.greal.epos
					with
					| Not_found -> ()
				end
			| _ -> ()
		in
		let rec check_esystem (g : gexpr) =
			EcsoGraph.iter check_esystem g;
			(* Check name globalization *)
			if actx.a_ctx.ctx_identity_mode = IGlobal then
				match g.gexpr with
				| GEcsoSystem (s,ctx_id) when ctx_id = actx.a_ctx.ctx_id ->
					check_system s
				| _ -> ()
		in
		DynArray.iter (fun id ->
			check_ecreate (Hashtbl.find actx.a_global.gl_fields id).a_graph;
			check_emutations (Hashtbl.find actx.a_global.gl_fields id).a_graph;
			check_esystem (Hashtbl.find actx.a_global.gl_fields id).a_graph
		) actx.a_ctx.ctx_field_ids;

end

(*
	Resolves the possible entity evolution which can occur at runtime.
*)
module EcsoArchetypeAnalyzer = struct

	open EcsoGraph
	open Ast

	let run_expr (actx : EcsoAnalyzer.t) (g : gexpr) (archetypes : (archetype list) ref) (mutations : (mutation list) ref) =
		let rec loop g =
			iter loop g;
			match g.gexpr with
			| GEcsoCreate (group,archetype,obj,ctx_id) when ctx_id = actx.a_ctx.ctx_id ->
				begin match List.find_opt (eq_archetype archetype) !archetypes with
				| None -> archetypes := archetype :: !archetypes
				| Some a' ->
					(* Rebind the archetype reference to its listed one, this is required by MCumulated *)
					g.gexpr <- GEcsoCreate (group,a',obj,ctx_id)
				end
			| GEcsoSystem (s,ctx_id) when ctx_id = actx.a_ctx.ctx_id && actx.a_ctx.ctx_mutation_accuracy = MPresumed ->
				
				let presume_mutations rl : mutation list =
					let rec loop rl mutl = match rl with
					| [] -> mutl
					| r :: rl ->
						match r with
						| SREntity (v,arch) ->
							let associate_mutations base nullables mutl : mutation list =
								let rec loop mutl nl = match nl with
								| [] -> mutl
								| n :: nl ->
									let n = { n with cf_type = unwrap_explicit_null n.cf_type } in
									loop (MutAdd (base,n) :: MutRem (n :: base,0) :: mutl) nl
								in loop mutl nullables
							in
							let base,nullables = ref[],ref[] in
							PMap.iter
								(fun name cf ->
									if is_explicit_null cf.cf_type then
										nullables := cf :: !nullables
									else
										base := cf :: !base
								)
								arch.a_components;
							loop rl (associate_mutations !base !nullables mutl)
					in loop rl []
				in
				mutations := !mutations @ presume_mutations s.s_requirements
			| _ ->
				()
		in
		loop g
	
	let apply_mutations (actx : EcsoAnalyzer.t) user_archetypes mutations : archetype list =
		let rec apply_mutations a mutl al cache =
			let match_mutation_base base =
				List.for_all (fun cf -> PMap.mem cf.cf_name a.a_components && eq_component_type (PMap.find cf.cf_name a.a_components) cf) base
			in
			let already_muted cf =
				PMap.mem cf.cf_name a.a_components
			in
			let cons_if_uniq a al =
				if List.exists (eq_archetype a) cache then al
				else a :: al
			in
			match mutl with
			| [] -> al
			| mut :: mutl -> begin match mut with
				| MutAdd(base,cf) ->
					if not (already_muted cf) && match_mutation_base base then
						apply_mutations a mutl (cons_if_uniq { a with a_components = PMap.add cf.cf_name cf a.a_components } al) cache
					else
						al
				| MutRem(base,i) ->
					if match_mutation_base base then
						apply_mutations a mutl (cons_if_uniq { a with a_components = PMap.remove (List.nth base i).cf_name a.a_components } al) cache
					else
						al
				end
		in
		let rec loop al mutl al2 cache = 
			match al with
			| [] -> al2
			| a :: al ->
				let al2 = apply_mutations a mutl al2 cache in
				loop al mutl al2 (cache@al2) (* HERE something is wrong *)
		in
		if actx.a_global.gl_debug_mutations then begin
			print_endline "{ECSO} | Mutation Repport";
			print_endline "       | Registered mutations:";
			print_list_br "              | " s_mutation mutations ~cache:true;
			print_endline "       | Initial archetypes:";
			print_list_br "              | " s_archetype user_archetypes ~cache:true;
		end;
		let rec pass archetypes cache =
			match loop archetypes mutations [] cache with
			| [] ->
				if actx.a_global.gl_debug_mutations then begin
					print_endline "       | Mutated archetypes: None";
				end;
				archetypes
			| mutateds ->
				if actx.a_global.gl_debug_mutations then begin
					print_endline "       | Mutated archetypes:";
					print_list_br "              | " s_archetype mutateds ~cache:true;
				end;
				archetypes @ pass mutateds (archetypes@cache)
		in
		let archetypes = pass user_archetypes [] in
		if actx.a_global.gl_debug_mutations then begin
			print_endline "       | Final archetypes:";
			print_list_br "              | " s_archetype archetypes ~cache:true;
		end;
		let archetypes = match actx.a_ctx.ctx_storage_mode with
			| AoS (_,MCumulated,_) ->
				let archetypes =
					if actx.a_ctx.ctx_identity_mode = IGlobal then
						archetypes
					else
						(* Rename a:Int, a:String into a_int:Int, a_string:String to be able to cumulate them *)
						GlobalizeNameFilter.run archetypes ~registry:actx.a_ctx.ctx_renaming_registry in
				let api = (actx.a_global.gl_ectx.curapi.get_com()).basic in
				let rec cumulate al = match al with
					| [] -> al
					| a :: al -> 
						let prune_a = ref false in
						let al = List.map
							(fun a' ->
								(* It is assumed that each component has an unique name *)
								let cumulate_components a1 a2 =
									let does_share_components a1 a2 =
										let partial_share = ref false in
										PMap.iter (fun name cf ->
												if (try PMap.find name a2.a_components; true with | Not_found -> false) then
													partial_share := true
											)
											a1.a_components;
										PMap.iter (fun name cf ->
												if (try PMap.find name a1.a_components; true with | Not_found -> false) then
													partial_share := true
											)
											a2.a_components;
										!partial_share
									in
									let fill_one_to_another (a : archetype) (a' : archetype) =
										PMap.iter
											(fun name cf ->
												if (try PMap.find name a'.a_components; true with | Not_found -> false) then
													(* Propagate nullability *)
													if is_explicit_null cf.cf_type then
														a'.a_components <- PMap.add name cf a'.a_components
													else
														()
												else
													a'.a_components <- PMap.add name { cf with cf_type = api.tnull cf.cf_type } a'.a_components;
												()
											)
											a.a_components
									in
									if does_share_components a1 a2 then begin
										fill_one_to_another a1 a2;
										fill_one_to_another a2 a1;
										a2.a_components <- a1.a_components;
										true,a1
									end else
										false,a2
								in
								let ps,ac = cumulate_components a a' in
								if ps then begin
									prune_a := true;
									ac
								end else a'
							) al
						in
						if !prune_a then
							cumulate al
						else
							a :: cumulate al
				in
				let cumulated_archetypes = cumulate archetypes in
				if actx.a_global.gl_debug_mutations then begin
					print_endline "       | Cumulated archetypes:";
					print_list_br "              | " s_archetype cumulated_archetypes ~cache:true;
				end;
				cumulated_archetypes
		in
		archetypes

	let run (actx : EcsoAnalyzer.t) : unit =

		if actx.a_ctx.ctx_identity_mode = IGlobal then
			CheckComponentGlobalization.run actx;

		let archetypes,mutations = ref[],ref[] in
		DynArray.iter (fun id ->
			run_expr actx (Hashtbl.find actx.a_global.gl_fields id).a_graph archetypes mutations
		) actx.a_ctx.ctx_field_ids;

		actx.a_ctx.ctx_archetypes <- apply_mutations actx !archetypes !mutations;

end