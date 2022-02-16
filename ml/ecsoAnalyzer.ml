open Type
open Analyzer
open AnalyzerTypes
open AnalyzerTexpr
open EcsoTypes
open EcsoGraph
open EcsoFilters

module EcsoAnalyzer = struct

	open Ast
	open Error
	open EcsoContext

	type analyze = {
		a_commit : EcsoGraph.gexpr->((string,string) Hashtbl.t)->unit;
		a_graph : EcsoGraph.gexpr;
	}

	type global = {
		gl_macro : bool;
		gl_fields : (string,analyze) Hashtbl.t;
		gl_ectx : EvalContext.context;
		gl_debug_mutations : bool;
	}

	type t = {
		a_global : global;
		a_ctx : EcsoContext.t;
	}

	let from_module (gctx : global) (basic : basic_types) (m : module_type) : t list =
		let find_fields_with meta fl =
			List.filter (fun cf -> Meta.has meta cf.cf_meta) fl
		in
		let make_group cl is_static fl =
			let api = basic in
			let tany = { null_abstract with a_path = ([],"Any") } in
			let trest = { null_abstract with a_path = (["haxe"],"Rest") } in
			let tfunc = { null_abstract with a_path = ([],"Any") } in
			let tparam = { null_class with cl_kind = KTypeParameter [] } in

			let creates = find_fields_with EcsoMeta.api_create fl in
			let deletes = find_fields_with EcsoMeta.api_delete fl in
			let foreachs = find_fields_with EcsoMeta.api_foreach fl in

			let hint_or_raise name t want p =
				match t with
				| TMono _ ->
					Error.error ("[ECSO] " ^ name ^ " functions must be explicitely typed " ^ TPrinting.Printer.s_type want) p
				| _ -> ()
			in
			let unify_or_raise a b p =
				if type_iseq_strict a b then ()
				else raise (Error (Unify [cannot_unify a b], p)) 
			in
			let single_param_or_raise name cf = match cf.cf_params with
				| [n,t] -> n,t
				| _ -> Error.error ("[ECSO] " ^ name ^ " functions must declare exactly one parameter") cf.cf_pos;
			in
			let common_sanity name cf =
				match cf.cf_type with
				| _ when not (has_class_field_flag cf CfExtern) ->
					Error.error ("[ECSO] " ^ name ^ " functions must be extern") cf.cf_pos
				| TFun (_,r) when not (ExtType.is_mono r) && not (ExtType.is_void r) ->
					raise (Error (Unify [cannot_unify r api.tvoid], cf.cf_pos))
				| TFun ([_,true,_],_) ->
					Error.error ("[ECSO] " ^ name ^ " functions cannot have an optional argument") cf.cf_pos
				| TFun ([a1],_) ->
					{ cf with cf_type = TFun([a1],api.tvoid) }
				| TFun (_,_) ->
					Error.error ("[ECSO] " ^ name ^ " functions must accept exactly one argument") cf.cf_pos
				| _ ->
					Error.error ("[ECSO] " ^ name ^ " fields must be function") cf.cf_pos
			in
			let sanitize_ec_ed name cf =
				let cf = common_sanity name cf in
				match cf.cf_type with
				| TFun ([n,opt,t],_) ->
					let pname,_ = single_param_or_raise name cf in
					let tparam = match t with
						| TInst({ cl_kind = KTypeParameter _ }, params) ->
							t
						| _ ->
							TInst({ tparam with cl_kind = KTypeParameter[]; cl_private = cl.cl_private; cl_path = ([],pname) }, [])
					in
					let have = TFun([n,opt,t],api.tvoid) in
					let want = TFun([n,false,tparam], api.tvoid) in
					hint_or_raise name t want cf.cf_pos;
					unify_or_raise have want cf.cf_pos;
					cf
				| _ -> assert false
			in
			let sanitize_ef name cf =
				let cf = common_sanity name cf in
				match cf.cf_type with
				| TFun ([n,opt,t],_) ->
					let rest_of_any = match t with
						| TAbstract({a_path=["haxe"],"Rest"}, [TAbstract ({ a_path = [],"Any" },[])])
						| TType({t_path=["haxe"; "extern"],"Rest"}, [TAbstract ({ a_path = [],"Any" },[])]) -> t
						| _ -> TAbstract(trest, [TAbstract (tfunc,[])])
					in
					let have = TFun([n,opt,t],api.tvoid) in
					let want = TFun([n,false,rest_of_any], api.tvoid) in
					hint_or_raise name t want cf.cf_pos;
					unify_or_raise have want cf.cf_pos;
					cf
				| _ -> assert false
			in
			let ctx_id = make_context_id is_static cl gctx.gl_macro in
			let get_single sanity_check l name =
				if List.length l = 0 then
					None
				else if List.length l = 1 then
					let cf = List.nth l 0 in
					if in_context_raw cf ctx_id then begin
						(* Already processed *)
						Some cf
					end else begin
						cf.cf_meta <- (EcsoMeta.context,[EConst (Int (string_of_int ctx_id)),cf.cf_name_pos],cf.cf_name_pos) :: cf.cf_meta;
						Some (sanity_check name cf)
					end
				else
					Error.error ("[ECSO] Redefined " ^ name ^ " with field " ^ (List.nth l 0).cf_name) (List.nth l 1).cf_name_pos
			in
			let get_meta_name m = match m with | Meta.Custom v -> "@" ^ v | _ -> assert false in
			{
				eg_t = m;
				eg_static = is_static;
				eg_create = get_single sanitize_ec_ed creates (get_meta_name EcsoMeta.api_create);
				eg_delete = get_single sanitize_ec_ed deletes (get_meta_name EcsoMeta.api_delete);
				eg_foreach = get_single sanitize_ef foreachs (get_meta_name EcsoMeta.api_foreach);
			},ctx_id
		in
		let does_define_group cf =
			let rec loop ml = match ml with
				| [] -> false
				| (m,args,p) :: _ when m = EcsoMeta.api_create -> true
				| (m,args,p) :: _ when m = EcsoMeta.api_delete -> true
				| (m,args,p) :: _ when m = EcsoMeta.api_foreach -> true
				| _ :: ml -> loop ml
			in
			loop cf.cf_meta
		in
		let rec retrive_groups cl is_static fl gl =
			match fl with
			| [] -> gl
			| f :: fl' ->
				if does_define_group f then
					(make_group cl is_static fl :: gl)
				else
					retrive_groups cl is_static fl' gl
		in
		match m with
		| TClassDecl cl ->
			let gl = retrive_groups cl false cl.cl_ordered_fields [] in
			let gl = retrive_groups cl true cl.cl_ordered_statics gl in
			List.map (fun (g,id) -> {
					(* a_global = gctx; *)
					a_global = { gctx with gl_fields = Hashtbl.create 0 ~random:false };
					a_ctx = EcsoContext.create id g basic;
				}) gl
		| TEnumDecl en -> []
		| TTypeDecl td -> []
		| TAbstractDecl ab -> []

	let fetch (ctx : EvalContext.context) (macro : bool) (ml : module_type list) : t list =
		let com = ctx.curapi.get_com() in
		let basic = if macro then begin match com.get_macros() with
			| Some mctx ->
				mctx.basic
			| None -> 
				com.basic
			end else com.basic
		in
		let gctx = {
			gl_macro = macro;
			gl_fields = Hashtbl.create 0 ~random:false;
			gl_ectx = ctx;
			gl_debug_mutations = false;
		} in
		List.flatten (List.map (from_module gctx basic) ml)

end

type uexpr =
	| UNone
	| UCreate
	| UDelete
	| UForeach
	
let u_analyze (ctx : EcsoContext.t) (e : texpr) : uexpr =
	match e.eexpr with
	| TCall(fident,fargs) ->
		begin match fident.eexpr with
			| TField (g, (FInstance(_,_,cf) | FStatic(_,cf) | FClosure(_,cf))) when EcsoContext.in_context cf ctx ->
				let rec find_api ml al = match ml with
					| [] -> UNone
					| m :: ml ->
						match m with
						| (m,_,p) when m = EcsoMeta.api_create -> UCreate
						| (m,_,p) when m = EcsoMeta.api_delete -> UDelete
						| (m,_,p) when m = EcsoMeta.api_foreach -> UForeach
						| _ -> find_api ml al
				in
				find_api cf.cf_meta fargs
			| _ ->
				UNone
		end
	| TField (entity_group, (FInstance(_,_,cf) | FStatic(_,cf) | FClosure(_,cf))) when EcsoContext.in_context cf ctx ->
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
			Hashtbl.iter
				(fun k _ ->
					if k = !key_cacke then ()
					else begin
						print_list_br ("              | " ^ k ^ " ") (fun v -> v) (Hashtbl.find_all debug_buffer k) ~cache:true;
						key_cacke := k;
					end
				)
				(debug_buffer)
		end
	
	let registered (actx : EcsoAnalyzer.t) (id : string) : bool =
		Hashtbl.mem actx.a_global.gl_fields id
	
	let make_field_id cl_s cf =
		cl_s ^ "." ^ cf.cf_name

	let restore_graph (actx : EcsoAnalyzer.t) (e : EcsoGraph.gexpr) (d : (string,string) Hashtbl.t) = 
		Some (EcsoGraph.restore actx.a_global.gl_ectx actx.a_ctx d e)

	let register (actx : EcsoAnalyzer.t) (id : string) (e : texpr) (commit : EcsoGraph.gexpr->((string,string) Hashtbl.t)->unit) : unit =
		DynArray.add actx.a_ctx.ctx_field_ids id;
		if not (registered actx id) then begin
			let g = EcsoGraph.run actx.a_ctx (actx.a_global.gl_ectx.curapi.get_com()) e in
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
					a_commit = (fun e d -> cf.cf_expr <- restore_graph actx e d);
				}
			in
			DynArray.iter register_extra g.gr_extra
		end

	let register_field (actx : EcsoAnalyzer.t) (cl_s : string) (cf : tclass_field) (e : texpr) _ : unit =
		let id = make_field_id cl_s cf in
		register actx id e (fun e d -> cf.cf_expr <- restore_graph actx e d)

	let register_init (actx : EcsoAnalyzer.t) (cl_s : string) (cl : tclass) (e : texpr) _ : unit =
		let id = cl_s in
		register actx id e (fun e d -> cl.cl_init <- restore_graph actx e d)

	let rec run_expr (actx : EcsoAnalyzer.t) (register : unit->unit) (e : texpr) : unit =
		match u_analyze actx.a_ctx e with
		| UNone -> iter (run_expr actx register) e
		| _ -> register()

	let run_field (actx : EcsoAnalyzer.t) (cl : string) (cf : tclass_field) : unit =
		match cf.cf_expr with
		| Some e -> run_expr actx (register_field actx cl cf e) e
		| None -> ()

	let add_dependencies (actx : EcsoAnalyzer.t) md =
		let g = match actx.a_ctx.ctx_group.eg_t with
			| TClassDecl cl -> cl.cl_module
			| TEnumDecl en -> en.e_module
			| TTypeDecl td -> td.t_module
			| TAbstractDecl ab -> ab.a_module
		in
		add_dependency g md;
		add_dependency md g

	let run_class (actx : EcsoAnalyzer.t) (cl : tclass) : unit =
		add_dependencies actx cl.cl_module;
		let s_cl = Globals.s_type_path cl.cl_path in
		List.iter (run_field actx s_cl) cl.cl_ordered_statics;
		List.iter (run_field actx s_cl) cl.cl_ordered_fields;
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
						^ "\n         declared at: " ^ s_error_pos cf'.cf_pos
						) cf.cf_pos;
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
							^ "\n         declared at: " ^ s_error_pos decl_pos
						) p
					in
					if cl == cl' then
						()
					else if extends cl' cl then
						downcast_error name p p'
					else if extends cl cl' then
						downcast_error  name p' p
				| _ -> ()
			end
		in
		let forbit_oneway_upcast name t p t' p' =
			begin match fetch_type t, fetch_type t' with
				| TInst (cl,_),TInst (cl',_) ->
					if cl == cl' then
						()
					else if extends cl cl' then
						Error.error (
							"[ECSO] Cannot upcast component " ^ name ^ " declared as " ^ Globals.s_type_path cl.cl_path
							^ "\n         declared at: " ^ s_error_pos p
						) p'
				| _ -> ()
			end
		in
		let rec check_ecreate (g : gexpr) =
			EcsoGraph.iter check_ecreate g;
			match g.gexpr with
			| GEcsoCreate (_,a,ctx_id) when ctx_id = actx.a_ctx.ctx_id ->

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
					| FEnum _ -> Error.error "{ECSO} unsupported entity kind - please report this at https://github.com/EcsoKit/ecso/issues" g.greal.epos
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

	let run_expr (actx : EcsoAnalyzer.t) (g : gexpr) (archetypes : (archetype list) ref) (mutations : mutation DynArray.t) =
		let rec loop g =
			iter loop g;
			match g.gexpr with
			| GEcsoCreate (_,a,ctx_id) when ctx_id = actx.a_ctx.ctx_id ->
				if not (List.exists (eq_archetype a) !archetypes) then
					archetypes := a :: !archetypes
			| GEcsoSystem (s,ctx_id) when ctx_id = actx.a_ctx.ctx_id && actx.a_ctx.ctx_mutation_accuracy = MPresumed ->
				
				let presume_mutations rl : unit =
					let rec loop rl = match rl with
					| [] -> ()
					| r :: rl ->
						match r with
						| SREntity (v,arch) ->
							let associate_mutations base nullables : unit =
								let rec loop nl = match nl with
								| [] -> ()
								| n :: nl ->
									let n = { n with cf_type = unwrap_explicit_null n.cf_type } in
									DynArray.add mutations (MutAdd (base,n));
									DynArray.add mutations (MutRem (n :: base,0));
									loop nl
								in loop nullables
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
							associate_mutations !base !nullables;
							loop rl;
					in loop rl
				in
				presume_mutations s.s_requirements
			| _ ->
				()
		in
		loop g
	
	let apply_mutations (actx : EcsoAnalyzer.t) user_archetypes mutations : archetype list =
		let api = actx.a_ctx.ctx_basic in
		let match_mutation_base a base =
			List.length base = 0 ||
			List.for_all (fun cf -> PMap.mem cf.cf_name a.a_components) base
		in
		let has_component a cf = PMap.mem cf.cf_name a.a_components in

		let list_filter_map f l =
			let rec loop l acc = match l with
				| [] -> acc
				| x :: l -> match f x with
					| Some x -> loop l (x :: acc)
					| None -> loop l acc
			in loop l []
		in
		(* convert mutation format *)
		let convert_mutl mutl = 
			let get_base mut = match mut with | MutAdd(base,_) | MutRem(base,_) -> base in
			let same_base mut mut' = match get_base mut, get_base mut' with
				| base,base' when (List.length base = List.length base') ->
					List.for_all (fun cf' -> List.exists (fun cf -> cf.cf_name = cf'.cf_name) base) base'
				| _,_ -> false
			in
			(* group mutations with identical bases *)
			let grouped_mutl : (mutation list) list = 
				let rec loop mutl groups = match mutl with
					| [] -> groups
					| mut :: mutl ->
						let group = ref [mut] in
						let mutl = list_filter_map (fun mut' ->
							if same_base mut mut' then begin
								group := mut' :: !group;
								None
							end else
								Some mut'
						) mutl in
						loop mutl (!group :: groups)
				in
				loop mutl []
			in
			(* ensure we don't get grandparents for parents by sorting first *)
			let grouped_mutl = List.sort (fun a b -> 
				let a = List.hd a in
				let b = List.hd b in
				if List.length (get_base a) < List.length (get_base b) then 1
				else if List.length (get_base a) > List.length (get_base b) then -1
				else 0
			) grouped_mutl in

			let rec loop grouped_mutl rml =
				match grouped_mutl with
				| [] -> rml
				| grouped_mut :: grouped_mutl ->
					let base = get_base (List.hd grouped_mut) in
					let evolutions = List.map (fun mut -> (match mut with | MutAdd(_,cf) -> MutValueAdd(cf) | MutRem(_,i) -> MutValueRem(i))) grouped_mut in
					let node = {
						rm_base = base;
						rm_evolutions = evolutions;
					} in
					loop grouped_mutl (node :: rml)
			in
			loop grouped_mutl []
		in 
		let pass al al2 =
			let additions = List.filter (fun mut -> match mut with | MutAdd _ -> true | _ -> false) mutations in
			let removals = List.filter (fun mut -> match mut with | MutRem _ -> true | _ -> false) mutations in
			let mutated_arr = ref PMap.empty in
			let pass_mutations al mutl =
				let mutl = convert_mutl mutl in
				let mutate a mut : (archetype list) option = with_timer ["archetypes";"mutation";"mutate"] (fun () ->
					if match_mutation_base a mut.rm_base then begin
						let al = 
							list_filter_map (fun evolution -> match evolution with
								| MutValueAdd(cf) ->
									if not (has_component a cf) then
										Some { a with a_components = PMap.add cf.cf_name cf a.a_components }
									else
										None
								| MutValueRem(i) ->
									Some { a with a_components = PMap.remove (List.nth mut.rm_base i).cf_name a.a_components }
							) mut.rm_evolutions
						in
						Some al
					end else
						None
				) in
				match actx.a_ctx.ctx_storage_mode with
				| AoS (_,MCumulated,_) ->
					assert (actx.a_ctx.ctx_identity_mode = IGlobal); (* TODO: mutations are not yet prefiltered *)
					(* bypass the mutation process by relying on the cumulation phase *)
					List.iter (fun a -> List.iter (fun mut ->
						let rec loop a mut =
							if match_mutation_base a mut.rm_base then begin
								let fl =
									List.fold_left (fun acc evolution -> match evolution with
										| MutValueAdd(cf) -> PMap.add cf.cf_name cf acc
										| MutValueRem(i) -> acc (* rely on the cumulation phase *)
									) a.a_components mut.rm_evolutions
								in
								let a' = mk_archetype fl in
								let hash = hash_archetype a' in
								if not (PMap.mem hash !mutated_arr) then begin
									mutated_arr := PMap.add hash a' !mutated_arr;
									List.iter (loop a') mutl
								end
							end
						in loop a mut
					) mutl ) al
				| _ ->
					(* slow route *)
					List.iter (fun a -> List.iter (fun mut ->
						let rec transverse_relatives a mut =
							match (mutate a mut) with
							| Some [] -> ()
							| Some al' ->
								let al' =  with_timer ["archetypes";"mutation";"filter"] (fun () -> 
									List.filter (fun a' ->
										let hash = hash_archetype a' in
										if not (PMap.mem hash !mutated_arr) then begin
											mutated_arr := PMap.add hash a' !mutated_arr;
											true
										end else
											false
									) al'
								) in
								List.iter (fun a' ->
									List.iter (transverse_relatives a') mutl
								) al'
							| None -> ()
						in
						transverse_relatives a mut
					) mutl ) al
			in
			with_timer ["archetypes";"mutation";"additions"] (fun () -> pass_mutations al additions );
			with_timer ["archetypes";"mutation";"removals"] (fun () -> pass_mutations al removals );
			al @ PMap.fold (fun a al -> a :: al) !mutated_arr []
		in
		if actx.a_global.gl_debug_mutations then begin
			print_endline "{ECSO} | Mutation Repport";
			print_endline "       | Registered mutations:";
			print_list_br "              | " s_mutation mutations ~cache:true;
			print_endline "       | Initial archetypes:";
			print_list_br "              | " s_archetype user_archetypes ~cache:true;
		end;
		let archetypes = with_timer ["archetypes";"mutation"] (fun () -> pass user_archetypes [] ) in
		if actx.a_global.gl_debug_mutations then begin
			print_endline "       | Final archetypes:";
			print_list_br "              | " s_archetype archetypes ~cache:true;
		end;
		let archetypes = match actx.a_ctx.ctx_storage_mode with
			| AoS (_,MCumulated,_) -> with_timer ["archetypes";"cumulate"] (fun () -> 
				if not (actx.a_ctx.ctx_identity_mode = IGlobal) then
					with_timer ["filter";"names"] (fun () -> 
						GlobalizeNameFilter.run_archetypes actx.a_ctx.ctx_renaming_registry archetypes
					);
				
				let is_from_mutation cf =
					List.exists (fun mut ->
						(* It is assumed that each component has an unique name *)
						match mut with
						| MutAdd (base,cf') ->
							cf.cf_name = cf'.cf_name
						| MutRem (base,i) ->
							let cf' = List.nth base i in
							cf.cf_name = cf'.cf_name
					) mutations
				in
				
				let edit_archetype (a : archetype) (with_components : (string, tclass_field) PMap.t) : unit =
					PMap.iter (fun name cf ->
						if is_explicit_null cf.cf_type then
							(* Propagate nullability *)
							a.a_components <- PMap.add name cf a.a_components
						else if not (PMap.mem name a.a_components) || is_from_mutation cf then
							(* Apply nullability for mutated or missing field *)
							a.a_components <- PMap.add name { cf with cf_type = api.tnull cf.cf_type } a.a_components
					) with_components
				in

				let cumulated_archetypes = 
					let chained_components = ChainTbl.init_map archetypes (fun a -> a.a_components) in
					ref (ChainTbl.unload_map mk_archetype chained_components)
				in

				(* Re-link with user's archetypes *)
				let linked_archetypes = 
					let missings = ref [] in
					List.iter (fun ua ->
						let present = ref false in
						cumulated_archetypes := List.map (fun ca ->
							if unify_archetype ca ua then begin
								edit_archetype ua ca.a_components;
								present := true;
								ua
							end else ca
						) !cumulated_archetypes;
						if not !present then
							missings := ua :: !missings
					) user_archetypes;
					(!missings)@(!cumulated_archetypes)
				in
				
				if actx.a_global.gl_debug_mutations then begin
					print_endline "       | Cumulated archetypes:";
					print_list_br "              | " s_archetype linked_archetypes ~cache:true;
				end;
				linked_archetypes
			)
		in
		archetypes

	let run (actx : EcsoAnalyzer.t) : unit =

		if actx.a_ctx.ctx_identity_mode = IGlobal then
			with_timer ["component-globalization"] (fun () -> 
				CheckComponentGlobalization.run actx
			);

		let archetypes = ref[] in
		let unique_mutations = DynArray.create() in

		with_timer ["archetypes";"aggregate"] (fun () -> 
			let mutations = DynArray.create() in
			DynArray.iter (fun id ->
				run_expr actx (Hashtbl.find actx.a_global.gl_fields id).a_graph archetypes mutations
			) actx.a_ctx.ctx_field_ids;
			dynarray_filter_dupplicates_into unique_mutations mutations eq_mutation
		);
		
		actx.a_ctx.ctx_archetypes <- with_timer ["archetypes"] (fun () -> 
			apply_mutations actx !archetypes (DynArray.to_list unique_mutations);
		)

end
