open Ast
open EvalValue
open Type
open Printf
open Printer
open Globals

exception Invalid_expr
exception Invalid_ecso_analyse
exception Unhandled_meta
exception Unhandled_system_type
exception Unhandled_component_type
exception Unexpected_expr
exception Missing_ecso_library
exception Invalid_rlist_type
exception Invalid_rlist_optimization
exception Not_used
exception Found

let decode_module_type v =
	match EvalDecode.decode_enum v with
	| 0, [c] -> TClassDecl (EvalDecode.decode_ref c)
	| 1, [en] -> TEnumDecl (EvalDecode.decode_ref en)
	| 2, [t] -> TTypeDecl (EvalDecode.decode_ref t)
	| 3, [a] -> TAbstractDecl (EvalDecode.decode_ref a)
	| _ -> raise Invalid_expr

let string_of_path (path : Globals.path) : string =
	let str = ref "" in 
	let concat_name v = str := !str ^ v in
	let concat_pack v = concat_name (v ^ ".") in
		match path with
			| (p, s) -> (
				List.iter concat_pack p;
				concat_name s;
				!str
			)

let identifier_of_path (p,s) = match p with [] -> s | _ -> String.concat "_" p ^ "_" ^ s

(* Extract *)

type r =
	| REntity of tanon * bool * t

and rcomponent = {
	(* rc_path : path;
	rc_type : t;
	rc_ident : string;
	rc_opt : bool;
	rc_pos : pos; *)
	rc_name : string;
	rc_def : tclass_field;
}

(* let r_type_eq a b : bool =
	match a, b with
		| REntity rc_a, REntity rc_b -> rc_a.rc_ident = rc_b.rc_ident *)

type saccess =
	| SField of tfield_access
	| SAnon of tfunc
	| SLocal of tvar * texpr

type uexpr = 
	| EProcessSystem of sprocess
	| ECreateEntity of ecreate
	| ECreateComponent of ccreate
	| EDeleteComponent of cdelete
	| EGetComponent of cget

and sprocess = {
	expr : texpr;
	eorigin : tclass_field;
	group : texpr;
	s_requirement : r list;
}
and ecreate = {
	expr : texpr;
	eorigin : tclass_field;
	group : texpr;
	e_inits : ((string * pos * quote_status) * texpr) list;
	e_def : tanon;
}
and edelete = {
	expr : texpr;
	eorigin : tclass_field;
	group : texpr;
	e_def : tanon;
}
and ccreate = {
	c_expr : texpr;
	to_group : texpr;
}
and cdelete = {
	d_expr : texpr;
	from_group : texpr;
}
and cget = {
	g_expr : texpr;
}

let rec fetch_type t =
	match t with
	| TLazy tlazy ->
		(match !tlazy with
		| LAvailable t -> fetch_type t
		| LProcessing f -> fetch_type (f())
		| LWait f -> fetch_type (f()))
	| TMono tmono -> 
		(match tmono.tm_type with
		| Some t -> fetch_type t
		| None -> t)
	| _ -> t

let s_saccess v = 
	match v with
		| SField faccess -> "SField-" ^ (s_field_access s_type_kind faccess)
		| SAnon func -> "SAnon" ^ (s_expr_pretty func.tf_expr)
		| SLocal (svar,sexpr) -> "SLocal(" ^ svar.v_name ^ ")"

let s_saccess_kind v = 
	match v with
		| SField faccess -> "SField"
		| SAnon func -> "SAnon"
		| SLocal (svar,sexpr) -> "SLocal"

let rec edef_of_followed t =
	match fetch_type t with
	| TType (tdef, tparams) -> edef_of_followed tdef.t_type
	| TAnon tanon -> tanon
	| _ -> 
		print_endline ("Unhandled component type: " ^ (s_type_kind t));
		raise Unhandled_component_type

let hash_tanon v = Hashtbl.hash (s_type (TAnon v))
let hash_saccess v =
	Hashtbl.hash begin
		match v with
		| SField faccess -> "f" ^ (s_field_access s_type_kind faccess)
		| SAnon func -> "a" ^ (s_expr s_type_kind { eexpr = TFunction func; etype = func.tf_type; epos = func.tf_expr.epos; })
		| SLocal (svar,sexpr) -> "l" ^ (string_of_int svar.v_id)
	end

(* let make_srequirement ((tvar : tvar), (texpr_opt : texpr option)) : r = *)
let make_srequirement (name, is_opt, t) : r =
	let def : tanon = edef_of_followed t in
	REntity (def, is_opt, t)

let sort_requirements (r_list : r list) : r list = 
	r_list
	(* let r_sorting (a : r) (b : r) : int =
		match a, b with 
			| REntity c1, REntity c2 -> (
				match c1.rc_opt, c2.rc_opt with
					| true, false -> 1
					| false, true -> -1
					| _ ->
						if (EvalHash.path_hash c1.rc_path) < (EvalHash.path_hash c2.rc_path) then
							-1
						else
							1
			)
	in List.sort r_sorting r_list *)

(* let string_of_requirement r : string =
	match r with
		| REntity (def,opt,t) -> 
			(if opt then "?" else "") ^ (Globals.s_type_path rc.rc_path) *)

(* Process *)

(* type rlist =
	| LImpl of rlist_kind * rlist_length
	| LShare of typed_rlist
and rlist_length = {
	len_ident : string
}
and rlist_kind =
	| KArray of string
	(* | RVector of string * int *)
and typed_rlist = {
	(* l_requirement : r; *)
	mutable l_impl : rlist;
}

type rset = {
	r_name : string;
	r_hash : int;
	r_kind : rset_kind;
}
and rset_kind = typed_rlist list

let make_rset_kind rset_hash (sorted_requirements : r list) : rset_kind =
	let make_typed_rlist rset_hash r : typed_rlist =
		let list_name = match r with
			| REntity rc -> "clist_" ^ (string_of_int rset_hash) ^ "_" ^ rc.rc_ident
		in
		{
			l_requirement = r;
			l_impl = LImpl(KArray list_name, { len_ident = list_name ^ "_length"; })
		}
	in
	List.map (make_typed_rlist rset_hash) sorted_requirements *)

(* let make_rset (srequirements : r list) : rset =
	let sorted_requirements = sort_requirements srequirements in
	let named = ref "" in
		List.iter
			(fun r -> (named := !named ^ "#" ^ (string_of_requirement r)))
			sorted_requirements;
	let hash = EvalHash.hash !named in
	let kind = make_rset_kind hash sorted_requirements in
	{
		r_name = !named;
		r_hash = hash;
		r_kind = kind;
	} *)

(* let rset_eq a b : bool =
	a.r_hash = b.r_hash *)

let remove_dupplicate (f : 'a -> 'b -> bool) list =
	let new_list = ref [] in
	List.iter
		(fun a ->
			let f = f a in
			if false = (List.exists f !new_list)
				then
					new_list := a :: !new_list
		)
		list;
	!new_list

(* Optimization *)

let optimize_rlists unoptimized_rsets = ()
	(* let for_every_list f =
		List.iter
		(fun rset ->
			List.iter f rset.r_kind;
			()
		)
		unoptimized_rsets
	in
	for_every_list
		(fun a ->
			match a.l_impl with
				| LImpl (_, _) -> (* If have an implemention, seeks to convert other lists *)
					let chained = ref false in
					for_every_list
						(fun b ->
							if (!chained = false && a != b && r_type_eq a.l_requirement b.l_requirement) then
								(match b.l_impl with
									| LImpl (k_b, len_b) -> 
										print_endline "Optimaze!";
										b.l_impl <- LShare a;
										chained := true
									| _ -> ()
								)
						)
				| _ -> ()
		) *)
				

(* Tools *)

let append_field_into cl cf =
	cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
	cl.cl_ordered_fields <- cf :: cl.cl_ordered_fields;
	print_endline ("[ECSO] Generate " ^ cf.cf_name)

type cf_impl_accessor = {
	cf_access : tfield_access; 
	cf_type : t;
}

let mk_cf_accessor cl cf : cf_impl_accessor =
	{
		cf_access = FInstance (cl,[],cf);
		cf_type = cf.cf_type;
	}

let get_field_access t name =
	match t with
	| TInst (cl, params) ->
		let push = PMap.find name cl.cl_fields in
		{
			cf_access = FInstance (cl, params, push);
			cf_type = push.cf_type;
		}
	| _ -> 
		print_endline ("Unhandled get_field_access type for " ^ (s_type_kind t));
		raise Unhandled_component_type

let is_compatible_def (def : tanon) (with_def : tanon) : bool =
	let original_fields = PMap.map (fun f -> f) with_def.a_fields in
	let non_opt_fields = 
		let fields = ref PMap.empty in
		PMap.iter
			(fun name cf ->
				if Meta.has Meta.Optional cf.cf_meta = false then
					fields := PMap.add name cf !fields
			)
			original_fields;
		!fields
	in

	with_def.a_status := Const;
	with_def.a_fields <- non_opt_fields;
	let compatible = try unify_anons (TAnon def) (TAnon with_def) def with_def; true with Unify_error _ -> false in
	with_def.a_status := Closed;
	with_def.a_fields <- original_fields;

	print_endline ("Entity Type " ^ (s_type (TAnon def)));
	if compatible then
		print_endline ("------pass> " ^ (s_type (TAnon with_def)))
	else 
		print_endline ("------fail> " ^ (s_type (TAnon with_def)));
	compatible

let contains_compatible_defs from_list (with_def : tanon) : bool =
	try begin (Hashtbl.iter (fun def_hash def -> if is_compatible_def def with_def then raise Found) from_list); false end
	with | Found -> true

let foreach_compatible_system sprocesses f (def : tanon) =
	let processed = ref PMap.empty in
	Hashtbl.iter
		(fun shash sp ->
			List.iter
				(fun r -> 
					match r with
					| REntity (edef,opt,t) ->
						let h = hash_tanon edef in
						if (PMap.exists h !processed) = false && is_compatible_def def edef then f edef;
						processed := PMap.add h h !processed;
						()
				)
				sp.s_requirement
		)
		sprocesses

let foreach_dependent_system sprocesses f (def : tanon) =
	let processed = ref PMap.empty in
	Hashtbl.iter
		(fun def_hash sp ->
			List.iter
				(fun r -> 
					match r with
					| REntity (sdef,opt,t) -> 
						let h = hash_tanon sdef in
						if (PMap.exists h !processed) = false && (is_compatible_def def sdef || is_compatible_def sdef def) then f sdef;
						processed := PMap.add h h !processed;
						()
				)
				sp.s_requirement
		)
		sprocesses

let iter_with_locals f vars e =
	match e.eexpr with
	| TBlock el -> begin
		let local_cache = Hashtbl.create 0 in
		let add_value vid e =
			Hashtbl.add vars vid e;
			Hashtbl.add local_cache vid ()
		in
		List.iter
			(fun e ->
				match e.eexpr with
				| TVar(v,eo) -> 
					(match eo with | Some ve -> f vars ve | _ -> ());
					add_value v.v_id eo;
				| TBinop(OpAssign,{ eexpr = TLocal(v) },e2) when Hashtbl.mem vars v.v_id ->
					f vars e2;
					add_value v.v_id (Some e2);
				| _ ->
					f vars e
			)
			el;
		Hashtbl.iter
			(fun vid () ->
				Hashtbl.remove vars vid
			)
			local_cache
	end
	| _ ->
		iter (f vars) e

(* Transform *)

(* let rec retrieve_or_gen_rlist (into_cl : tclass) (rlist : typed_rlist) : tfield_access =
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	match rlist.l_impl with 
		| LImpl (k, len) ->
			(match k with
				| KArray list_name -> 

					let retrieved = try Some (PMap.find list_name into_cl.cl_fields) with Not_found -> None in
					let list = match retrieved with
						| Some cf_list ->
							cf_list
						| None ->
							let list_type = match rlist.l_requirement with
								| REntity rc -> rc.rc_type
							in
							let list_initialization pos = match rlist.l_requirement with
								| REntity rc -> Some {
									eexpr = TArrayDecl[];
									etype = list_type;
									epos = pos;
								}
							in

							(* Generate the runtime list *)
							let t = api.tarray list_type in
							let pos = { pfile = "ecso.gen.RList"; pmin = 1; pmax = 4; } in
							let kind = Var { v_read = AccNormal; v_write = AccNormal; } in
							let cf_list = Gencommon.mk_class_field list_name t true pos kind [] in (* name type public pos kind params *)
							cf_list.cf_expr <- list_initialization cf_list.cf_pos;
							
							append_field_into into_cl cf_list;
							cf_list
					in
					FInstance (into_cl,[],list)
			)
		| LShare l -> 
			retrieve_or_gen_rlist into_cl l *)

(* let retrieve_or_gen_rset_lists (into_cl : tclass) (rset : rset) : tfield_access list = *)
	(* List.map
		(retrieve_or_gen_rlist into_cl)
		rset.r_kind *)
	

(* let retrieve_or_gen_rset_range (into_cl : tclass) (rset : rset) : tfield_access =
	let range_name = "rdata_" ^ (string_of_int rset.r_hash) ^ "$length" in
	let retrieved = try Some (PMap.find range_name into_cl.cl_fields) with Not_found -> None in
	let range = match retrieved with
		| Some cf_length ->
			cf_length
		| None ->
			let api = ((EvalContext.get_ctx()).curapi.get_com()).basic in
			let cf_length = Gencommon.mk_class_field
				range_name (* name *)
				api.tint (* type *)
				true (* public *)
				{ pfile = "ecso.gen.LengthOfRSet"; pmin = 1; pmax = 4; } (* pos *)
				(Var { v_read = AccNormal; v_write = AccNormal; }) (* kind *)
				[] (* params *)
			in
			cf_length.cf_expr <- Some {
				eexpr = TConst (TInt 0l);
				etype = cf_length.cf_type;
				epos = cf_length.cf_pos;
			};
			append_field_into into_cl cf_length;
			cf_length
	in FInstance (into_cl,[],range) *)

let setup_extern_field cl cf_name arg_name arg_count = 
	let compiler = (EvalContext.get_ctx()).curapi in
	let api = (compiler.get_com()).basic in
	let cf = PMap.find cf_name cl.cl_fields in
	if (has_class_field_flag cf CfExtern) then begin
		remove_class_field_flag cf CfExtern;
		cf.cf_kind <- Method MethInline;
		(match cf.cf_expr with
			| None ->
				let mk_arg name =
					let system_varg = alloc_var VGenerated name api.tstring cf.cf_pos in
					(system_varg, Some (mk (TConst TNull) system_varg.v_type system_varg.v_pos))
				in
				let mk_args name count =
					List.init
						count 
						(fun i -> mk_arg (name ^ string_of_int i))
				in
				let arg_var arg = match arg with | (v,_) -> v in
				let type_arg arg = 
					let var = arg_var arg in
					(var.v_name,false,var.v_type)
				in
				let args = 
					if arg_count = 1 then
						[mk_arg arg_name]
					else
						mk_args arg_name arg_count
				in
				let process_impl = {
					tf_args = args;
					tf_type = TFun (List.map type_arg args,api.tvoid);
					tf_expr = mk (TBlock[]) api.tvoid cf.cf_pos;
				} in
				cf.cf_expr <- Some (mk (TFunction process_impl) process_impl.tf_type cf.cf_pos);
				cf.cf_type <- process_impl.tf_type
			| Some e -> ()
		)
	end else ()

let implement_uexpr (eorigin : tclass_field) (expr : texpr) (impl : texpr) =
	
		let rec check_expr (e : texpr) : texpr = 
			if (s_expr s_type_kind e = s_expr s_type_kind expr && e.epos.pmin = expr.epos.pmin && e.epos.pmax = expr.epos.pmax && e.epos.pfile = expr.epos.pfile) then
				impl
			else
				map_expr
					check_expr
					e
		in
		eorigin.cf_expr <- Some (match eorigin.cf_expr with
			| Some field_expr ->
				(* Some *) (
					check_expr
						field_expr
				)
			| None ->
				(* Some *) impl
		)

(* let make_process (g : tclass) (rset : rset) (sp : sprocess) : texpr = 
	print_endline "make process _ _ _";
	(* rewrite `g.process(s)` into `for (i in g.iterator<RSET-HASH>()) s(g.get_<RSET-HASH>_r0_at(i))` *)
	(* var i:Int = g.<RSET-HASH>_length;
	while( --i >= 0 )
		s( g.get_<RSET-HASH>_r0_at(i) ); *)
	let compiler = (EvalContext.get_ctx()).curapi in
	let api = (compiler.get_com()).basic in
	let mk_block exprs = {
		eexpr = TBlock exprs;
		etype = (List.nth exprs (List.length exprs - 1)).etype;
		epos = sp.expr.epos;
	} in
	let mk_texpr = function
		| TClassDecl c -> TAnon { a_fields = PMap.empty; a_status = ref (Statics c) }
		| TEnumDecl e -> TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics e) }
		| TAbstractDecl a -> TAnon { a_fields = PMap.empty; a_status = ref (AbstractStatics a) }
		| TTypeDecl _ -> assert false
	in

	let p = sp.expr.epos in
	let gmodule = TClassDecl g in
	let gexpr = mk (TTypeExpr gmodule) (mk_texpr gmodule) p in	
	let smodule = match sp.s_field with 
		| FStatic (cl,cf) -> TClassDecl cl
		| _ -> raise Unhandled_system_type
	in
	let sexpr = mk (TTypeExpr smodule) (mk_texpr smodule) p in
	
	let flength = mk (TField (gexpr,retrieve_or_gen_rset_range g rset)) api.tint p in (* g.<RSET-HASH>_length *)
	let var_length = alloc_var VGenerated "rlength" flength.etype flength.epos in
	let decl_length = mk (TVar (var_length,Some flength)) api.tvoid p in (* var i = $e{flength} *)
	let ident_length = mk (TLocal var_length) flength.etype p in
	let decrement_length = mk (TUnop (Decrement,Prefix,ident_length)) flength.etype p in

	let rlist_accesses = retrieve_or_gen_rset_lists g rset in
	let args = List.map
		(fun rla ->
			let rltype = match rla with
				| FInstance (_,_,cf) -> cf.cf_type
				| _ -> raise Unexpected_expr
			in
			let rtype = match rltype with
				| TAbstract (_,[rtype]) | TInst (_,[rtype]) | TType (_,[rtype]) ->
					rtype
				| _ ->
					raise Unhandled_component_type
			in
			let flist = mk 
				(TField (gexpr,rla))
				rltype
				p
			in
			mk (TArray (flist,ident_length)) rtype p
		)
		rlist_accesses
	in
	let fsystem = 
		mk 
		(TField (sexpr,sp.s_field)) 
		(match sp.s_field with
			| FStatic (cl,cf) -> cf.cf_type
			| _ -> raise Unhandled_system_type
		)
		p
	in
	let call = mk (TCall (fsystem, args)) api.tvoid p in
	let zero = mk (TConst (TInt 0l)) api.tint p in
	let call_all_condition = mk (TBinop (OpGte,decrement_length,zero)) api.tbool p in
	let call_all = mk (TWhile (call_all_condition, call, NormalWhile)) api.tvoid p in
	
	mk_block [
		decl_length;
		call_all;
		ident_length
	] *)

(* Generate *)

type rset_kind =
	| RMonolist

and rset_status = 
	| Retrieved of rset_impl
	| Missing of rset_build_data

and rset_build_data =
	| BMonolist of { list_name : string; length_name : string; }

and rset_impl = {
	gen_add : texpr -> texpr -> pos -> texpr;
	gen_remove : texpr -> texpr -> pos -> texpr;
	gen_iter : texpr -> pos -> (texpr -> texpr) -> t -> texpr;
}

let make_rmonolist_accessor (list_impl : cf_impl_accessor) (length_impl : cf_impl_accessor) : rset_impl =
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let list g p = mk (TField (g, list_impl.cf_access)) list_impl.cf_type p in
	let length g p = mk (TField (g, length_impl.cf_access)) length_impl.cf_type p in
	let gen_add_func (g : texpr) (einstance : texpr) (p : pos) : texpr =
		let list_push_info = get_field_access list_impl.cf_type "push" in
		let list_push = mk (TField (list g p, list_push_info.cf_access)) list_push_info.cf_type p in

		(mk (TBlock[
			(mk (TCall (list_push, [einstance])) api.tint p); (* rset.push( _ ) TODO: ensure to not push a reference! *)
			(mk (TBinop (OpAdd,length g p, mk (TConst (TInt 1l)) api.tint p)) api.tint p); (* ++rset_length *)
			(mk (TUnop (Increment,Prefix,length g p)) api.tint p); (* ++rset_length *)
		]) api.tint p)
	in
	let gen_remove_func (g : texpr) (einstance : texpr) (p : pos) : texpr =
		let list_remove_info = get_field_access list_impl.cf_type "remove" in
		let list_remove = mk (TField (list g p, list_remove_info.cf_access)) list_remove_info.cf_type p in

		(mk (TBlock[
			(mk (TIf (
				(mk (TCall (list_remove, [einstance])) api.tbool p), (* rset.remove( _ ) *)
				(mk (TUnop (Decrement,Prefix,length g p)) api.tint p), (* --rset_length *)
				None
			)) api.tvoid p);
		]) api.tvoid p)
	in
	let gen_iter_func (g : texpr) (p : pos) (f : texpr -> texpr) (iterated_t : t) : texpr =
		let ivar = alloc_var VGenerated "i" api.tint p in
		let i = mk (TLocal ivar) ivar.v_type p in
		let decrease_i = mk (TUnop (Decrement,Prefix,i)) api.tint p in
		let entity_at_i = mk (TArray (list g p,i)) iterated_t p in
		(mk (TBlock[
			mk (TVar (ivar, Some (length g p))) api.tint p;
			mk (TWhile (
				mk (TBinop (OpGte,decrease_i, mk (TConst (TInt 0l)) api.tint p)) api.tbool p,
				f entity_at_i,
				NormalWhile
			)) api.tvoid p;
		]) api.tvoid p)
	in
	{
		gen_add = gen_add_func;
		gen_remove = gen_remove_func;
		gen_iter = gen_iter_func;
	}

let retrieve_rset (kind : rset_kind) (into_cl : tclass) (def : tanon) : rset_status =
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let rset_id = string_of_int (hash_tanon def) in
	let list_name = "slist_" ^ rset_id in
	let length_name = "slength_" ^ rset_id in
	let list_impl =
		try Some (mk_cf_accessor into_cl (PMap.find list_name into_cl.cl_fields))
		with Not_found -> None
	in
	let length_impl =
		try Some (mk_cf_accessor into_cl (PMap.find length_name into_cl.cl_fields))
		with Not_found -> None
	in
	match list_impl, length_impl with
	| Some list_impl, Some length_impl ->
		Retrieved (make_rmonolist_accessor list_impl length_impl)
	| _ ->
		(
		match kind with
		| RMonolist ->
			Missing (BMonolist {
				list_name = list_name;
				length_name = length_name;
			})
		)		

let gen_rset (bdata : rset_build_data) (into_cl : tclass) (def : tanon) : rset_impl =
	match bdata with
	| BMonolist impl_data ->
		let gcon = (EvalContext.get_ctx()).curapi.get_com() in
		let api = gcon.basic in
		let list_impl =
			let t = api.tarray (TAnon def) in
			let pos = { pfile = "ecso.gen.RList"; pmin = 1; pmax = 4; } in
			let kind = Var { v_read = AccNormal; v_write = AccNormal; } in
			let cf_list = Gencommon.mk_class_field impl_data.list_name t true pos kind [] in (* name type public pos kind params *)
			cf_list.cf_expr <- Some (mk (TArrayDecl []) cf_list.cf_type cf_list.cf_pos);
			append_field_into into_cl cf_list;
			mk_cf_accessor into_cl cf_list
		in
		let length_impl =
			let pos = { pfile = "ecso.gen.RLength"; pmin = 1; pmax = 4; } in
			let kind = Var { v_read = AccNormal; v_write = AccNormal; } in
			let cf_length = Gencommon.mk_class_field impl_data.length_name api.tint true pos kind [] in (* name type public pos kind params *)
			cf_length.cf_expr <- Some (mk (TConst (TInt 0l)) api.tint cf_length.cf_pos);
			append_field_into into_cl cf_length;
			mk_cf_accessor into_cl cf_length
		in
		match retrieve_rset RMonolist into_cl def with
		| Retrieved impl -> impl
		| Missing _ -> 
			print_endline "Could not generate correctly";
			raise Invalid_expr
	
let retrieve_or_gen_rset (kind : rset_kind) (into_cl : tclass) (def : tanon) : rset_impl =
	match retrieve_rset kind into_cl def with
	| Retrieved impl -> impl
	| Missing build_data ->
		gen_rset build_data into_cl def

let gen_ecreate (into_cl : tclass) sprocesses (def_hash : int) (ec : ecreate) = 
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let p = ec.expr.epos in
	let block = ref [mk (TConst (TString ("CreateEntity " ^ (s_type (TAnon ec.e_def))))) api.tstring p] in
	let einstance_var = alloc_var VGenerated "e" (TAnon ec.e_def) p in
	let einstance = mk (TLocal einstance_var) einstance_var.v_type p in
	print_endline ("gen_ecreate" ^ (s_type (TAnon ec.e_def)));
	foreach_compatible_system sprocesses
		(fun sdef -> 
			match retrieve_rset RMonolist into_cl sdef with
			| Retrieved rset -> block := (rset.gen_add ec.group einstance p) :: !block;
				print_endline ("--- gen add into " ^ (s_type (TAnon sdef)))
			| Missing _ -> ()
		)
		ec.e_def;
	block := (mk (TVar (einstance_var, Some ec.expr)) api.tvoid p) :: !block;
	implement_uexpr ec.eorigin ec.expr (mk (TBlock !block) api.tstring p);
	()

let gen_edelete (into_cl : tclass) sprocesses (def_hash : int) (ed : edelete) = 
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let p = ed.expr.epos in
	let block = ref [mk (TConst (TString ("DeleteEntity " ^ (s_type (TAnon ed.e_def))))) api.tstring p] in
	let einstance_var = alloc_var VGenerated "e" (TAnon ed.e_def) p in
	let einstance = mk (TLocal einstance_var) einstance_var.v_type p in
	foreach_dependent_system sprocesses
		(fun sdef -> 
			match retrieve_rset RMonolist into_cl sdef with
			| Retrieved rset -> block := (rset.gen_remove ed.group einstance p) :: !block
			| Missing _ -> ()
		)
		ed.e_def;
	block := (mk (TVar (einstance_var, Some ed.expr)) api.tvoid p) :: !block;
	implement_uexpr ed.eorigin ed.expr (mk (TBlock !block) api.tstring p);
	()

let gen_sprocess (into_cl : tclass) possible_edefs (system : saccess) (sp : sprocess) = 
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let p = sp.expr.epos in
	let used = ref true in
	let gen_system_call args : texpr =
		(mk (TCall (sp.expr, args)) api.tvoid p)
	in
	let rec gen_next_requirement (r_list : r list) (system_args : texpr list) : texpr =
		if List.length r_list = 0 then
			gen_system_call (List.rev system_args)
		else match List.hd r_list with
		| REntity (def,opt,t) -> 
			if contains_compatible_defs possible_edefs def then begin
				let rset = retrieve_or_gen_rset RMonolist into_cl def in
				let gen_block (entity : texpr) =
					(gen_next_requirement (List.tl r_list) (entity :: system_args))
				in
				rset.gen_iter sp.group p gen_block t
			end else begin
				used := false;
				mk (TConst TNull) t p
			end
	in
	let impl = gen_next_requirement sp.s_requirement [] in
	if !used then begin
		implement_uexpr sp.eorigin sp.expr (mk (TBlock[
			impl;
			(mk (TConst (TString ("ProcessSystem " ^ (s_saccess system) ^ ""))) api.tstring p); (* report *)
		]) api.tstring p)
	end else
		implement_uexpr sp.eorigin sp.expr (mk (TBlock[
			(mk (TConst (TString ("ProcessSystem " ^ (s_saccess system) ^ " (skipped)"))) api.tstring p);
		]) api.tstring p);
	()
	(* let rsets_for_iteration =
		try
			List.filter_map
				(fun r ->
					match r with
					| REntity (def,opt,t) -> 
						if contains_compatible_defs possible_edefs def then
							begin
							Some (retrieve_or_gen_rset RMonolist into_cl def)
							end
						else
							raise Not_used
				)
				sp.s_requirement
		with Not_used -> 
			used := false;
			[]
	in
	if !used then
		begin
		let impl = ref None in
		List.iter 
			(fun rset -> (
				let block (entity : texpr) : texpr =
					(* iter the next until calling the system *)
					call sp.s_field [entity]
				in
				let impl = rset_impl.gen_iter sp.group sp sp.expr.epos block in
			))
			rsets_for_iteration
		implement_uexpr sp.eorigin sp.expr impl;
		end
	else
		() *)

type c = {
	def : t;
}
and caccess_expr =
	| CCreate of (texpr * c)
	| CDelete of (texpr * c)
	| CGet of (texpr * c)

type p =
	| PSystem of psystem

and psystem = {
	spath : string;
	sprocess : pexecution;
}

and pexecution =
	| PExpr of texpr

class plugin =
	object (self)

		val mutable need_init = (true)

		(* Analyse *)

		val mutable ecso_entity_group = (None : tclass option)

		method analyze_types types =
			List.iter self#analyse_type types

		method analyse_type t =
			match t with
				| TClassDecl cl -> 
					let s_cl_path  = Globals.s_type_path cl.cl_path in
					(match s_cl_path with
						| "ecso.EntityGroup" -> (ecso_entity_group <- Some cl)
						| _ ->
							List.iter (fun static -> self#analyse_field static true) cl.cl_ordered_statics;
							List.iter (fun field -> self#analyse_field field false) cl.cl_ordered_fields;
							List.iter (fun field -> self#analyse_field field false) cl.cl_overrides;
							self#analyse_field_opt cl.cl_constructor false
					)
				| TAbstractDecl a ->
					let s_a_path  = Globals.s_type_path a.a_path in
					(match s_a_path with
						| _ -> ()
					)
				| _ -> ()

		method analyse_field_opt field is_static = 
			match field with
				| Some field -> self#analyse_field field is_static
				| _ -> ()

		method analyse_field field is_static = 
			(match field.cf_expr_unoptimized with
				| Some func -> raise Invalid_ecso_analyse
				| _ -> ()
			);
			(match field.cf_expr with
				| Some texpr -> self#analyse_texpr field texpr
				| _ -> ()
			)
		
		method analyse_texpr (eorigin : tclass_field) texpr =
			let rec loop vars e =
				match e.eexpr with
				| TCall ({ eexpr = TField (e1, FInstance(cl, _, field)) }, args) ->
					if field.cf_name = "process" && (string_of_path cl.cl_path) = "ecso.EntityGroup" then
						self#extract_sprocess eorigin e1 vars args
					else if field.cf_name = "createEntity" && (string_of_path cl.cl_path) = "ecso.EntityGroup" then
						self#extract_ecreate eorigin e1 (match args with | [arg] -> arg | _ -> raise Unexpected_expr)
					else if field.cf_name = "deleteEntity" && (string_of_path cl.cl_path) = "ecso.EntityGroup" then
						self#extract_edelete eorigin e1 (match args with | [arg] -> arg | _ -> raise Unexpected_expr)
					else
						List.iter (iter_with_locals loop vars) args
				| _ -> iter_with_locals loop vars e
			in
			loop (Hashtbl.create 0) texpr
			(* iter (self#analyse_texpr eorigin) texpr *)

		(* Extract *)

		val mutable ecso_extracts = ([] : uexpr list)
		val mutable ecreates = Hashtbl.create 0 ~random:false
		val mutable edeletes = Hashtbl.create 0 ~random:false
		val mutable sprocesses = Hashtbl.create 0 ~random:false
		val mutable systems = Hashtbl.create 0 ~random:false

		method extract_sprocess (eorigin : tclass_field) (entity_group : texpr) vars (process_args : texpr list) =
			(* let extract ((e, tfield_access) : texpr * tfield_access) =  *)
			let extract_system args ret e = 
				let rec extract_access e =
					match e.eexpr with
					| TParenthesis e1 | TBlock [e1] | TCast(e1,None) -> extract_access e1
					| TMeta (m,e1) ->
						begin match m with
						| _ -> extract_access e1
						end
					| TFunction tfunc -> SAnon tfunc
					| TField (f, faccess) -> SField faccess
					| TLocal tvar ->
						if not tvar.v_final then begin
							print_endline ("Cannot process non-final system " ^ tvar.v_name);
							raise Invalid_expr
						end else if Hashtbl.mem vars tvar.v_id then begin
							match Hashtbl.find vars tvar.v_id with
							| Some sexpr ->
								SLocal (tvar, sexpr)
							| None -> 
								print_endline ("Cannot process non-initialized system " ^ tvar.v_name);
								raise Invalid_expr
						end else begin
							print_endline ("Cannot process unknown system " ^ tvar.v_name);
							raise Invalid_expr
						end
					| TBlock el when List.length el > 0 -> begin (* happens with function binding *)
						let block_vars = Hashtbl.create 1 in (* at least 1 for the binded value *)
						List.iter
							(fun e ->
								match e.eexpr with
								| TVar(v,Some e) -> Hashtbl.add block_vars v.v_id e
								| _ -> ()
							)
							el;
						begin match List.nth el (List.length el - 1) with
							| { eexpr = TFunction f } ->
								begin match f.tf_expr.eexpr with
									| TBlock([{ eexpr = TCall({ eexpr = TLocal v},args) }])
									| TCall({ eexpr = TLocal v},args)
									when Hashtbl.mem block_vars v.v_id ->
										print_endline ("Extract System from Block: " ^ s_expr_pretty e);
										SLocal (v, Hashtbl.find block_vars v.v_id)
									| _ ->
										SAnon f
								end
							| e1 ->
								extract_access e1
						end
					end	
					| _ -> 
						print_endline ("Fail extract system access from " ^ s_expr_kind e);
						raise Invalid_expr
				in
				let r_list = List.map make_srequirement args in
				let saccess = extract_access e in
				let shash = hash_saccess saccess in
				if not (Hashtbl.mem systems shash) then
					Hashtbl.add systems shash saccess;
				Hashtbl.add sprocesses shash {
					expr = e;
					eorigin = eorigin;
					group = entity_group;
					s_requirement = r_list;
				}
			in
			let rec analyse_arg e =
				match e.eexpr, fetch_type e.etype with
				| _, TFun (args, ret) ->
					extract_system args ret e
				| TMeta (metadata_entry, texpr), _ -> 
					(match metadata_entry with
						| (Meta.ImplicitCast, [], pos) -> (
								match texpr.eexpr with
									| TCast (texpr, _) -> analyse_arg texpr
									| _ -> 
										print_endline ("[ECSO] Wrong process parsing " ^ (s_expr s_type_kind texpr));
										raise Unexpected_expr
							)
						| _ -> raise Unhandled_meta
					)
				| _ ->
					print_endline ("[ECSO] Wrong anonymous system parsing for " ^ (s_type_kind e.etype));
					raise Unexpected_expr
			in
			List.iter analyse_arg process_args
		
		method extract_ecreate (eorigin : tclass_field) (entity_group : texpr) (edef : texpr) =
			match follow edef.etype with
			| TAnon def ->
				Hashtbl.add ecreates (hash_tanon def) {
					expr = edef;
					eorigin = eorigin;
					group = entity_group;
					e_inits = [](* inits *);
					e_def = def;
				}
			| _ ->
				print_endline ("[ECSO] Wrong create parsing of " ^ (s_type_kind edef.etype));
				raise Unhandled_component_type
		
		method extract_edelete (eorigin : tclass_field) (entity_group : texpr) (einstance : texpr) =
			match follow einstance.etype with
			| TAnon def ->
				Hashtbl.add edeletes (hash_tanon def) {
					expr = einstance;
					eorigin = eorigin;
					group = entity_group;
					e_def = def;
				}
			| _ ->
				print_endline ("[ECSO] Wrong deletion parsing of " ^ (s_type_kind einstance.etype));
				raise Unhandled_component_type

		(* Process *)

		method init () =
			if need_init then begin
				need_init <- false;
				print_endline "Init Ecso plugin";
				let compiler = (EvalContext.get_ctx()).curapi in
					compiler.after_typing
						self#on_after_typing
			end;
			vnull
		
		method on_after_typing haxe_types =
			self#analyze_types haxe_types;

			(* List.iter
				(fun uexpr ->
					match uexpr with
						| EProcessSystem sp -> print_endline ("SysUexpr: " ^ (string_of_system sp.s_field)); ()
						| _ -> ()
				)
				ecso_extracts; *)

			(* let raw_rsets : rset list = 
				List.filter_map 
					(fun uexpr ->
						match uexpr with
							| EProcessSystem sp -> Some (make_rset sp.s_requirement)
							| _ -> None
					)
					ecso_extracts
			in *)

			(* let rsets_uniq = remove_dupplicate rset_eq raw_rsets in
			optimize_rlists rsets_uniq; *)
			
			(* let get_uniq_rset sp = 
				List.find
					(fun rs -> rset_eq rs (make_rset sp.s_requirement))
					rsets_uniq
			in *)

			(* let rsets_usages : (rset * uexpr) list =
				List.filter_map
					(fun uexpr ->
						match uexpr with
							| EProcessSystem sp ->
								Some (List.find (rset_eq (make_rset sp.s_requirement)) rsets_uniq, uexpr)
							| _ -> None
					)
					ecso_extracts
			in *)

			(* List.iter
				(fun (r,u) ->
					print_endline ("RSy-u: " ^ r.r_name)
				)
				rsets_usages; *)

			let ecso_entity_group = (match ecso_entity_group with
				| Some cl -> cl
				| None -> raise Missing_ecso_library) in (* TODO: print nice error message *)

			let print_sprocess k sp = 
				List.iter
					(fun r -> 
						(match r with
						| REntity (def,opt,t) -> print_endline ("Process System" ^ (s_type (TAnon def)) )
						)
					)
					sp.s_requirement
			in

			let print_ecreate k ec = 
				print_endline ("Create Entity " ^ (s_type (TAnon ec.e_def)) )
			in

			print_endline "System list:";
			Hashtbl.iter
				(fun shash system -> print_endline (s_saccess system))
				systems;
			
			print_endline "Process calls:";
			Hashtbl.iter
				print_sprocess
				sprocesses;

			print_endline ("Entities created: " ^ (string_of_int (Hashtbl.length ecreates)));
			Hashtbl.iter
				print_ecreate
				ecreates;
			
			let possible_edefs = (* TODO: find a better structure for this *)
				let list = Hashtbl.create (1 + (Hashtbl.length ecreates) / 4) in
				Hashtbl.iter
					(fun def_hash ec -> if (Hashtbl.mem list def_hash) = false then Hashtbl.add list def_hash ec.e_def)
					ecreates;
				list
			in

			print_endline ("List possible_edefs " ^ (string_of_int (Hashtbl.length possible_edefs)));

			Hashtbl.iter
				(fun shash sp -> 
					gen_sprocess 
						ecso_entity_group
						possible_edefs
						(Hashtbl.find systems shash)
						sp
				)
				sprocesses;
			
			Hashtbl.iter
				(gen_ecreate ecso_entity_group sprocesses)
				ecreates;
			
			Hashtbl.iter
				(gen_edelete ecso_entity_group sprocesses)
				edeletes;
			
			(* setup_process ecso_entity_group; *)
			setup_extern_field ecso_entity_group "process" "s" 32;
			setup_extern_field ecso_entity_group "createEntity" "e" 1;
			setup_extern_field ecso_entity_group "deleteEntity" "e" 1;
									
			(* List.iter
				(fun (rset,uexpr) ->
					match uexpr with
						| EProcessSystem sp ->
							(* let rset = List.find (rset_eq (make_rset sp.s_requirement)) rsets_uniq in *)
							print_endline "implement sp";
							let impl = make_process ecso_entity_group rset sp in
							(match sp.eorigin.cf_expr with
								| Some e ->
									sp.eorigin.cf_expr <- Some { e with eexpr = (implement_uexpr sp impl).eexpr }
								| None ->
									sp.eorigin.cf_expr <- Some (implement_uexpr sp impl)
							)
							
							(* let implemented_cf_expr = change_expr ps.expr impl (match ps.eorigin.cf_expr with | Some e -> e | None -> raise Invalid_ecso_analyse) in
							ps.eorigin.cf_expr <- Some implemented_cf_expr *)
						| _ -> ()
				)
				rsets_usages; *)

			(*
				// uexpr: g.process( Sys.move )
				for (i in 0...g.rsetCompsAB_length)
					Sys.move(g.rsetCompsAB_A[i], g.rsetCompsAB_B[i])
			 *)

			(* system_to_set_link = rset * tsystem *)
			
			()
		(**
			Takes `haxe.macro.Position` and returns a string of that position in the same format used for
			compiler errors
		*)
		method stringify_position (pos:value) : value =
			let pos = EvalDecode.decode_pos pos in
			let str = Lexer.get_error_pos (Printf.sprintf "%s:%d:") pos in
			EvalEncode.encode_string str

	end
;;

let api = new plugin in

(**
	Register our plugin API.
	This code is executed upon `eval.vm.Context.loadPlugin` call.
*)
EvalStdLib.StdContext.register [
	("init", EvalEncode.vfun0 api#init);
]