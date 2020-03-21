open Ast
open EvalValue
open Type
open Printf
open Printer
open Globals
open AnalyzerTexpr

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
exception Not_implemented
exception Found
exception Found_opt_expr of texpr option

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

let rec edef_of_followed t =
	match fetch_type t with
	| TType (tdef, tparams) -> edef_of_followed tdef.t_type
	| TAnon tanon -> tanon
	| _ -> 
		print_endline ("Unhandled component type: " ^ (s_type_kind t));
		raise Unhandled_component_type

(* Extract *)

type uft = field_tag * string

and field_tag =
	| FTField of tclass_field
	| FTClassInit of tclass

type ecreate = {
	expr : texpr;
	uft : uft;
	group : texpr;
	e_inits : ((string * pos * quote_status) * texpr) list;
	ec_def : tanon;
}
type edelete = {
	expr : texpr;
	uft : uft;
	group : texpr;
	ed_def : tanon;
}
type eprocess = {
	ep_requirement : r list;
	ep_group : texpr;
	ep_uft : uft;
	ep_system : texpr;
}
and r =
	| REntity of tanon * bool * Type.t

type saccess =
	| SField of tfield_access * tfunc * texpr
	| SAnon of tfunc * AnalyzerTypes.analyzer_context * AnalyzerTypes.BasicBlock.t
	| SLocal of tvar * tfunc

let make_srequirement (name, is_opt, t) : r =
	let def : tanon = edef_of_followed t in
	REntity (def, is_opt, t)

let hash_saccess v = Hashtbl.hash begin
	match v with
	| SField (faccess,_,_) -> "f" ^ (s_field_access s_type_kind faccess)
	| SAnon (func,ctx,bb) -> "a" ^ (s_expr s_type_kind { eexpr = TFunction func; etype = func.tf_type; epos = func.tf_expr.epos; })
	| SLocal (svar,_) -> "l" ^ (string_of_int svar.v_id)
end

let s_saccess v = match v with
	| SField (faccess,tf,_) -> "SField-" ^ s_field_access s_type_kind faccess ^ "()" ^ s_expr_pretty tf.tf_expr
	| SAnon (tf,ctx,bb) -> "SAnon" ^ s_expr_pretty tf.tf_expr
	| SLocal (svar,tf) -> "SLocal(" ^ svar.v_name ^ ")" ^ s_expr_pretty tf.tf_expr

let s_saccess_kind v = match v with
	| SField _ -> "SField"
	| SAnon _ -> "SAnon"
	| SLocal _ -> "SLocal"

let hash_tanon v = Hashtbl.hash (s_type (TAnon v))

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

	if compatible then
		print_endline ("------pass> " ^ (s_type (TAnon with_def)))
	else 
		print_endline ("------fail> " ^ (s_type (TAnon with_def)));
	compatible

let contains_compatible_defs from_list (with_def : tanon) : bool =
	try begin (Hashtbl.iter 
		(fun def_hash def -> 
			print_endline ("Entity Type " ^ (s_type (TAnon def))); (* FIXME: improve debuging *)
			if is_compatible_def def with_def then raise Found
		) from_list); false end
	with | Found -> true

let foreach_compatible_system eprocesses f (def : tanon) =
	print_endline ("Entity Type " ^ (s_type (TAnon def)));
	let processed = ref PMap.empty in
	let check_ep (ep : eprocess) =
		let check_r (r : r) = match r with
			| REntity (edef,opt,t) ->
				let h = hash_tanon edef in
				if (PMap.exists h !processed) = false && is_compatible_def def edef then f edef;
				processed := PMap.add h h !processed;
				()
		in
		List.iter
			check_r
			ep.ep_requirement
	in
	DynArray.iter
		check_ep
		eprocesses

let foreach_dependent_system eprocesses f (def : tanon) =
	let processed = ref PMap.empty in
	let check_ep (ep : eprocess) =
		let check_r (r : r) = match r with
			| REntity (sdef,opt,t) -> 
				let h = hash_tanon sdef in
				if (PMap.exists h !processed) = false && (is_compatible_def def sdef || is_compatible_def sdef def) then f sdef;
				processed := PMap.add h h !processed;
				()
		in
		List.iter
			check_r
			ep.ep_requirement
	in
	DynArray.iter
		check_ep
		eprocesses

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

let is_expr_with_uft uft e : bool = 
	match e.eexpr with
	| TMeta ((Meta.Custom "$ecso.uft",[EConst(Int e_uft),_],_), _) when e_uft = uft -> true
	| _ -> false
let write_expr_with_uft uft e : texpr =
	{ e with eexpr = TMeta ((Meta.Custom "$ecso.uft",[EConst(Int uft),e.epos],e.epos), e) }
let remove_uft e : texpr = 
	match e.eexpr with
	| TMeta ((Meta.Custom "$ecso.uft",_,_), e1) -> e1
	| _ -> e
let get_uft cft e = match e.eexpr with
	| TMeta ((Meta.Custom "$ecso.uft",[EConst(Int v),_],_), _) ->
		(cft,v)
	| _ -> 
		print_endline ("missing uft from: " ^ s_expr s_type_kind e);
		raise Invalid_ecso_analyse

let rec append_expr e into : texpr =
	match into.eexpr with
	| TParenthesis e1 | TBlock [e1] | TCast(e1,None) -> append_expr e e1
	| TMeta(m,e1) -> { into with eexpr = TMeta(m,append_expr e e1) }
	| TBlock el -> { into with eexpr = TBlock(el@[e]) }
	| _ -> { into with eexpr = TBlock(into :: [e]) }

let implement_uexpr (uft : uft) (impl : texpr) (replace : bool) =
	let ft,uft = match uft with | (ft,meta) -> (ft,meta) in
	let implemented = ref false in
	let rec check_expr (e : texpr) : texpr = 
		if is_expr_with_uft uft e then begin
			implemented := true;
			let impl = 
				if replace then 
					impl
				else
					append_expr impl e
			in
			write_expr_with_uft uft impl
		end else
			map_expr check_expr e
	in
	begin match ft with
		| FTField cf ->
			cf.cf_expr <- Some (match cf.cf_expr with
				| Some e -> check_expr e
				| None -> impl
			)
		| FTClassInit cl ->
			cl.cl_init <- Some (match cl.cl_init with
				| Some e -> check_expr e
				| None -> impl
			)
	end;
	if not !implemented then
		raise Not_implemented

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
	gen_iter : eprocess -> pos -> (texpr -> texpr) -> t -> texpr;
}

let make_rmonolist_accessor (list_impl : cf_impl_accessor) (length_impl : cf_impl_accessor) : rset_impl =
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let list g p = mk (TField (g, list_impl.cf_access)) list_impl.cf_type p in
	let length g p = mk (TField (g, length_impl.cf_access)) length_impl.cf_type p in
	let gen_add_func (g : texpr) (model : texpr) (p : pos) : texpr =
		let list_push_info = get_field_access list_impl.cf_type "push" in
		let list_push = mk (TField (list g p, list_push_info.cf_access)) list_push_info.cf_type p in

		(mk (TBlock[
			(mk (TCall (list_push, [model])) api.tint p); (* rset.push( _ ) TODO: ensure to not push a reference! *)
			(mk (TBinop (OpAdd,length g p, mk (TConst (TInt 1l)) api.tint p)) api.tint p); (* ++rset_length *)
			(mk (TUnop (Increment,Prefix,length g p)) api.tint p); (* ++rset_length *)
		]) api.tint p)
	in
	let gen_remove_func (g : texpr) (instance : texpr) (p : pos) : texpr =
		let list_remove_info = get_field_access list_impl.cf_type "remove" in
		let list_remove = mk (TField (list g p, list_remove_info.cf_access)) list_remove_info.cf_type p in

		(mk (TBlock[
			(mk (TIf (
				(mk (TCall (list_remove, [instance])) api.tbool p), (* rset.remove( _ ) *)
				(mk (TUnop (Decrement,Prefix,length g p)) api.tint p), (* --rset_length *)
				None
			)) api.tvoid p);
		]) api.tvoid p)
	in
	let gen_iter_func (ep : eprocess) (p : pos) (f : texpr -> texpr) (iterated_t : t) : texpr =
		let g = ep.ep_group in
		let ivar = alloc_var VGenerated "i" api.tint p in
		let i = mk (TLocal ivar) ivar.v_type p in
		let decrease_i = mk (TUnop (Decrement,Prefix,i)) api.tint p in
		let entity_at_i = mk (TArray (list g p,i)) iterated_t p in
		(mk (TBlock[
			mk (TVar (ivar, Some (length g p))) api.tvoid p;
			mk (TWhile (
				mk (TBinop (OpGte,decrease_i, mk (TConst (TInt 0l)) api.tint p)) api.tbool p,
				f entity_at_i,
				NormalWhile
			)) api.tvoid p
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

let gen_ecreate (into_cl : tclass) eprocesses (def_hash : int) (ec : ecreate) = 
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let p = ec.expr.epos in
	let block = ref [mk (TConst (TString ("CreateEntity " ^ (s_type (TAnon ec.ec_def))))) api.tstring p] in
	let einstance_var = alloc_var VGenerated "e" (TAnon ec.ec_def) p in
	let einstance = mk (TLocal einstance_var) einstance_var.v_type p in
	print_endline ("gen_ecreate" ^ (s_type (TAnon ec.ec_def)));
	foreach_compatible_system eprocesses
		(fun sdef -> 
			match retrieve_rset RMonolist into_cl sdef with
			| Retrieved rset -> block := (rset.gen_add ec.group einstance p) :: !block;
				print_endline ("--- gen add into " ^ (s_type (TAnon sdef)))
			| Missing _ -> ()
		)
		ec.ec_def;
	block := (mk (TVar (einstance_var, Some ec.expr)) api.tvoid p) :: !block;
	implement_uexpr ec.uft (mk (TBlock !block) api.tstring p) true;
	()

let gen_edelete (into_cl : tclass) eprocesses (def_hash : int) (ed : edelete) = 
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let p = ed.expr.epos in
	let block = ref [mk (TConst (TString ("DeleteEntity " ^ (s_type (TAnon ed.ed_def))))) api.tstring p] in
	let einstance_var = alloc_var VGenerated "e" (TAnon ed.ed_def) p in
	let einstance = mk (TLocal einstance_var) einstance_var.v_type p in
	foreach_dependent_system eprocesses
		(fun sdef -> 
			match retrieve_rset RMonolist into_cl sdef with
			| Retrieved rset -> block := (rset.gen_remove ed.group einstance p) :: !block
			| Missing _ -> ()
		)
		ed.ed_def;
	block := (mk (TVar (einstance_var, Some ed.expr)) api.tvoid p) :: !block;
	implement_uexpr ed.uft (mk (TBlock !block) api.tstring p) true;
	()

let gen_eprocess (into_cl : tclass) possible_edefs (ep : eprocess) =
	let gcon = (EvalContext.get_ctx()).curapi.get_com() in
	let api = gcon.basic in
	let p = ep.ep_system.epos in
	let used = ref true in
	let gen_system_call args : texpr =
		EcsoSystemAnalyzer.make_sbb api ep.ep_system args
	in
	let rec gen_next_requirement (r_list : r list) (system_args : texpr list) : texpr =
		if List.length r_list = 0 then
			gen_system_call (List.rev system_args)
		else match List.hd r_list with
		| REntity (def,opt,t) -> 
			if contains_compatible_defs possible_edefs def then begin
				let rset = retrieve_or_gen_rset RMonolist into_cl def in
				let gen_system_block (entity : texpr) =
					gen_next_requirement (List.tl r_list) (entity :: system_args)
				in
				rset.gen_iter ep p gen_system_block t
			end else begin
				used := false;
				mk (TConst TNull) t p
			end
	in
	let impl = gen_next_requirement ep.ep_requirement [] in
	let impl =
		let impl = { impl with eexpr = TMeta((Meta.Custom("$ecso.sbb2"), [], impl.epos), impl) } in
		if !used then
			mk
				(TBlock[
					impl;
					Builder.make_string api "ProcessSystem" p;
				])
				api.tstring
				p
		else
			Builder.make_string api ("ProcessSystem (skipped)") p
	in
	implement_uexpr ep.ep_uft impl false

type global_analyze_info = {
	gi_sbb_fields : tclass_field DynArray.t; (* Fields containing system blocks *)
	gi_sbb_class_inits : tclass DynArray.t; (* Class inits containing system blocks *)
}

let create_global_analyze_info () = 
	{
		gi_sbb_fields = DynArray.create();
		gi_sbb_class_inits = DynArray.create();
	}

class plugin =
	object (self)

		val mutable need_init = (true)

		(* Analyse *)

		val mutable ecso_entity_group = (None : tclass option)
		method analyze_types gi com types =
			List.iter (self#analyse_type gi) types

		method analyse_type gi t =
			match t with
				| TClassDecl cl -> 
					let s_cl_path  = Globals.s_type_path cl.cl_path in
					(match s_cl_path with
						| "ecso.EntityGroup" -> (ecso_entity_group <- Some cl)
						| _ ->
							let analyze_cf = self#analyse_field gi cl in
							List.iter (fun cf -> analyze_cf cf true) cl.cl_ordered_statics;
							List.iter (fun cf -> analyze_cf cf false) cl.cl_ordered_fields;
							List.iter (fun cf -> analyze_cf cf false) cl.cl_overrides;
							begin match cl.cl_constructor with
								| Some cf -> analyze_cf cf false
								| None -> ()
							end;
					);
					()
				| TAbstractDecl a ->
					let s_a_path  = Globals.s_type_path a.a_path in
					(match s_a_path with
						| _ -> ()
					)
				| _ -> ()

		method analyse_field gi cl cf is_static =
			let unique_arg_counter = ref 0 in
			(* 
				Add tags to resolve in a reliable way all system processing after the analyze.
			*)
			let rec pre_transform (e : texpr) (transformed : bool ref) (has_sp : bool ref) : texpr =
				let pre_transform e = pre_transform e transformed has_sp in
				let pre_transform_arg i e =
					let i = !unique_arg_counter in
					unique_arg_counter := i + 1;
					{ e with eexpr = TMeta((Meta.Custom("$ecso.uft"), [EConst(Int (string_of_int i)),e.epos], e.epos), pre_transform e) }
				in
				map_expr
					(fun e -> match e.eexpr with
						| TCall(fident,args) ->
							begin match fident.eexpr with
							| TField (_, FInstance(cl, _, _)) when (string_of_path cl.cl_path) = "ecso.EntityGroup" ->
								has_sp := true;
								transformed := true;
								{ e with eexpr = TCall(pre_transform fident, List.mapi pre_transform_arg args) }
							| _ ->
								pre_transform e
							end
						| _ ->
							pre_transform e
					)
					e
			in
			match cf.cf_expr with
			| Some e ->
				let changed = ref false in
				let has_sp = ref false in
				let e = pre_transform e changed has_sp in
				if !changed then
					cf.cf_expr <- Some e;
				let has_sbb = self#analyse_texpr gi (get_uft (FTField cf)) e in
				if has_sbb then
					DynArray.add gi.gi_sbb_fields cf
			| _ ->
				()
		
		method analyse_texpr gi get_uft texpr =
			let has_sbb = ref false in
			let rec loop e = match e.eexpr with
				| TCall(fident,args) ->
					List.iter loop args;
					loop fident;
					begin match fident.eexpr with
					| TField (entity_group, FInstance(cl, _, cf)) when (string_of_path cl.cl_path) = "ecso.EntityGroup" ->
						begin match cf.cf_name with
						| "createEntity" -> 
							let arg1 = match args with | [arg] -> arg | _ -> raise Unexpected_expr in
							let uft = (get_uft arg1) in
							self#extract_ecreate uft entity_group arg1
						| "deleteEntity" ->
							let arg1 = match args with | [arg] -> arg | _ -> raise Unexpected_expr in
							let uft = (get_uft arg1) in
							self#extract_edelete uft entity_group arg1
						| "process" ->
							List.iter
								(fun arg ->
									let uft = (get_uft arg) in
									self#extract_eprocess uft entity_group arg
								)
								args;
							has_sbb := true
						| _ -> ()
						end
					| _ -> ()
					end
				| _ ->
					iter loop e
			in
			loop texpr;
			!has_sbb

		(* Extract *)

		val mutable ecreates = Hashtbl.create 0 ~random:false
		val mutable edeletes = Hashtbl.create 0 ~random:false
		val mutable eprocesses = DynArray.create()
		
		method extract_ecreate (uft : uft) (entity_group : texpr) (model : texpr) =
			match follow model.etype with
			| TAnon def ->
				Hashtbl.add ecreates (hash_tanon def) {
					expr = model;
					uft = uft;
					group = entity_group;
					e_inits = [](* inits *);
					ec_def = def;
				}
			| _ ->
				print_endline ("[ECSO] Wrong create parsing of " ^ (s_type_kind model.etype));
				raise Unhandled_component_type
		
		method extract_edelete (uft : uft) (entity_group : texpr) (instance : texpr) =
			match follow instance.etype with
			| TAnon def ->
				Hashtbl.add edeletes (hash_tanon def) {
					expr = instance;
					uft = uft;
					group = entity_group;
					ed_def = def;
				}
			| _ ->
				print_endline ("[ECSO] Wrong deletion parsing of " ^ (s_type_kind instance.etype));
				raise Unhandled_component_type

		method extract_eprocess (uft : uft) (entity_group : texpr) (system : texpr) =
			let rec skip e =
				match e.eexpr with
				| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) -> skip e1
				| TBlock el ->
					let len = List.length el in
					if len > 0 then
						skip (List.nth el (len - 1))
					else
						e
				| _ -> e
			in
			match follow (skip system).etype with
			| TFun (args, ret) ->
				DynArray.add eprocesses {
					ep_group = entity_group;
					ep_requirement = List.map make_srequirement args;
					ep_uft = uft;
					ep_system = system;
				}
			| t ->
				print_endline ("[ECSO] Wrong process parsing of " ^ s_type_kind t);
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


			(* let rsets_uniq = remove_dupplicate rset_eq raw_rsets in
			optimize_rlists rsets_uniq; *)
			let ctx = EvalContext.get_ctx() in
			let compiler = ctx.curapi in
			let com = compiler.get_com() in
			let gi = create_global_analyze_info() in

			(*
				Extract ecreate, edelete and sprocess.
			*)
			
			self#analyze_types gi com haxe_types;

			let ecso_entity_group = (match ecso_entity_group with
				| Some cl -> cl
				| None -> raise Missing_ecso_library)
			in

			let possible_edefs = (* TODO: find a better structure for this *)
				let list = Hashtbl.create (1 + (Hashtbl.length ecreates) / 4) in
				Hashtbl.iter
					(fun def_hash ec -> if not (Hashtbl.mem list def_hash) then Hashtbl.add list def_hash ec.ec_def)
					ecreates;
				list
			in
			
			(*
				Generate eprocess, this will generate used rsets (see retrieve_or_gen_rset call).
			*)
			
			DynArray.iter
				(gen_eprocess ecso_entity_group possible_edefs)
				eprocesses;
			
			(*
				Generate ecreate and edelete AFTER eprocess as they depends on it (see retrieve_rset call).
			*)

			Hashtbl.iter
				(gen_ecreate ecso_entity_group eprocesses)
				ecreates;
			
			Hashtbl.iter
				(gen_edelete ecso_entity_group eprocesses)
				edeletes;
			
			(* setup_process ecso_entity_group; *)
			setup_extern_field ecso_entity_group "process" "s" 32;
			setup_extern_field ecso_entity_group "createEntity" "e" 1;
			setup_extern_field ecso_entity_group "deleteEntity" "e" 1;


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