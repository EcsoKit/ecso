open Type
open Analyzer
open AnalyzerTypes
open AnalyzerTexpr
open EcsoTypes

(*
	LocalFlow is built on top of Hashtbl and provides an easy way to follow local variables and maintain a list
	of their possible values through branches.

	To ensure visibility across scopes it is assumed that every variable has a unique id.

	Internally the Hashtbl is used as follow:
		var a = 0		| 	a = [0]
		a = 1			|	a = [1]			replace last
		if (x) {		|	a = [1;1]		duplicate last (branch in)
			a = 3		|	a = [1;3]		replace last
		}				|	a = [1,3]		commit last (branch out)
		if (y) {		|	a = [1,3;1,3]	duplicate last (branch in)
			a = 4		|	a = [1,3;4]		replace last
			if (z) {	|	a = [1,3;4;4]	duplicate last (branch in)
				a = 5	|	a = [1,3;4;5]	replace last
			}			|	a = [1,3;4,5]	commit last (branch out)
		}				|	a = [1,3,4,5]	commit last (branch out)
		trace(a) 		| either 1, 3, 4 or 5
*)
module LocalFlow = struct

	module VarHashtbl = Hashtbl.Make(struct
		type t = tvar
		let equal v1 v2 = v1.v_id = v2.v_id
		let hash v = v.v_id land max_int
	end)

	module IntHashtbl = Hashtbl.Make(struct
		type t = int
		let equal v1 v2 = v1 = v2
		let hash v = v land max_int
	end)

	type 'a t = 'a list VarHashtbl.t

	let create (size : int) : 'a t = VarHashtbl.create size

	let vars (lf : 'a t) : tvar list =
		let cache = IntHashtbl.create (VarHashtbl.length lf) in
		let l = ref [] in
		VarHashtbl.iter
			(fun v lfv ->
				if not (IntHashtbl.mem cache v.v_id) then begin
					IntHashtbl.add cache v.v_id ();
					l := v :: !l
				end
			)
			lf;
		!l

	let print_debug = false

	let assign (lf : 'a t) (v : tvar) (x : 'a) : unit =
		if print_debug then print_endline ("[LocalFlow] assign:");
		let all = VarHashtbl.find_all lf v in
		VarHashtbl.replace lf v [x];
		let all2 = VarHashtbl.find_all lf v in
		if print_debug then print_endline ("                      " ^ v.v_name ^ " (from " ^ string_of_int(List.length all) ^ " to " ^ string_of_int(List.length all2) ^ ")")

	let print (lf : 'a t) : string =
		let length = string_of_int (VarHashtbl.length lf) in
		"lengh = " ^ length

	let branch_in (lf : 'a t) : 'a t =
		if print_debug then print_endline ("[LocalFlow] branch in:");
		List.iter (fun v ->
			let all = VarHashtbl.find_all lf v in
			VarHashtbl.add lf v (VarHashtbl.find lf v);
			if print_debug then begin
				let all2 = VarHashtbl.find_all lf v in
				print_endline ("                      " ^ v.v_name ^ " (from " ^ string_of_int(List.length all) ^ " to " ^ string_of_int(List.length all2) ^ ")")
			end
		) (vars lf);
		lf

	let branch_out (lf : 'a t) : 'a t =
		if print_debug then begin
			print_endline ("[LocalFlow] branch out:");
			print_endline ("                      start with " ^ print lf);
			VarHashtbl.iter (fun v lfv -> print_endline ("                            contains " ^ v.v_name ^ " with " ^ string_of_int(List.length lfv) ^ " values.")) lf;
		end;
		List.iter (fun v ->
			let all = VarHashtbl.find_all lf v in
			let xl = VarHashtbl.find lf v in
			VarHashtbl.remove lf v;
			let s = ref "" in
			begin match VarHashtbl.find_opt lf v with
			| Some xl' ->
				(* do not commit values that are physically equal with values of the parent branch *)
				let xl' = List.filter (fun x' -> not (List.exists (fun x -> x == x') xl)) xl' in
				VarHashtbl.replace lf v (xl'@xl);
				s:= !s ^ "concat " ^ string_of_int(List.length xl') ^ " to " ^ string_of_int(List.length xl)
			| None ->
				()
			end;
			if print_debug then begin
				let all2 = VarHashtbl.find_all lf v in
				print_endline ("                      " ^ v.v_name ^ " (from " ^ string_of_int(List.length all) ^ " to " ^ string_of_int(List.length all2) ^ ", " ^ !s ^ ")");		
			end
		) (vars lf);
		if print_debug then print_endline ("                      ends with " ^ print lf);
		lf
	
	let values (lf : 'a t) (v : tvar) : 'a list =
		VarHashtbl.find lf v

	(* this is currently required because capture, catch, for and argument variable are ignored *)
	let values_opt (lf : 'a t) (v : tvar) : 'a list =
		if VarHashtbl.mem lf v then 
			VarHashtbl.find lf v
		else
			[]
	
	let get_first (lf : 'a t) (v : tvar) : 'a option =
		if VarHashtbl.mem lf v then 
			match List.rev (VarHashtbl.find_all lf v) with
			| xl :: xll -> Some (List.hd xl)
			| [] ->	None
		else
			None
end

(* Generate *)

module RsetGenerator = struct

	open Ast
	open Globals
	open Type

	let hash_tanon (v : archetype) = Hashtbl.hash (TPrinting.Printer.s_type (TAnon { a_fields = v.a_components; a_status = ref Closed; }))

	type cf_impl_accessor = {
		cf_access : tfield_access; 
		cf_type : t;
	}

	let mk_cf_accessor cl cf static : cf_impl_accessor =
		if not static then {
			cf_access = FInstance (cl,[],cf);
			cf_type = cf.cf_type;
		} else {
			cf_access = FStatic (cl,cf);
			cf_type = cf.cf_type;
		}

	let get_field_access t name =
		match t with
		| TInst (cl, params) ->
			let push =
				try 
					PMap.find name cl.cl_fields
				with | Not_found ->
					Error.typing_error ("{ECSO} failed to resolve field " ^ name ^ " on " ^ s_type_kind t ^ " - please report this at https://github.com/EcsoKit/ecso/issues") cl.cl_pos
			in
			{
				cf_access = FInstance (cl, params, push);
				cf_type = push.cf_type;
			}
		| _ -> 
			print_endline ("{ECSO} unhandled get_field_access type for " ^ (s_type_kind t) ^ " - please report this at https://github.com/EcsoKit/ecso/issues");
			assert false

	type rset_kind =
		| RMonolist

	type rset_status = 
		| Retrieved of rset_impl
		| Missing of rset_build_data

	and rset_build_data =
		| BMonolist of { list_name : string; length_name : string; }

	and rset_impl = {
		gen_add : texpr -> texpr -> pos -> texpr;
		gen_remove : texpr -> texpr -> pos -> texpr;
		gen_iter : texpr -> pos -> (texpr -> texpr) -> t -> texpr;
	}

	let make_rmonolist_accessor (ctx : EcsoContext.t) (list_impl : cf_impl_accessor) (length_impl : cf_impl_accessor) : rset_impl =
		let api = ctx.ctx_basic in
		let list g p = mk (TField (g, list_impl.cf_access)) list_impl.cf_type p in
		let length g p = mk (TField (g, length_impl.cf_access)) length_impl.cf_type p in
		let gen_add_func (g : texpr) (model : texpr) (p : pos) : texpr =
			let list_push_info = get_field_access list_impl.cf_type "push" in
			let list_push = mk (TField (list g p, list_push_info.cf_access)) list_push_info.cf_type p in

			(mk (TBlock[
				(mk (TCall (list_push, [model])) api.tint p);
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
		let gen_iter_func (g : texpr) (p : pos) (f : texpr -> texpr) (iterated_t : t) : texpr =
			let ivar = alloc_var VGenerated "i" api.tint p in
			let i = mk (TLocal ivar) ivar.v_type p in
			let decrease_i = mk (TUnop (Decrement,Prefix,i)) api.tint p in
			let entity_at_i = mk (TArray (list g p,i)) iterated_t p in
			(mk (TBlock[

				(* iter on one of rset's list *)
				mk (TVar (ivar, Some (length g p))) api.tvoid p;
				mk (TWhile (
					mk (TBinop (OpGte,decrease_i, mk (TConst (TInt 0l)) api.tint p)) api.tbool p,
					f entity_at_i,
					NormalWhile
				)) api.tvoid p

				(* iter here on additional rset's lists if needed *)

			]) api.tvoid p)
		in
		{
			gen_add = gen_add_func;
			gen_remove = gen_remove_func;
			gen_iter = gen_iter_func;
		}
	
	let get_class (ctx : EcsoContext.t) =
		match ctx.ctx_group.eg_t with
		| TClassDecl cl -> cl,(if ctx.ctx_group.eg_static then cl.cl_statics else cl.cl_fields)
		| _ ->
			print_endline ("{ECSO} invalid entity group instance - please report this at https://github.com/EcsoKit/ecso/issues");
			assert false

	let retrieve_rset (kind : rset_kind) (ctx : EcsoContext.t) (def : archetype) : rset_status =
		let cl,fl = get_class ctx in
		let api = ctx.ctx_basic in
		match kind with
		| RMonolist ->
			let rset_id = string_of_int ctx.ctx_id ^ "_" ^ string_of_int (hash_tanon def) in
			let list_name = "slist_" ^ rset_id in
			let length_name = "slength_" ^ rset_id in
			let list_impl =
				try Some (mk_cf_accessor cl (PMap.find list_name fl) ctx.ctx_group.eg_static)
				with Not_found -> None
			in
			let length_impl =
				try Some (mk_cf_accessor cl (PMap.find length_name fl) ctx.ctx_group.eg_static)
				with Not_found -> None
			in
			begin match list_impl, length_impl with
			| Some list_impl, Some length_impl ->
				Retrieved (make_rmonolist_accessor ctx list_impl length_impl)
			| _ ->
				Missing (BMonolist {
					list_name = list_name;
					length_name = length_name;
				})
			end

	let gen_rset (bdata : rset_build_data) (ctx : EcsoContext.t) (def : archetype) : rset_impl =
		let cl,_ = get_class ctx in
		match bdata with
		| BMonolist impl_data ->
			let api = ctx.ctx_basic in
			let list_impl =
				let t = api.tarray (TAnon { a_fields = def.a_components; a_status = ref Closed; }) in
				let pos = { pfile = "ecso.gen.RList"; pmin = 1; pmax = 4; } in
				let kind = Var { v_read = AccNormal; v_write = AccNormal; } in
				let cf_list = Gencommon.mk_class_field impl_data.list_name t true pos kind [] in (* name type public pos kind params *)
				cf_list.cf_expr <- Some (mk (TArrayDecl []) cf_list.cf_type cf_list.cf_pos);
				append_field_into cl cf_list ctx.ctx_group.eg_static;
				mk_cf_accessor cl cf_list ctx.ctx_group.eg_static
			in
			let length_impl =
				let pos = { pfile = "ecso.gen.RLength"; pmin = 1; pmax = 4; } in
				let kind = Var { v_read = AccNormal; v_write = AccNormal; } in
				let cf_length = Gencommon.mk_class_field impl_data.length_name api.tint true pos kind [] in (* name type public pos kind params *)
				cf_length.cf_expr <- Some (mk (TConst (TInt 0l)) api.tint cf_length.cf_pos);
				append_field_into cl cf_length ctx.ctx_group.eg_static;
				mk_cf_accessor cl cf_length ctx.ctx_group.eg_static
			in
			match retrieve_rset RMonolist ctx def with
			| Retrieved impl -> impl
			| Missing _ ->
				print_endline "{ECSO} could not generate correctly - please report this at https://github.com/EcsoKit/ecso/issues";
				assert false
		
	let retrieve_or_gen_rset (kind : rset_kind) (ctx : EcsoContext.t) (def : archetype) : rset_impl =
		match retrieve_rset kind ctx def with
		| Retrieved impl -> impl
		| Missing build_data ->
			gen_rset build_data ctx def

end

(* Graph *)

module EcsoGraph = struct

	open Globals
	open Ast

	type graph_info = {
		gr_expr : gexpr; (* Representation of a texpr *)
		gr_extra : (tclass * tclass_field * gexpr) DynArray.t; (* Additional expressions *)
	}

	and gexpr = {
		mutable greal : texpr;
		mutable gexpr : gexpr_expr;
	}

	and srequirement =
		| SREntity of tvar * archetype
	
	and runtime_rstatus =
		| RBoolStatus
		(* | RBitStatus One bitmask for each 16, 32 or 64 components regardless of the amout of requirement *)
	
	and s = {
		s_requirements : srequirement list; (* List of the system's arguments *)
		s_runtime_status : runtime_rstatus; (* Specifies how to track entities states at runtime *)
		s_expr : gexpr; (* Implementation of the system *)
		s_ret : t;
	}

	and assignment =
		| AInit of gexpr option
		| ANormal of Ast.binop * gexpr

	and prel = (gexpr list) ref
	
	and gexpr_value =
		| VVoid
		| VSelf
		| VBranch of gexpr list

	and flow_value = prel * gexpr option * gexpr_value (* prel, expression, value of the expression *)

	and gexpr_expr =
		| GEcsoCreate of (texpr * gexpr) * archetype  * int (* (group, obj), archetype, ctx_id *)
		| GEcsoDelete of texpr * gexpr * int (* group, entity, ctx_id *)
		| GEcsoProcess of texpr * gexpr * int (* group, system, ctx_id *)
		| GEcsoSystem of s * int
		| GEcsoMutation of tvar * tfield_access * Ast.binop * gexpr * int
		(* | GReal of texpr,gexpr *)
		(* | GVars of (tvar,gexpr option) DynArray.t *)
		(* | GBlock of gexpr DynArray.t *)
		| GReal
		| GLocal of tvar * flow_value list
		| GAssign of prel * tvar * assignment * gexpr_value
		(* | GConst of tconstant *)
		(* | GLocal of tvar *)
		| GArray of gexpr * gexpr
		| GBinop of Ast.binop * gexpr * gexpr
		| GField of gexpr * tfield_access
		(* | GTypeExpr of module_type *)
		| GParenthesis of gexpr
		| GObjectDecl of ((string * pos * quote_status) * gexpr) list
		| GArrayDecl of gexpr list
		| GCall of gexpr * gexpr list
		| GNew of tclass * tparams * gexpr list
		| GUnop of Ast.unop * Ast.unop_flag * gexpr
		| GFunction of tfunc * gexpr
		| GVar of tvar * gexpr option
		| GBlock of gexpr list
		| GFor of tvar * gexpr * gexpr
		| GIf of gexpr * gexpr * gexpr option
		| GWhile of gexpr * gexpr * Ast.while_flag
		| GSwitch of gexpr * (gexpr list * gexpr) list * gexpr option
		| GTry of gexpr * (tvar * gexpr) list
		| GReturn of gexpr option
		(* | GBreak *)
		(* | GContinue *)
		| GThrow of gexpr
		| GCast of gexpr * module_type option
		| GMeta of metadata_entry * gexpr
		| GEnumParameter of gexpr * tenum_field * int
		| GEnumIndex of gexpr
		(* | GIdent of string *)
	
	let s_gexpr_value vl : string =
		match vl with
		| VSelf -> "VSelf"
		| VBranch el -> "VBranch[" ^ TPrinting.Printer.s_list "," (fun e -> s_expr_pretty e.greal) el ^ "]"
		| VVoid -> "VVoid"
	
	let s_gexpr_kind (ge : gexpr_expr) : string =
		match ge with
		| GEcsoCreate _ -> "GEcsoCreate"
		| GEcsoDelete _ -> "GEcsoDelete"
		| GEcsoProcess _ -> "GEcsoProcess"
		| GEcsoSystem _ -> "GEcsoSystem"
		| GEcsoMutation _ -> "GEcsoMutation"
		| GReal -> "GReal"
		| GLocal _ -> "GLocal"
		| GAssign _ -> "GAssign"
		| GArray _ -> "GArray"
		| GBinop _ -> "GBinop"
		| GField _ -> "GField"
		| GParenthesis _ -> "GParenthesis"
		| GObjectDecl _ -> "GObjectDecl"
		| GArrayDecl _ -> "GArrayDecl"
		| GCall _ -> "GCall"
		| GNew _ -> "GNew"
		| GUnop _ -> "GUnop"
		| GFunction _ -> "GFunction"
		| GVar _ -> "GVar"
		| GBlock _ -> "GBlock"
		| GFor _ -> "GFor"
		| GIf _ -> "GIf"
		| GWhile _ -> "GWhile"
		| GSwitch _ -> "GSwitch"
		| GTry _ -> "GTry"
		| GReturn _ -> "GReturn"
		| GThrow _ -> "GThrow"
		| GCast _ -> "GCast"
		| GMeta _ -> "GMeta"
		| GEnumParameter _ -> "GEnumParameter"
		| GEnumIndex _ -> "GEnumIndex"

	let mk_srequirement ((v,eo) : tvar * texpr option) : srequirement =
		begin match eo with | None -> () | Some _ -> Error.typing_error "{ECSO} Optional entities are not supported yet" v.v_pos end;
		let archetype = archetype_of_type v.v_type v.v_pos in
		SREntity (v,archetype)
	
	let get_entity_context v =
		match Meta.get EcsoMeta.entity v.v_meta with
		| _,[EConst(Int (ctx_id,_)),_],_ -> int_of_string ctx_id
		| _ -> raise Not_found

	let mk_local_decl api v eo =
		{
			greal = Builder.make_null api.tvoid v.v_pos;
			gexpr = GAssign (ref [],v,AInit eo,VVoid);
		}
	let mk_local v p =
		{
			greal = Builder.make_null v.v_type p;
			gexpr = GLocal (v,[])
		}
	let mk_local_assign v e =
		{
			greal = Builder.make_null v.v_type v.v_pos;
			gexpr = GAssign (ref [],v,ANormal (OpAssign,e),VBranch[mk_local v v.v_pos]); (* intentionally forwards the local *)
		}
	
	let fusion_gexpr_value vl1 e1 vl2 e2 =
		let value_or_expr vl e = match vl with | VBranch el -> el | VSelf | VVoid -> [e] in
		match vl1, vl2 with
		| _, VVoid -> vl1
		| VSelf, _ -> VBranch(e1 :: value_or_expr vl2 e2)
		| VBranch el, _ -> VBranch(el @ value_or_expr vl2 e2)
		| VVoid, _ -> vl2
	
	let append_gexpr_value tvl vl e =
		match vl with
		| VBranch el -> tvl @ el
		| VSelf -> tvl @ [e]
		| VVoid -> tvl
	
	let cons_gexpr_value vl e tvl =
		match vl with
		| VBranch el -> el @ tvl
		| VSelf -> e :: tvl
		| VVoid -> tvl

	let rec skip e =
		match e.gexpr with
		| GParenthesis e1 | GMeta(_,e1) | GCast(e1,None) -> skip e1
		| GBlock el ->
			let len = List.length el in
			if len > 0 then
				skip (List.nth el (len - 1))
			else
				e
		| _ -> e
	
	let always_gives_null e =
		match skip e with
		| { gexpr = GReal; greal = { eexpr = TConst TNull } } -> true
		| _ -> false

	let rec always_gives_true acc e vl =
		match e.gexpr, e.greal.eexpr with
		| GReal, TConst (TBool true) -> true
		| _ -> false (* TODO: we can do better *)

	let iter (f : gexpr->unit) (g : gexpr) : unit =
		match g.gexpr with
		| GLocal _
		| GReal -> ()
		| GArray (e1,e2)
		| GBinop (_,e1,e2)
		| GFor (_,e1,e2)
		| GWhile (e1,e2,_) ->
			f e1;
			f e2;
		| GThrow e
		| GField (e,_)
		| GEnumParameter (e,_,_)
		| GEnumIndex e
		| GParenthesis e
		| GCast (e,_)
		| GUnop (_,_,e)
		| GFunction (_,e)
		| GEcsoCreate ((_,e),_,_)
		| GEcsoDelete (_,e,_)
		| GEcsoProcess (_,e,_)
		| GMeta(_,e) ->
			f e
		| GArrayDecl el
		| GNew (_,_,el)
		| GBlock el ->
			List.iter f el
		| GObjectDecl fl ->
			List.iter (fun (_,e) -> f e) fl
		| GCall (e,el) ->
			f e;
			List.iter f el
		| GVar (v,eo) ->
			(match eo with None -> () | Some e -> f e)
		| GIf (e,e1,e2) ->
			f e;
			f e1;
			(match e2 with None -> () | Some e -> f e)
		| GSwitch (e,cases,def) ->
			f e;
			List.iter (fun (el,e2) -> List.iter f el; f e2) cases;
			(match def with None -> () | Some e -> f e)
		| GTry (e,catches) ->
			f e;
			List.iter (fun (_,e) -> f e) catches
		| GReturn eo ->
			(match eo with None -> () | Some e -> f e)
		| GAssign (el,v,a,_) ->
			(* DynArray.iter f el; *)
			List.iter f !el;
			begin match a with
			| AInit (Some e) | ANormal (_,e) -> f e
			| AInit None -> ()
			end
		| GEcsoSystem (s,_) ->
			f s.s_expr
		| GEcsoMutation (_,_,_,value,_) ->
			f value

	let restore (ectx : EvalContext.context) (ctx : EcsoContext.t) (debug_buffer : (string,string)Hashtbl.t) (g : gexpr) : texpr =
		let api = ctx.ctx_basic in
		let rec f (g : gexpr) : texpr = 
			let e = g.greal in 
			let e' = match g.gexpr with
			| GReal -> e
			| GLocal (v,_) -> 
				{ e with eexpr = TLocal v; etype = v.v_type }
			| GVar (v, eo) -> 
				(* { e with eexpr = TVar (v, match eo with None -> None | Some e -> Some (f e)) } *)
				assert false
			| GAssign (prel,v,a,_) ->
				let e' = match a with
					| AInit eo ->
						{ e with eexpr = TVar (v, match eo with None -> None | Some e -> Some (f e)); etype = api.tvoid }
					| ANormal (op,e2) ->
						let e2 = f e2 in
						let e1 = { eexpr = TLocal v; epos = v.v_pos; etype = v.v_type } in
						{ e with eexpr = TBinop (op,e1,e2); etype = e1.etype }
				in
				let el = (List.map f !prel) @ [e'] in
				let t = e'.etype in
				{ e with eexpr = TBlock el; etype = t }
			(* | GConst of tconstant *)
			(* | GLocal of tvar *)
			| GArray (e1,e2) ->
				let e1 = f e1 in
				{ e with eexpr = TArray (e1,f e2) }
			| GBinop (op,e1,e2) ->
				let e1 = f e1 in
				{ e with eexpr = TBinop (op,e1,f e2) }
			| GFor (v,e1,e2) ->
				let e1 = f e1 in
				{ e with eexpr = TFor (v,e1,f e2) }
			| GWhile (e1,e2,flag) ->
				let e1 = f e1 in
				{ e with eexpr = TWhile (e1,f e2,flag) }
			| GThrow e1 ->
				{ e with eexpr = TThrow (f e1) }
			| GEnumParameter (e1,ef,i) ->
				{ e with eexpr = TEnumParameter(f e1,ef,i) }
			| GEnumIndex e1 ->
				{ e with eexpr = TEnumIndex (f e1) }
			| GField (e1,fa) ->
				{ e with eexpr = TField (f e1,fa) }
			| GParenthesis e1 ->
				{ e with eexpr = TParenthesis (f e1) }
			| GUnop (op,pre,e1) ->
				{ e with eexpr = TUnop (op,pre,f e1) }
			| GArrayDecl el ->
				{ e with eexpr = TArrayDecl (List.map f el) }
			| GNew (t,pl,el) ->
				{ e with eexpr = TNew (t,pl,List.map f el) }
			| GBlock gl ->
				let rec loop gl el = 
					match gl with
					| [] -> el
					| { gexpr = GAssign _ } as g :: gl ->
						begin match f g with
							| { eexpr = TBlock el' } -> loop gl (el'@el) (* merge block *)
							|                      e -> loop gl (e :: el)
						end
					| e :: gl ->
						loop gl (f e :: el)
				in
				let el = loop (List.rev gl) [] in
				let t = if List.length el = 0 then api.tvoid else (List.nth el (List.length el - 1)).etype in
				{ e with eexpr = TBlock el; etype = t }
			| GObjectDecl el ->
				{ e with eexpr = TObjectDecl (List.map (fun (v,e) -> v, f e) el) }
			| GCall (e1,el) ->
				let e1 = f e1 in
				{ e with eexpr = TCall (e1, List.map f el) }
			| GFunction (fu,e1) ->
				{ e with eexpr = TFunction { fu with tf_expr = f e1 } }
			| GIf (ec,e1,e2) ->
				let ec = f ec in
				let e1 = f e1 in
				{ e with eexpr = TIf (ec,e1,match e2 with None -> None | Some e -> Some (f e)) }
			| GSwitch (e1,cases,def) ->
				let e1 = f e1 in
				let cases = List.map (fun (el,e2) -> List.map f el, f e2) cases in
				{ e with eexpr = TSwitch (e1, cases, match def with None -> None | Some e -> Some (f e)) }
			| GTry (e1,catches) ->
				let e1 = f e1 in
				{ e with eexpr = TTry (e1, List.map (fun (v,e) -> v, f e) catches) }
			| GReturn eo ->
				{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)) }
			| GCast (e1,t) ->
				{ e with eexpr = TCast (f e1,t) }
			| GMeta (m,e1) ->
				{ e with eexpr = TMeta (m,f e1) }
			| GEcsoCreate ((group,e1),a,_) ->

				let a = match ctx.ctx_storage_mode with
					| AoS (_,MCumulated,_) -> begin
						try
							List.find (fun a' ->
								try PMap.iter (fun n' c' -> if PMap.mem n' a.a_components then raise Exit) a'.a_components; false
								with Exit -> true
							) ctx.ctx_archetypes	
						with
							| Not_found -> a (* Hapens when empty *)
					end					
				in

				if ctx.ctx_debug_gen >= EcsoContext.simple_debugging then begin
					Hashtbl.add debug_buffer "create-entity" (s_archetype a);
				end;

				let p = e1.greal.epos in
				let einstance_t = TAnon { a_fields = a.a_components; a_status = ref Closed } in
				let einstance_var = alloc_var VGenerated "e" einstance_t p in
				let einstance = mk (TLocal einstance_var) einstance_var.v_type p in

				let rset = RsetGenerator.retrieve_or_gen_rset RMonolist ctx a in
				{ e with eexpr = (mk (TBlock [
					(mk (TVar (einstance_var, Some (f e1))) api.tvoid p);
					PMap.fold (fun component expr ->
						let ecomponent = mk (TField (einstance, FAnon component)) component.cf_type component.cf_pos in
						EcsoCallbacks.with_component_added ctx expr ecomponent
					) a.a_components (rset.gen_add group einstance p);
					mk (TConst (TString ("CreateEntity " ^ s_archetype a))) api.tstring p
				]) api.tstring p).eexpr }
				
			| GEcsoDelete (group,instance,_) ->

				let archetype = archetype_of_type instance.greal.etype instance.greal.epos in
				let instance = f instance in

				let p = instance.epos in
				let einstance_var = alloc_var VGenerated "e" instance.etype p in
				let einstance = mk (TLocal einstance_var) einstance_var.v_type p in
				let maybe_removed_components = ref PMap.empty in
				let delete_expr =
					let block = ref [] in
					foreach_compatible_archetype ctx.ctx_debug_archetype_eq ctx.ctx_archetypes
						(fun a ->
							maybe_removed_components := pmap_append a.a_components !maybe_removed_components;
							let einstance = match ctx.ctx_storage_mode with
								| AoS (_,MCumulated,_) -> mk_cast einstance (TAnon { a_fields = a.a_components; a_status = ref Closed }) e.epos
								| _ -> einstance
							in
							match RsetGenerator.retrieve_rset RMonolist ctx a with
							| Retrieved rset ->
								block := (rset.gen_remove group einstance p) :: !block
							| Missing _ -> ()
						)
						archetype;
					(mk (TBlock !block) api.tvoid p)
				in
				{ e with eexpr = TBlock [
					(mk (TVar (einstance_var, Some instance)) api.tvoid p);
					PMap.fold (fun component expr ->
						let ecomponent = mk (TField (einstance, FAnon component)) component.cf_type component.cf_pos in
						EcsoCallbacks.with_component_removed ctx expr ecomponent
					) !maybe_removed_components delete_expr
				]; etype = api.tvoid }

			| GEcsoProcess (group,system,_) ->

				let rl = match follow (skip system).greal.etype with
					| TFun (rl,ret) -> rl
					| t -> Error.typing_error ("[ECSO] Invalid system type " ^ TPrinting.Printer.s_type t) system.greal.epos
				in

				let p = system.greal.epos in
				let used = ref true in
				let gen_system_call (args : texpr list) : texpr =
					let t = api.tvoid in
					let tcall = mk (TCall (f system, args)) t p in
					let nullsafety_off = (Meta.NullSafety, [(EConst (Ident "Off"), p)], p) in
					mk (TMeta(nullsafety_off, tcall)) t p
				in
				let rec gen_next_requirement (r_list : (string * bool * t) list) (system_args : texpr list) : texpr =
					if List.length r_list = 0 then
						gen_system_call (List.rev system_args)
					else match List.hd r_list with
					| (name,opt,t) ->
						let required_archetype = archetype_of_type t p in
						if not (ctx.ctx_identity_mode = IGlobal) then begin
							print_endline ("{ECSO} Non unique component is not supported yet.");
							(* FIXME: to make this work we would have to change
										any requirement typed as Unknown into Dynamic,
										as well as finding a solution to connect
										both x_A:A and x_B:B with the renamed `x_Dynamic`.
							*)
							assert false;
							EcsoFilters.GlobalizeNameFilter.run_archetype ctx.ctx_renaming_registry required_archetype
						end;
						let list_iterations = ref [] in
						begin foreach_compatible_archetype ctx.ctx_debug_archetype_eq ctx.ctx_archetypes
							(fun archetype ->
								if ctx.ctx_debug_gen >= EcsoContext.simple_debugging then begin
									Hashtbl.add debug_buffer "foreach-entity" (s_archetype archetype);
								end;
								let entity_type = TAnon { a_fields = archetype.a_components; a_status = ref Closed } in
								let rset = RsetGenerator.retrieve_or_gen_rset RMonolist ctx archetype in
								let gen_system_block (entity : texpr) =
									let write_rt_entity_check e : texpr =
										match ctx.ctx_storage_mode with
											| AoS (_, MCumulated, _) ->
												(*
													Generates the following (only for nullable components):
														if (e.c0 != null)
															if (e.c1 != null)
																if (e.cN != null)
																	system(e);
												*)
												let rec fold_checks cf e =
													if PMap.mem cf.cf_name required_archetype.a_components then begin
														let null_safety = false in (* FIXME *)
														let make_has_component =
															let optional = is_optional_component (PMap.find cf.cf_name required_archetype.a_components) in
															let nullable = is_nullable cf.cf_type in
															if (not null_safety || is_explicit_null cf.cf_type) && not optional && nullable then
																Builder.binop OpNotEq (Builder.field entity cf.cf_name cf.cf_type e.epos) (Builder.make_null cf.cf_type e.epos) api.tbool e.epos
															else
																Builder.make_bool api true e.epos
														in
														{ e with
															eexpr = TIf(make_has_component, e, None)
														}
													end else
														e
												in
												PMap.fold fold_checks archetype.a_components e
											| _ ->
												e
									in
									write_rt_entity_check
										(gen_next_requirement (List.tl r_list) (entity :: system_args))
								in
								let e = rset.gen_iter group p gen_system_block entity_type in
								list_iterations := (e :: !list_iterations)
							)
							required_archetype
						end;
						if (List.length !list_iterations = 0) then begin
							(* used := false; *)
							mk (TConst TNull) api.tvoid p
						end else begin
							mk (TBlock(!list_iterations)) api.tvoid p
						end
				in
				if ctx.ctx_debug_gen >= EcsoContext.full_debugging then begin
					print_endline ("              | SYSTEM " ^ s_expr_pretty system.greal);
				end;
				let impl = gen_next_requirement rl [] in
				let impl =
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
				
				if ctx.ctx_debug_gen >= EcsoContext.full_debugging then begin
					print_endline ("              | IMPL " ^ s_expr_pretty impl);
				end;

				{ e with eexpr = impl.eexpr; etype = api.tvoid }
			| GEcsoSystem (s,_) ->
				(*
					Generate a normal function, without any entity/component runtime status added.
				*)
				let arg_of_requirement r = match r with
					| SREntity (v,a) -> v,None
				in
				let signature_of_system s =
					let type_of_requirement r = match r with
						| SREntity (v,a) -> v.v_name,false,v.v_type
					in
					List.map type_of_requirement s.s_requirements,s.s_ret
				in
				{ e with
					eexpr = TFunction {
						tf_expr = f s.s_expr;
						tf_args = List.map arg_of_requirement s.s_requirements;
						tf_type = s.s_ret;
					};
					etype = TFun (signature_of_system s);
				}
			| GEcsoMutation (v,fa,op,value,_) ->
				let value = f value in
				let entity = Builder.make_local v e.epos in
				let component = mk (TField (entity,fa)) value.etype e.epos in
				let emut = { e with eexpr = TBinop(op,component,value) } in
				EcsoCallbacks.with_component_callbacks ctx emut component value
			in
			(* Check typing wasn't altered during development only *)
			if true || type_iseq e.etype e'.etype then
				e'
			else
				let pctx = print_context() in
				raise (Error.Error (Custom (
					"{ECSO} Please report the following error:\n" ^
					"Typing divergence at texpr " ^ s_expr_kind g.greal ^ " => gexpr " ^ s_gexpr_kind g.gexpr ^ " : " ^
					s_type pctx e'.etype ^ " should be " ^ s_type pctx e.etype ^ "\n" ^
					"From " ^ TPrinting.s_expr_pretty false "   " false (s_type pctx) e ^ "\n" ^
					"To " ^ TPrinting.s_expr_pretty false "   " false (s_type pctx) e'
				), e.epos, 0))
		in
		f g

	type acc_typer = {
		locals : ((gexpr list) ref * gexpr option * gexpr_value) LocalFlow.t;
	}

	(* Debug *)

	let rec s_gexpr_debug ?(tabs = "") (g : gexpr) : string =
		let f g = s_gexpr_debug g ~tabs:(tabs ^ "  ") in
		s_gexpr_kind g.gexpr ^ "(from texpr: " ^ s_expr_debug g.greal ^ ")" ^ "\n" ^ tabs ^ match g.gexpr with
		| GLocal (v,vl) -> v.v_name ^ ": " ^ string_of_int (List.length vl) ^ " branches"
		| GReal -> ""
		| GArray (e1,e2)
		| GBinop (_,e1,e2)
		| GFor (_,e1,e2)
		| GWhile (e1,e2,_) ->
			f e1 ^ "\n" ^ tabs ^ f e2
		| GThrow e
		| GField (e,_)
		| GEnumParameter (e,_,_)
		| GEnumIndex e
		| GParenthesis e
		| GCast (e,_)
		| GUnop (_,_,e)
		| GFunction (_,e)
		| GEcsoCreate ((_,e),_,_)
		| GEcsoDelete (_,e,_)
		| GEcsoProcess (_,e,_)
		| GMeta(_,e) ->
			f e
		| GArrayDecl el
		| GNew (_,_,el)
		| GBlock el ->
			List.fold_left (fun s e -> s ^ "\n" ^ tabs ^ f e ^ ";") "" el
		| GObjectDecl fl ->
			let s_field (_,e) = f e in
			List.fold_left (fun s field -> s ^ "\n" ^ tabs ^ s_field field) "" fl
		| GCall (e,el) ->
			f e ^ "\n" ^ tabs ^ List.fold_left (fun s e -> s ^ "\n" ^ tabs ^ "arg: " ^ f e) "" el
		| GVar (v,eo) ->
			(match eo with None -> "<empty>" | Some e -> f e)
		| GIf (e,e1,e2) ->
			f e ^ "\n" ^ tabs ^
			"eif:" ^ f e1 ^ "\n" ^ tabs ^
			"eelse:" ^ (match e2 with None -> "<empty>" | Some e -> f e)
		| GSwitch (e,cases,def) ->
			f e ^ "\n" ^ tabs ^
			List.fold_left (fun s (el,e2) -> s ^ "\n" ^ tabs ^ "case: " ^ 
				List.fold_left (fun s e -> s ^ "\n" ^ tabs ^ "| " ^ f e) "" el ^
				" if " ^ f e2
			) "" cases ^
			"def: " ^ (match def with None -> "<empty>" | Some e -> f e)
		| GTry (e,catches) ->
			f e ^ "\n" ^ tabs ^ List.fold_left (fun s (_,e) -> s ^ "\n" ^ tabs ^ "catch: " ^ f e) "" catches
		| GReturn eo ->
			(match eo with None -> "<void>" | Some e -> f e)
		| GAssign (el,v,a,_) ->
			(* DynArray.iter f el; *)
			v.v_name ^ " = " ^
			List.fold_left (fun s e -> s ^ "\n" ^ tabs ^ f e ) "" !el ^
			"\n" ^ tabs ^ "value: \n" ^ tabs ^ "  " ^ begin match a with
			| AInit (Some e) | ANormal (_,e) -> f e
			| AInit None -> "<empty>"
			end
		| GEcsoSystem (s,_) ->
			f s.s_expr
		| GEcsoMutation (_,_,_,value,_) ->
			f value

	let s_prel prel tabs =
		let el = !prel in
		let last = List.length el - 1 in
		let s,_ =
			(List.fold_left (fun (s,i) e -> 
				let s = 
					tabs ^ s ^ string_of_int i ^ ": " ^ s_gexpr_debug e ^ (if i = last then "" else "\n")
				in s,i+1
			) ("",0) el)
		in
		if List.length el = 0 then tabs^"<empty>" else s

	let s_flow_value (fv : flow_value) tabs =
		let prel,e,v = fv in
		let s = tabs ^ "prel : \n" ^ s_prel prel (tabs ^ "    ") in
		let s = s ^ "\n" ^ tabs ^ "expr : " ^ begin match e with
			| Some(e) -> s_gexpr_debug e
			| None -> ""
			end
		in
		let s = s ^ "\n" ^ tabs ^ "value : " ^ s_gexpr_value v in
		s
	
	let s_flow_value_list (fvl : flow_value list) tabs =
		let last = List.length fvl - 1 in
		let s,_ =
			List.fold_left (fun (s,i) fv -> 
				let s = s ^ s_flow_value fv (tabs ^ "    ") in
				let s = s ^ if i = last then "" else "\n" in
				s,(i+1)
			) ("",0) fvl
		in if List.length fvl = 0 then tabs^"<empty>" else s

	(* Graph execution *)

	let run (ctx : EcsoContext.t) com (e : texpr) : graph_info =
		let e = TexprFilter.apply com e in
		let api = com.basic in
		let extra = DynArray.create() in
		let same_branch f acc (e : texpr) = 
			f acc e
		in
		let sub_branch f acc (e : texpr) = 
			let l0 = LocalFlow.print acc.locals in
			let acc = { acc with locals = LocalFlow.branch_in acc.locals } in
			let l1 = LocalFlow.print acc.locals in
			let acc,e,vl = same_branch f acc e in
			let l2 = LocalFlow.print acc.locals in
			let acc = { acc with locals = LocalFlow.branch_out acc.locals } in
			let l3 = LocalFlow.print acc.locals in
			if LocalFlow.print_debug then print_endline ("sub-branch (before: " ^ l0 ^ ", in-pre: " ^ l1 ^ ", in-post: " ^ l2 ^ ", out: " ^ l3 ^ ")" );
			acc,e,vl
		in
		let foldmap_list f acc el =
			let rec loop acc el acc2 vll = (match el with
				| [] -> acc,(List.rev acc2),(List.rev vll)
				| e1 :: el ->
					let acc,e1,vl = f acc e1 in
					loop acc el (e1 :: acc2) (vl :: vll))
			in loop acc el [] []
		in
		let foldmap_chain f acc el =
			let rec loop acc el acc2 vll = (match el with
				| [] -> acc,(List.rev acc2),(List.rev vll)
				| e1 :: el ->
					let acc,e1,vl = f acc e1 in
					loop acc el (e1 :: acc2) (vl :: vll))
			in loop acc el [] []
		in
		let foldmap_opt branch f acc eo = match eo with
			| Some(e) -> let acc,e,vl = branch f acc e in acc,Some(e),vl
			| None    -> acc,None,VVoid
		in
		let foldmap_pairs branch f acc pairs f_left =
			let tvl = ref [] in
			let acc,pairs = List.fold_left
				(fun (acc,el) (v,e) ->
					let acc,e,vl' = branch (fun acc e -> f_left acc v; f acc e) acc e in
					begin match vl' with
					| VBranch el -> tvl := !tvl@el
					| VSelf -> tvl := !tvl@[e]
					| VVoid -> ()
					end;
					(acc,(v,e) :: el)
				)
				(acc,[])
				pairs
			in acc,(List.rev pairs),!tvl
		in
		let create_acc size : acc_typer = {
			locals = LocalFlow.create size;
		} in
		let rec f acc (e : texpr) =
			match e.eexpr with
			| TConst _
			| TTypeExpr _
			| TIdent _ ->
				let e = { greal = e; gexpr = GReal } in
				acc,e,VSelf
			| TBreak
			| TContinue ->
				acc,{ greal = e; gexpr = GReal },VVoid
			| TLocal v ->
				let e = { greal = e; gexpr = GLocal (v, LocalFlow.values_opt acc.locals v) } in
				acc,e,VSelf
			| TArray (e1,e2) ->
				let acc,e1,_ = f acc e1 in
				let acc,e2,_ = f acc e2 in
				let e = { greal = e; gexpr = GArray (e1, e2) } in
				acc,e,VSelf
			| TBinop ((OpAssign | OpAssignOp _) as op,({ eexpr = TLocal v } as e1),e2) ->
				let acc,e2,vl = f acc e2 in
				let prel = ref [] in
				LocalFlow.assign acc.locals v (prel,Some e2,vl);
				let acc,e1,_ = f acc e1 in
				let e = { greal = e; gexpr = GAssign (prel, v, ANormal(op,e2), vl) } in

				if Meta.has EcsoMeta.entity v.v_meta then
					Error.typing_error ("[ECSO] Assigning entity variables is not supported yet") e.greal.epos;
				
				acc,e,VBranch[e1] (* intentionally forwards the local *)
			| TBinop (op,e1,e2) ->
				let is_writting = match op with | OpAssign | OpAssignOp _ -> true | _ -> false in
				let is_entity e vl =
					let entity = ref false in
					let always = ref true in
					let rec loop e vl = match vl,e.gexpr with
						| VSelf, GLocal (v,_) when Meta.has EcsoMeta.entity v.v_meta ->
							entity := true
						| (VSelf | VVoid), _ ->
							always := false
						| VBranch el, _ ->
							List.iter (fun e -> loop e VSelf) el
					in
					loop e vl;
					!entity,!always
				in
				let transform_binop (e1 : texpr) f acc e2 vl2 e =
					let normal_binop e1 e2 = { greal = e; gexpr = GBinop (op,e1,e2) } in
					let unfold_entity (entity,entity_vl) e2v f =
						let transform e1v =
							match e1v.gexpr with
							| GLocal (v,_) when Meta.has EcsoMeta.entity v.v_meta ->
								f v { greal = e2v.greal; gexpr = e2v.gexpr; }
							| _ ->
								normal_binop e1v e2v
						in
						let rec unfold e l =
							match l with
							| [] -> e
							| x :: l -> unfold { e with gexpr = (transform x).gexpr } l
						in
						let e = { greal = e; gexpr = GReal } in
						match entity_vl with
						| VSelf | VVoid -> unfold e [entity]
						| VBranch el -> unfold e el
					in
					let inlined_binop e1 e2 = { greal = e; gexpr = e2.gexpr } in
					let normal_mutation entity fa value = unfold_entity entity value (fun v value -> { greal = e; gexpr = GEcsoMutation(v,fa,op,value,get_entity_context v) }) in
					let inlined_mutation e1 e2 = { greal = e; gexpr = e2.gexpr } in
					match e1.eexpr with
					| TField (fe,fa) ->
						let acc,fe,fvl = f acc fe in
						let e1 = { greal = e1; gexpr = GField (fe,fa) } in

						let is_entity,always = is_entity fe fvl in
						let e =
							if is_writting && is_entity then begin
								let entity = (fe,fvl) in
								(* Transform e2 as an entity value *)
								begin match vl2 with
								| VSelf | VVoid -> (* Nothing to do *)
									normal_mutation entity fa e2
								| VBranch el ->  (* Inline e1 *)
									List.iter
										(fun e2v ->
											e2v.gexpr <- (normal_mutation entity fa e2v).gexpr
										)
										el;
									inlined_binop e1 e2
								end
							end else begin
								normal_binop e1 e2
							end;
						in
						acc,e,VSelf
					| _ ->
						let acc,e1,vl = f acc e1 in
						acc,normal_binop e1 e2,VSelf
				in

				let acc,e2,vl2 = f acc e2 in
				let acc,e,vl = transform_binop e1 f acc e2 vl2 e in
				acc,e,VSelf
			| TFor (v,e1,e2) ->
				assert false
				(* let acc,e1,_ = f acc e1 in
				let acc = { acc with locals = LocalFlow.branch_in acc.locals } in
				(* LocalFlow.assign acc v []; *) (* ignore iteration variables *)
				let acc,e2,_ = f acc e2 in
				LocalFlow.branch_out acc.locals;
				acc,{ greal = e; gexpr = GFor (v,e1,e2) },VVoid *)
			| TWhile (e1,e2,flag) ->
				let acc,e1,vl = f acc e1 in
				let acc,e2,_ =
					if always_gives_true acc e1 vl then f acc e2
					else sub_branch f acc e2
				in
				acc,{ greal = e; gexpr = GWhile (e1,e2,flag) },VVoid
			| TThrow e1 ->
				let acc,e1,_ = f acc e1 in
				acc,{ greal = e; gexpr = GThrow (e1) },VSelf
			| TEnumParameter (e1,ef,i) ->
				let acc,e1,_ = f acc e1 in
				let e = { greal = e; gexpr = GEnumParameter(e1,ef,i) } in
				acc,e,VSelf
			| TEnumIndex e1 ->
				let acc,e1,_ = f acc e1 in
				let e = { greal = e; gexpr = GEnumIndex e1 } in
				acc,e,VSelf
			| TField (e1,fa) ->
				let acc,e1,_ = f acc e1 in
				let e = { greal = e; gexpr = GField (e1,fa) } in
				acc,e,VSelf
			| TParenthesis e1 ->
				let acc,e1,vl = f acc e1 in
				acc,{ greal = e; gexpr = GParenthesis (e1) },vl
			| TUnop (op,pre,e1) ->
				let acc,e1,_ = f acc e1 in
				acc,{ greal = e; gexpr = GUnop (op,pre,e1) },VSelf
			| TArrayDecl el ->
				let acc,el,_ = foldmap_list f acc el in
				let e = { greal = e; gexpr = GArrayDecl el } in
				acc,e,VSelf
			| TNew (t,pl,el) ->
				let acc,el,_ = foldmap_list f acc el in
				let e = { greal = e; gexpr = GNew (t,pl,el) } in
				acc,e,VSelf
			| TBlock el ->
				let acc,el,vll = foldmap_chain f acc el in
				let len = List.length vll in
				let rvalue = if len = 0 then VVoid else List.nth vll (len - 1) in
				acc,{ greal = e; gexpr = GBlock (el) },rvalue
			| TObjectDecl el ->
				let acc,el,_ = 
					foldmap_pairs
						same_branch
						f
						acc
						el
						(fun _ _ -> ())
				in
				let e = { greal = e; gexpr = GObjectDecl el } in
				acc,e,VSelf
			| TCall ({ eexpr = TField(group, fa) },[e1]) when EcsoContext.is_api_create ctx fa ->
				let acc,e1,_ = f acc e1 in
				let archetype = match (skip e1).gexpr with
					| GObjectDecl fl ->
						let a = archetype_of_type e1.greal.etype e1.greal.epos in
						let prune_dead_components cmap fl = 
							let useds = ref PMap.empty in
							List.iter (fun ((name,pos,quote),e) ->
								begin match quote with
								| NoQuotes -> ()
								| DoubleQuotes -> Error.typing_error "[ECSO] Cannot declare component with quotes" pos
								end;
								if not (always_gives_null e) then begin
									let c = (PMap.find name cmap) in
									useds := PMap.add name c !useds
								end
							) fl;
							!useds
						in
						a.a_components <- prune_dead_components a.a_components fl;
						a
					| _ ->
						let real = (skip e1).greal in
						let fname = match ctx.ctx_group.eg_create with | Some cf -> cf.cf_name in
						Error.typing_error ("[ECSO] Object declaration expected in " ^ fname ^ " function") e1.greal.epos
				in
				let e = { greal = e; gexpr = GEcsoCreate ((group,e1),archetype,ctx.ctx_id) } in
				acc,e,VSelf
			| TCall ({ eexpr = TField(group, fa) },[e1]) when EcsoContext.is_api_delete ctx fa ->
				let acc,e1,_ = f acc e1 in
				let e = { greal = e; gexpr = GEcsoDelete (group,e1,ctx.ctx_id) } in
				acc,e,VSelf
			| TCall ({ eexpr = TField(group, fa) },el) when EcsoContext.is_api_foreach ctx fa ->

				let ereal = e in

				(* Unwrap rest arguments *)
				let el = match el with
					| [e] when ExtType.is_rest e.etype ->
						let el = ref [] in
						let rec loop e = match e.eexpr with
							| TArrayDecl el' -> el := el'
							| _ -> Texpr.iter loop e
						in loop e;
						!el
					| el ->
						el
				in

				(* Explore each argument *)
				let acc,el,vll = foldmap_list f acc el in

				let make_s real tf : gexpr =
					let rl = List.map mk_srequirement tf.tf_args in
					let acc = create_acc (List.length tf.tf_args) in
					List.iter
						(fun r -> match r with
							| SREntity (v,arch) ->
								v.v_meta <- (EcsoMeta.entity,[EConst(Int (string_of_int ctx.ctx_id, None)),v.v_pos],v.v_pos) :: v.v_meta;
						)
						rl;
					let _,e,_ = f acc tf.tf_expr in
					{
						greal = real; 
						gexpr = GEcsoSystem ({
							s_requirements = rl;
							s_runtime_status = RBoolStatus;
							s_ret = tf.tf_type;
							s_expr = e;
						}, ctx.ctx_id);
					}
				in

				let mk_eprocess (system : gexpr) =
					let e = skip system in
					let p = e.greal.epos in
					let system' = match e.gexpr with
						| GField (fe,fa) ->
							let mk_anon_system cf =
								Error.typing_error ("[ECSO] Unsupported system " ^ cf.cf_name) p
							in
							let mk_field_system cf =
								let e = match cf.cf_expr with
									| Some impl ->
										begin match impl.eexpr with
											| TFunction tf ->
												make_s impl tf
											| _ ->
												Error.typing_error ("[ECSO] Unsupported system " ^ cf.cf_name) p
										end
									| None ->
										Error.typing_error "[ECSO] Cannont use system without implementation" p
								in { cf with
									cf_name = "_ECSO" ^ string_of_int ctx.ctx_id ^ "_" ^ cf.cf_name;
									cf_type = cf.cf_type;
									cf_doc = cf.cf_doc;
									cf_meta = cf.cf_meta;
									cf_kind = cf.cf_kind;
									cf_params = cf.cf_params;
									cf_expr = None;
									cf_expr_unoptimized = cf.cf_expr_unoptimized;
									cf_overloads = cf.cf_overloads;
									cf_flags = cf.cf_flags;
								},e
							in
							let mk_class_system cl cf static =
								let cf,e = mk_field_system cf in
								let fl = if static then cl.cl_statics else cl.cl_fields in
								if PMap.mem cf.cf_name fl then PMap.find cf.cf_name fl
								else begin
									let cf = append_field_into cl cf static in
									DynArray.add extra (cl,cf,e);
									cf
								end
							in
							let fa' = match fa with
								| FInstance (cl,params,cf) -> FInstance (cl, params, mk_class_system cl cf false)
								| FClosure (Some(cl,params),cf) -> FClosure (Some(cl,params), mk_class_system cl cf false)
								| FStatic (cl, cf) -> FStatic (cl, mk_class_system cl cf true)
								| FAnon cf -> FAnon (mk_anon_system cf)
								| FClosure (None,cf) -> mk_anon_system cf
								| FDynamic _ -> Error.typing_error "[ECSO] Cannot use Dynamic as system" p
								| FEnum _ -> Error.typing_error "[ECSO] Cannot use Enum as system" p
							in
							{ e with gexpr = GField (fe,fa') }
						| GFunction (tf,_) ->

							(* If we inline a function expression, we have to duplicate its locals. *)
							let tf = match duplicate_tvars e_identity e.greal with | { eexpr = TFunction tf } -> tf | _ -> assert false in
							make_s e.greal tf
						| GLocal (v,vl) ->

							let decl_prel = match LocalFlow.get_first acc.locals v with
								| Some (prel,_,_) -> prel
								| None -> Error.typing_error ("[ECSO] Cannot reach the system value of " ^ v.v_name) v.v_pos
							in

							(*
								We declare a local variable which will hold the value of each system out of `values`.
								By doing so we can preserve the user's variable.
								Step 1:
									var system = z;
									g.process( system );
								Becomes:
									var copy;
									var system = z;
									g.process( system );
							*)
							let v' =
								let extract_existing e = match e.gexpr with
									| GAssign (_,v,_,_) when Meta.has (Meta.Custom ("$ecso.v_clone" ^ string_of_int v.v_id)) v.v_meta -> true
									| _ -> false
								in match List.find_opt extract_existing !decl_prel with (* FIXME Ocaml 4.10 has find_map *)
								| Some { gexpr = GAssign (_,v',_,_) } ->
									v' (* Avoid multiple declaration per copy *)
								| None ->
									let v' = alloc_var VGenerated ("_ECSO_" ^ v.v_name ^ "_") v.v_type v.v_pos in
									v'.v_meta <- (Meta.Custom ("$ecso.v_clone" ^ string_of_int v'.v_id),[],v.v_pos) :: v.v_meta;
									v'.v_extra <- v.v_extra;
									add_var_flag v' VCaptured;
									decl_prel := (mk_local_decl api v' None) :: !decl_prel;
									v'
								| _ -> assert false
							in
							
							(*
								Process the local copy instead of the user's variable, which we also move above
								the process call in order to still be executed.
								Step 2:
									var copy;
									var system = z;
									g.process( system );
								Becomes:
									var copy;
									var system = z;
									g.process( { system; copy; } );
							*)
							(* let e = { e with gexpr = GBlock[{ e with gexpr = e.gexpr }] } in *)

							(*
								Assign the local copy everywhere the user's variable is being assigned a value.
								Step 3:
									var copy;
									var system = z;
									g.process( { system; copy; } );
								Becomes:
									var copy;
									var system = { copy = transformed_z; z; }
									g.process( { system; copy; } );
							*)
							let rec loop_fv (fv : flow_value) = match fv with prel,e,gv ->
								let assign_copy e =

									let eassign =
										let e' = 
											(* make_s e.gexpr function *)
											begin match e.gexpr with
												| GFunction (tf,_) ->
													(* If we inline a function expression, we have to duplicate its locals. *)
													let tf = match duplicate_tvars e_identity e.greal with | { eexpr = TFunction tf } -> tf | _ -> assert false in
													make_s e.greal tf
												| _ ->
													Error.typing_error ("{ECSO} failed to parse this system") p;
											end
										in
										mk_local_assign v' { e with gexpr = e'.gexpr }
									in
									
									let original _ = { e with gexpr = e.gexpr } in
									e.gexpr <- GBlock[eassign ; original()]
								in
								(* This is a bit hacky, not sure it covers every cases *)
								let assign_copy_follow e =
									let e = skip e in
									match e.gexpr with
									| GLocal (v,vl) ->
										List.iter loop_fv vl
									| _ -> assign_copy e
								in
								begin match gv,e with
								| VBranch el,_ ->
									List.iter assign_copy_follow el
								| VSelf,Some e ->
									assign_copy_follow e
								| VVoid,_ ->
									()
								| VSelf,None ->
									assert false
								end
							in
							List.iter loop_fv vl;
							
							let system = { e with gexpr = e.gexpr } in
							system

						| _ ->
							assert false
					in
							
					e.gexpr <- GEcsoProcess (group,system',ctx.ctx_id);
					e.greal <- ereal;
				in

				let rec loop el vll =
					match el,vll with
					| e :: el,vl :: vll ->
						begin match vl with
						| VSelf -> mk_eprocess e
						| VBranch el -> List.iter mk_eprocess el
						| VVoid -> assert false
						end;
						loop el vll
					| [],[] -> ()
					| _ -> assert false
				in loop el vll;
				
				let e = { greal = e; gexpr = GBlock el } in
				acc,e,VSelf
			| TCall (e1,el) ->
				let acc,e1,_ = f acc e1 in
				let acc,el,_ = foldmap_list f acc el in
				let e = { greal = e; gexpr = GCall (e1,el) } in
				acc,e,VSelf
			| TVar (v,eo) ->
				let acc,eo,vl = foldmap_opt same_branch f acc eo in
				let prel = ref [] in
				LocalFlow.assign acc.locals v (prel,eo,vl);
				acc,{ greal = e; gexpr = GAssign (prel,v,AInit eo,vl) },VVoid
			| TFunction fu -> (* TODO: try to be more precise by determining when fu is actually being called *)
				List.iter (fun (v,eo) ->
						let acc,eo,vl = foldmap_opt same_branch f acc eo in ()
					)
					fu.tf_args;
				let acc,e1,_ = f acc fu.tf_expr in
				let e = { greal = e; gexpr = GFunction (fu,e1) } in
				acc,e,VSelf
			| TIf (ec,e1,eo) ->
				let acc,ec,vlc = f acc ec in
				let acc,e1,vl1 = if always_gives_true acc ec vlc then f acc e1 else sub_branch f acc e1 in
				let acc,eo,vlo = foldmap_opt sub_branch f acc eo in
				let vl = match eo with | None -> vl1 | Some e2 -> fusion_gexpr_value vl1 e1 vlo e2 in
				acc,{ greal = e; gexpr = GIf (ec,e1,eo) },vl
			| TSwitch (e1,cases,def) ->
				let acc,e1,_ = f acc e1 in
				let tvl = ref [] in
				let acc,cases = List.fold_left (fun (acc,cases) (el,e2) ->
					let acc,el,_ = foldmap_list f acc el in
					let acc,e2,vl2 = sub_branch f acc e2 in
					tvl := append_gexpr_value !tvl vl2 e2;
					acc,((el,e2) :: cases)
				) (acc,[]) cases in
				let acc,def,vldef = foldmap_opt sub_branch f acc def in
				let vl = match def with | None -> VBranch !tvl | Some e -> VBranch (append_gexpr_value !tvl vldef e) in
				acc,{ greal = e; gexpr = GSwitch (e1, cases, def) },vl
			| TTry (e1,catches) ->
				let acc,e1,vl1 = sub_branch f acc e1 in
				let acc,catches,tvl2 = foldmap_pairs sub_branch f acc catches (fun acc v -> ()(* LocalFlow.assign acc v [] *)) in (* ignore catch variables *)
				acc,{ greal = e; gexpr = GTry (e1, catches) },VBranch(cons_gexpr_value vl1 e1 tvl2)
			| TReturn eo ->
				let acc,eo,_ = foldmap_opt same_branch f acc eo in
				acc,{ greal = e; gexpr = GReturn eo },VVoid
			| TCast (e1,t) ->
				let acc,e1,vl = f acc e1 in
				acc,{ greal = e; gexpr = GCast (e1,t) },vl
			| TMeta (m,e1) ->
				let acc,e1,vl = f acc e1 in
				acc,{ greal = e; gexpr = GMeta(m,e1)},vl
		in
		let _,e,_ = f (create_acc 0) e in
		{
			gr_expr = e;
			gr_extra = extra;
		}

end
