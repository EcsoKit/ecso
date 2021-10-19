open Type
open Analyzer
open AnalyzerTypes
open AnalyzerTexpr
open EcsoTypes
open Globals
open Ast

let gen_if_not (ctx : EcsoContext.t) evalue1 evalue2 e : texpr =
	mk (TIf (
		mk (TBinop(OpNotEq,evalue1,evalue2)) ctx.ctx_basic.tbool e.epos, e, None
	)) ctx.ctx_basic.tvoid e.epos

let gen_if_not_null (ctx : EcsoContext.t) evalue e : texpr =
	gen_if_not ctx evalue (Builder.make_null evalue.etype e.epos) e

let rec get_annotated_fields (from : t) meta pos =
	match fetch_type from with
	| TAbstract({a_path=([],"Null")} as a,[t1]) ->
		get_annotated_fields t1 meta pos
	| TAbstract (({a_impl = Some impl} as a),params) when not (Meta.has Meta.CoreType a.a_meta) ->
		let fl = List.filter (fun cf ->
			has_class_field_flag cf CfImpl && not (has_class_field_flag cf CfEnum) && Meta.has meta cf.cf_meta
		) impl.cl_ordered_statics in
		List.map (fun cf ->
			let fa = FStatic (impl,cf) in
			let efield = mk (TField ({ eexpr = TTypeExpr (TClassDecl impl); etype = TInst (impl,[]); epos = pos; },fa)) cf.cf_type pos in
			Meta.get meta cf.cf_meta,[a.a_this],efield
		) fl
	| _ -> []

let make_callback_call (ctx : EcsoContext.t) self pos ((_,_,meta_pos),targs,efield) : texpr =
	let api = ctx.ctx_basic in
	match follow efield.etype with
	| TFun((_,_,_) :: args,ret) ->
		if List.length args == 0 then begin
			mk (TCall (efield,List.map2 (fun e t -> mk (TCast(e,None)) t e.epos) [self] targs)) api.tvoid pos
		end else
			Error.error "[ECSO] @:ecso.added must be annotated on functions without arguments" meta_pos
	| _ ->
		Error.error "[ECSO] @:ecso.added must be annotated on functions" meta_pos

let with_component_removed ?(compare=false) (ctx : EcsoContext.t) emutation ecomponent : texpr =
	let api = ctx.ctx_basic in
	let removed_callbacks = get_annotated_fields ecomponent.etype EcsoMeta.api_removed_callback ecomponent.epos in
	if List.length removed_callbacks = 0 then emutation else begin
		let vold = alloc_var VGenerated "_ECSO_old_" ecomponent.etype emutation.epos in
		add_var_flag vold VCaptured;
		let eold = mk (TVar (vold,Some ecomponent)) api.tvoid vold.v_pos in
		let lold = mk (TLocal vold) vold.v_type vold.v_pos in

		let removed_exprs = List.map (make_callback_call ctx lold emutation.epos) removed_callbacks in
		let eremoved = 
			let e = mk (TBlock removed_exprs) api.tvoid emutation.epos in
			gen_if_not_null ctx lold (
				if not compare then e else gen_if_not ctx lold ecomponent e
			)
		in
		let ends = if emutation.etype == ecomponent.etype then [ecomponent] else [] in
		mk (TBlock (eold :: emutation :: eremoved :: ends)) emutation.etype emutation.epos
	end

let with_component_added (ctx : EcsoContext.t) emutation ecomponent : texpr =
	let api = ctx.ctx_basic in
	let added_callbacks = get_annotated_fields ecomponent.etype EcsoMeta.api_added_callback ecomponent.epos in
	if List.length added_callbacks = 0 then emutation else begin
		let added_exprs = List.map (make_callback_call ctx ecomponent emutation.epos) added_callbacks in
		let eadded = gen_if_not_null ctx ecomponent (
			mk (TBlock added_exprs) api.tvoid emutation.epos
		) in
		let ends = if emutation.etype == ecomponent.etype then [ecomponent] else [] in
		let e = mk (TBlock (emutation :: eadded :: ends)) emutation.etype emutation.epos in
		e
	end

let with_component_callbacks (ctx : EcsoContext.t) emutation ecomponent_old ecomponent_new : texpr =
	let api = ctx.ctx_basic in
	with_component_added ctx (with_component_removed ctx emutation ecomponent_old ~compare:true) ecomponent_new
