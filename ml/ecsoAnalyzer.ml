module EcsoAnalyzer = struct
    open StringHelper
    open Type
    open Common
    open Analyzer
    open AnalyzerTypes
    open Globals
	open AnalyzerConfig

	let with_timer detailed s f =
		let timer = Timer.timer (if detailed then "ecso" :: s else ["ecso"]) in
		let r = f() in
		timer();
		r

	let run_on_expr actx e =
		let is_real_function = Run.there actx e in
		with_timer actx.config.detail_times ["analyze"] (fun () -> 
            Graph.infer_immediate_dominators actx.graph;
            Graph.infer_scopes actx.graph;
            Graph.infer_var_writes actx.graph;
        );
		if actx.com.debug then Graph.check_integrity actx.graph;
		Run.back_again actx is_real_function
    
    let is_removable_field (ctx : EvalContext.context) cf =
        (* Taken from Typecore.is_removable_field *)
        has_class_field_flag cf CfExtern || Meta.has Meta.Generic cf.cf_meta
        || (match cf.cf_kind with
            | Var {v_read = AccRequire (s,_)} -> true
            | Method MethMacro -> not ctx.is_macro
            | _ -> false)

	let run_on_field (ctx : EvalContext.context) config cl cf =
        let com = ctx.curapi.get_com() in
        match cf.cf_expr with
		| Some e when not (is_removable_field ctx cf) ->
			let actx = Run.create_analyzer_context com config e in
			let debug() =
				print_endline (Printf.sprintf "While analyzing %s.%s" (s_type_path cl.cl_path) cf.cf_name);
				List.iter (fun (s,e) ->
					print_endline (Printf.sprintf "<%s>" s);
					print_endline (Type.s_expr_pretty true "" false (s_type (print_context())) e);
					print_endline (Printf.sprintf "</%s>" s);
				) (List.rev actx.debug_exprs);
				Debug.dot_debug actx cl cf;
				print_endline (Printf.sprintf "dot graph written to %s" (String.concat "/" (Debug.get_dump_path actx cl cf)));
			in
			let e = try
				run_on_expr actx e
			with
			| Error.Error _ | Abort _ | Sys.Break as exc ->
				raise exc
			| exc ->
				debug();
				raise exc
			in
			begin match config.debug_kind with
				| DebugNone -> ()
				| DebugDot -> Debug.dot_debug actx cl cf;
				| DebugFull -> debug()
			end;
			cf.cf_expr <- Some e;
		| _ -> ()

	let run_on_class (ctx : EvalContext.context) config cl =
        let com = ctx.curapi.get_com() in
		let process_field static cf = match cf.cf_kind with
			| Var _ when not static -> ()
			| _ -> run_on_field ctx config cl cf
		in
		List.iter (process_field false) cl.cl_ordered_fields;
		List.iter (process_field true) cl.cl_ordered_statics;
		begin match cl.cl_constructor with
			| None -> ()
			| Some f -> process_field false f;
		end;
		begin match cl.cl_init with
			| None ->
				()
			| Some e ->
				let tf = { tf_args = []; tf_type = e.etype; tf_expr = e; } in
				let e = mk (TFunction tf) (tfun [] e.etype) e.epos in
				let actx = Run.create_analyzer_context com {config with optimize = false} e in
				let e = run_on_expr actx e in
				let e = match e.eexpr with
					| TFunction tf -> tf.tf_expr
					| _ -> assert false
				in
				cl.cl_init <- Some e
		end

	let run_on_type (ctx : EvalContext.context) config t =
		match t with
		| TClassDecl cl -> run_on_class ctx config cl
		| TEnumDecl _ -> ()
		| TTypeDecl _ -> ()
		| TAbstractDecl _ -> ()

	let run_on_types (ctx : EvalContext.context) types =
		let com = ctx.curapi.get_com() in
        let config = { (get_base_config com) with 
            optimize = false;
            const_propagation = false;
            copy_propagation = false;
            local_dce = false;
            purity_inference = false;
            fusion = true;
        } in
		with_timer config.detail_times ["other"] (fun () ->
			List.iter (run_on_type ctx config) types
		)
end