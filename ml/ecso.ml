open Ast
open EvalValue
open Type
open Printf
open Printer
open Globals
open AnalyzerTexpr
open EcsoTypes
open EcsoAnalyzer

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

let dynarray_map_opt f arr =
	let mapped = DynArray.make(DynArray.length arr) in
	DynArray.iter
		(fun v ->
			match f v with
			| Some v -> DynArray.add mapped v;
			| None -> ()
		)
		arr;
	mapped

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

class plugin =
	object (self)

		val mutable executed = (true)

		method init () =
			executed <- false;
			let compiler = (EvalContext.get_ctx()).curapi in
				compiler.after_typing
					self#on_after_typing;
			vnull

		method on_after_typing (ml : module_type list) =
			if not executed then
				self#run ml;
			executed <- true
		
		(*
			Before alpha:
			- [X] fix all FIXME
			- [X] do all TODO
			- [ ] make real errors with correct positions
			- [ ] print only when `-D ecso-verbose` is set
			- [ ] make adding/removing component work
			- [ ] big clean-up
			- [ ] clean syntax too
			- [ ] add `gi.gi_entity_group : tclass option`
			- [ ] update the README.md file
			- [ ] move to github
			- [ ] setup haxe ci & unit tests
		*)
		method run (ml : module_type list) =

			let print_ctxs = false in (* FIXME *)

			let ctx = EvalContext.get_ctx() in
			let ctxl = EcsoAnalyzer.fetch ctx ml in

			if print_ctxs then begin
				let s_ctx (actx : EcsoAnalyzer.t) =
					let t = TPrinting.s_type_kind actx.a_ctx.ctx_group.eg_t in
					let ec = match actx.a_ctx.ctx_group.eg_create with | Some cf -> " ec(" ^ cf.cf_name ^ ")" | None -> "" in
					let ed = match actx.a_ctx.ctx_group.eg_delete with | Some cf -> " ed(" ^ cf.cf_name ^ ")" | None -> "" in
					let ef = match actx.a_ctx.ctx_group.eg_foreach with | Some cf -> " ef(" ^ cf.cf_name ^ ")" | None -> "" in
					t ^ ec ^ ed ^ ef;
				in
				print_endline "{ECSO} | Contexts:";
				print_list_br "              | " s_ctx ctxl ~cache:true;
			end;

			List.iter (fun ctx ->

				(*
					Prepare the analyzer graph.
				*)
				EcsoFilterFields.run ctx ml;

				(*
					Resolve every created archetype and every mutations.
				*)
				EcsoArchetypeAnalyzer.run ctx;

				(*
					Commit every graph changes.
				*)
				EcsoFilterFields.commit ctx;

			) ctxl;

			()

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