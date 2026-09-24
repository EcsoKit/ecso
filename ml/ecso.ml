open Ast
open EvalValue
open Type
open Printf
open Printer
open Globals
open AnalyzerTexpr
open EcsoTypes
open EcsoAnalyzer

class plugin =
	object (self)

		method init () =
			let ectx = EvalContext.get_ctx() in
			ectx.curapi.after_typing (self#run false);
			let com = ectx.curapi.get_com() in
			begin match com.get_macros() with
			| Some mctx ->
				mctx.callbacks#add_after_typing (self#run true)
			| None -> 
				()
			end;
			(* 
				Register extern functions to eval to allow compilation of normal context
				with dead code using ecso when the macro context is loaded (see issue #34).
			*)
			EvalStdLib.init_fields ectx.builtins (["ecso"],"EntityGroup") [] [
				("foreachEntity", EvalEncode.vfun1 (fun arg -> vnull ));
				("createEntity", EvalEncode.vfun1 (fun arg -> vnull ));
				("deleteEntity", EvalEncode.vfun1 (fun arg -> vnull ));
			];
			vnull

		method run (macro : bool) (ml : module_type list) =

			let print_ctxs = false in (* FIXME *)

			let ectx = EvalContext.get_ctx() in
			let com = ectx.curapi.get_com() in
			detail_times := Common.raw_defined com "ecso-times";

			(*
				When running on the macro context, only consider modules that don't
				exist in the normal context: modules shared by both are owned by the
				normal context, where the code is actually executed (see issue #34).
			*)
			let ml =
				if macro then
					List.filter (fun mt -> not (com.module_lut#mem (t_infos mt).mt_module.m_path)) ml
				else
					ml
			in

			let ctxl = with_timer ["fetch-contexts"] (fun () -> EcsoAnalyzer.fetch ectx macro ml) in

			if print_ctxs then begin
				let s_ctx (actx : EcsoAnalyzer.t) =
					let t = TPrinting.s_type_kind (type_of_module_type actx.a_ctx.ctx_group.eg_t) in
					let ec = match actx.a_ctx.ctx_group.eg_create with | Some cf -> " ec(" ^ cf.cf_name ^ ")" | None -> "" in
					let ed = match actx.a_ctx.ctx_group.eg_delete with | Some cf -> " ed(" ^ cf.cf_name ^ ")" | None -> "" in
					let ef = match actx.a_ctx.ctx_group.eg_foreach with | Some cf -> " ef(" ^ cf.cf_name ^ ")" | None -> "" in
					t ^ ec ^ ed ^ ef;
				in
				print_endline ("{ECSO} | Contexts" ^ if macro then " (macro):" else ":");
				print_list_br "              | " s_ctx ctxl ~cache:true;
			end;

			List.iter (fun ctx ->

				(*
					Prepare the analyzer graph.
				*)
				with_timer ["filter"] (fun () -> EcsoFilterFields.run ctx ml);

				(*
					Resolve every created archetype and every mutations.
				*)
				with_timer ["analyzer"] (fun () -> EcsoArchetypeAnalyzer.run ctx);

				(*
					Commit every graph changes.
				*)
				with_timer ["commit"] (fun () -> EcsoFilterFields.commit ctx);

			) ctxl;

			if print_ctxs then
				print_endline "{ECSO} | Done"
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