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
			vnull

		method run (macro : bool) (ml : module_type list) =

			let print_ctxs = false in (* FIXME *)

			let ectx = EvalContext.get_ctx() in
			let com = ectx.curapi.get_com() in
			detail_times := Common.raw_defined com "ecso-times";

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

		(*
			Deprecated.
		*)
		method register_context path fields static =
			EvalDecode.decode_string path;
			List.iter (fun v -> begin EvalDecode.decode_string v; () end) (EvalDecode.decode_array fields);
			EvalDecode.decode_bool static;
			vnull
	end
;;

let api = new plugin in

(**
	Register our plugin API.
	This code is executed upon `eval.vm.Context.loadPlugin` call.
*)
EvalStdLib.StdContext.register [
	("init", EvalEncode.vfun0 api#init);
	("registerContext", EvalEncode.vfun3 api#register_context);
]