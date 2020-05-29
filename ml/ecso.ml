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

		method run (ml : module_type list) =

			let print_ctxs = false in (* FIXME *)

			let ctx = EvalContext.get_ctx() in
			let ctxl = EcsoAnalyzer.fetch ctx ml in

			if print_ctxs then begin
				let s_ctx (actx : EcsoAnalyzer.t) =
					let t = TPrinting.s_type_kind (type_of_module_type actx.a_ctx.ctx_group.eg_t) in
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