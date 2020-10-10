open Ast
open EvalValue
open Type
open Printf
open Printer
open Globals
open AnalyzerTexpr
open EcsoTypes
open EcsoAnalyzer

(* 
	Fix for #19.
*)
module EvalEcso = struct

	let void = EvalEncode.vfun1 (fun systems ->
		vnull
	)

	let register_fields (ctx : EvalContext.context) (path : Globals.path) (fields : string list) (static : bool) =
		let rec associate fl into =
			match fl with
			| [] ->
				into
			| [f] ->
				(f,void) :: into
			| f :: fl ->
				(f,void) :: associate fl into
		in
		let statics = if static then associate fields [] else [] in
		let members = if not static then associate fields [] else [] in
		EvalStdLib.init_fields ctx.builtins path statics members;
		()

end

class plugin =
	object (self)

		val mutable executed = (true)

		method init () =
			executed <- false;
			let ctx = EvalContext.get_ctx() in
			let compiler = ctx.curapi in
			compiler.after_typing
				self#on_after_typing;
			EvalEcso.register_fields ctx (["ecso"],"EntityGroup") [
				"foreachEntity";
				"createEntity";
				"deleteEntity";
			] false;
			vnull

		method register_context path fields static =
			let path = Path.parse_path (EvalDecode.decode_string path) in
			let fields = List.map (fun v -> EvalDecode.decode_string v) (EvalDecode.decode_array fields) in
			let static = EvalDecode.decode_bool static in
			let ctx = EvalContext.get_ctx() in
			EvalEcso.register_fields ctx path fields static;
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
	("registerContext", EvalEncode.vfun3 api#register_context);
]