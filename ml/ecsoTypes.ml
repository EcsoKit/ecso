open Type
open Ast
open Globals

(* Tools *)

let make_context_id static cl : int = 
	Hashtbl.hash (Globals.s_type_path cl.cl_path ^ ":" ^ if static then "s" else "i")

let append_field_into cl cf static =
	if static then begin
		cl.cl_statics <- PMap.add cf.cf_name cf cl.cl_statics;
		cl.cl_ordered_statics <- cf :: cl.cl_ordered_statics;
	end else begin
		cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
		cl.cl_ordered_fields <- cf :: cl.cl_ordered_fields;
	end;
	cf

let print_list_br ?(cache=false) suffix s l =
	let tbl =
		if not cache then Hashtbl.create 0
		else Hashtbl.create (List.length l)
	in
	let count x =
		if not cache then ""
		else " (" ^ string_of_int (List.length (Hashtbl.find_all tbl (s x))) ^ ")"
	in
	let l = if not cache then l else
		List.filter 
			(fun x ->
				let x = s x in
				let cached = Hashtbl.mem tbl x in
				Hashtbl.add tbl x ();
				not cached
			)
			l
	in
	List.iter (fun x -> print_endline (suffix ^ s x ^ count x)) l

let rec unwrap_explicit_null = function
	| TMono r ->
		(match r.tm_type with None -> assert false | Some t -> unwrap_explicit_null t)
	| TAbstract ({ a_path = ([],"Null") },[t]) ->
		t
	| TLazy f ->
		unwrap_explicit_null (lazy_type f)
	| TType (t,tl) ->
		unwrap_explicit_null (apply_params t.t_params tl t.t_type)
	| t ->
		t

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

let dynarray_exists f darr = 
	try
		DynArray.index_of f darr; true
	with
		| Not_found -> false

let dynarray_filter_dupplicates darr f =
	let filtered = DynArray.create() in
	DynArray.iter (fun v ->
		if not (dynarray_exists (f v) filtered) then
			DynArray.add filtered v
	) darr;
	filtered

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

(* Types *)

let s_component_type ?(skip_null=false) cf =
	let rec follow t = match t with
		| TMono r ->
			(match r.tm_type with
			| Some t -> follow t
			| _ -> t)
		| TLazy f ->
			follow (lazy_type f)
		| TType (t,tl) ->
			follow (apply_params t.t_params tl t.t_type)
		| _ -> t
	in
	let t = if skip_null then unwrap_explicit_null cf.cf_type else cf.cf_type in
	TPrinting.Printer.s_type (follow t) 

let eq_component_type ?(skip_null=false) cf1 cf2 =
	(s_component_type cf1 ~skip_null:skip_null) = (s_component_type cf2 ~skip_null:skip_null)

let does_unify_component commutative c1 c2 =
	does_unify c1.cf_type c2.cf_type && (not commutative || does_unify c2.cf_type c1.cf_type)

type archetype = {
	mutable a_components : (string, tclass_field) PMap.t;
}

let s_archetype a =
	"{ " ^ (String.concat ", " (PMap.foldi (fun name cf acc -> (Printf.sprintf "%s : %s" (name) (s_component_type cf)) :: acc) a.a_components [])) ^ " }"

let is_compatible_archetype a1 a2 =
	try 
		(* Check all fields exist before trying to unify them *)
		PMap.iter (fun name _ ->
			PMap.find name a1.a_components; ()
		) a2.a_components;
		(* Try to unify the fields *)
		PMap.iter (fun name cf2 ->
			let cf1 = PMap.find name a1.a_components in
			unify cf1.cf_type cf2.cf_type
		) a2.a_components;
		true
	with
	| Not_found | Unify_error _ -> false

let foreach_compatible_archetype debug al f a =
	List.iter
		(fun a' ->
			let eq = is_compatible_archetype a' a in
			if debug then begin
				print_endline ("_______________________________________");
				print_endline ("{ECSO} | Archetype EQ " ^ s_archetype a');
				print_endline ("       | " ^ s_archetype a);
				print_endline ("       | " ^ string_of_bool eq);
			end;
			if eq then f a'
		)
		al

let rec archetype_of_type t p =
	match fetch_type t with
	| TType (td, tparams) ->
		archetype_of_type td.t_type p
	| TAnon a ->
		{ a_components = a.a_fields; }
	| TAbstract ({ a_path = [],"Any" }, []) ->
		archetype_of_type (TAnon { a_fields = PMap.empty; a_status = ref Closed }) p
	| TDynamic _
	| TMono { tm_type = None } ->
		Error.error ("[ECSO] Cannot use " ^ TPrinting.Printer.s_type t ^ " as entity") p
	| _ ->
		Error.error "[ECSO] Cannot use non-anonymous structure as entity" p

let eq_archetype a1 a2 =
	let eq = is_compatible_archetype a1 a2 && is_compatible_archetype a2 a1 in
	eq

type mutation =
	| MutAdd of tclass_field list * tclass_field
	| MutRem of tclass_field list * int

let s_mutation mut =
	let pmap_of_fl fl = 
		let rec loop fl pm = match fl with
			| [] -> pm
			| f :: fl -> loop fl (PMap.add f.cf_name f pm)
		in loop fl PMap.empty
	in
	let s_base fl = TPrinting.Printer.s_type (TAnon { a_fields = pmap_of_fl fl; a_status = ref Closed; }) in
	match mut with
	| MutAdd (base,cf) -> "MutAdd(" ^ s_base base ^ " + " ^ cf.cf_name ^ " : " ^ s_component_type cf ^ ")"
	| MutRem (base,i) -> "MutRem(" ^ s_base base ^ " - " ^ (List.nth base i).cf_name ^ ")"

type mutation_accuracy =
	| MPresumed (* Mutations will be systematically presumed, giving bad performances for mutable entities *)
	(* | MPruned (* Mutations will be analyzed from each system implementation *) *)

type mutation_mode =
	| MCumulated (* Cumulates mutations to store components in monolitic objects. E.g: if {a,b} can mutate to {a,b,c}, they will be represented by a single type {a,b,?c} at runtime *)
	(* | MSplit *)

type identity_mode =
	| IGlobal (* Components that share names must have the same types *)
	(* | ITyped (* Components that share names are differentiated using their types *) *)

type structure_mode =
	| SAnon
	(* | SClass *)

type array_kind =
	| AArray
	(* | AList *)
	(* | ANativeArray *)

type array_mode =
	| ATightlyPacked of array_kind
	(* | WithHoles *)

type storage_mode =
	(* | SoA of array_mode *)
	| AoS of array_mode * mutation_mode * structure_mode

type entity_format =
	| FMonolist (* Produce one array of entity per archetype *)

module EcsoContext = struct

	type t = {
		ctx_group : egroup; (* The entity containter *)
		ctx_id : int;
		ctx_field_ids : string DynArray.t; (* List of field which are part of the context *)
		mutable ctx_archetypes : archetype list; (* Every possible combinason of components *)
		ctx_storage_mode : storage_mode;
		ctx_mutation_accuracy : mutation_accuracy;
		ctx_identity_mode : identity_mode;
		ctx_renaming_registry : (string, string) Hashtbl.t;
		ctx_debug_archetype_eq : bool;
		ctx_debug_gen : int;
	}

	and egroup = {
		eg_t : Type.module_type;
		eg_static : bool;
		eg_create : tclass_field option;
		eg_delete : tclass_field option;
		eg_foreach : tclass_field option;
	}

	let no_debugging = 0
	let simple_debugging = 1
	let full_debugging = 2

	let create id g = {
		ctx_id = id;
		ctx_group = g;
		ctx_field_ids = DynArray.create();
		ctx_archetypes = [];
		ctx_storage_mode = AoS ((ATightlyPacked AArray), MCumulated, SAnon);
		ctx_mutation_accuracy = MPresumed;
		ctx_identity_mode = IGlobal;
		ctx_renaming_registry = Hashtbl.create 0;
		ctx_debug_archetype_eq = false;
		ctx_debug_gen = no_debugging;
	}

	let in_context (cf : tclass_field) (ctx : t) =
		Meta.has EcsoMeta.context cf.cf_meta
		&& match Meta.get EcsoMeta.context cf.cf_meta with
		| (_,[Ast.EConst (Int (id)),_],p) when id = string_of_int ctx.ctx_id -> true
		| _ -> false
	
	let does_match_api (ctx : t) (fa : tfield_access) (api_field : tclass_field option) : bool =
		match api_field, fa with
		| Some api_field, (FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf) | FAnon cf) ->
			cf.cf_name = api_field.cf_name && in_context cf ctx
		| _ ->
			false

	let is_api_create (ctx : t) (fa : tfield_access) : bool =
		does_match_api ctx fa ctx.ctx_group.eg_create

	let is_api_delete (ctx : t) (fa : tfield_access) : bool =
		does_match_api ctx fa ctx.ctx_group.eg_delete

	let is_api_foreach (ctx : t) (fa : tfield_access) : bool =
		does_match_api ctx fa ctx.ctx_group.eg_foreach

end