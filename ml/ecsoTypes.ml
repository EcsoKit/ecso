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

let dynarray_filter_dupplicates_into farr darr eq =
	DynArray.iter (fun v ->
		if not (dynarray_exists (eq v) farr) then
			DynArray.add farr v
	) darr

let dynarray_filter_dupplicates darr eq =
	let filtered = DynArray.create() in
	dynarray_filter_dupplicates_into filtered darr eq;
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

let detail_times = ref false

let with_timer s f =
	let timer = Timer.timer (if !detail_times then "ecso" :: s else ["ecso"]) in
	let r = f() in
	timer();
	r

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

let is_optional_component cf : bool =
	is_explicit_null cf.cf_type

type archetype = {
	mutable a_components : (string, tclass_field) PMap.t;
}

let s_archetype a =
	"{ " ^ (String.concat ", " (PMap.foldi (fun name cf acc -> (Printf.sprintf "%s : %s" (name) (s_component_type cf)) :: acc) a.a_components [])) ^ " }"

let hash_archetype a =
	let string_of_component cf = (if is_optional_component cf then "?" else "") ^ cf.cf_name ^ ":" ^ s_component_type cf in
	let hl = List.sort compare (PMap.fold (fun cf acc -> Hashtbl.hash (string_of_component cf) :: acc) a.a_components []) in
	Hashtbl.hash (String.concat "-" (List.map string_of_int hl))
	(* Hashtbl.hash hl *)

let unify_archetype a1 a2 =
	try 
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
			let eq = unify_archetype a' a in
			if debug then begin
				print_endline ("_______________________________________");
				print_endline ("{ECSO} | Archetype match : " ^ string_of_bool eq);
				print_endline ("       |      " ^ s_archetype a');
				print_endline ("       | with " ^ s_archetype a);
			end;
			if eq then f a'
		)
		al

let mk_archetype (components : (string, tclass_field) PMap.t) : archetype =
	{ a_components = components; }

let rec archetype_of_type t p =
	match fetch_type t with
	| TType (td, tparams) ->
		archetype_of_type td.t_type p
	| TAnon a ->
		mk_archetype a.a_fields
	| TDynamic _
	| TMono { tm_type = None }
	| TAbstract ({ a_path = [],"Any" }, []) ->
		archetype_of_type (TAnon { a_fields = PMap.empty; a_status = ref Closed }) p
	| _ ->
		Error.error "[ECSO] Cannot use non-anonymous structure as entity" p

let eq_archetype a1 a2 =
	let rec loop fields1 fields2 =
		match fields1, fields2 with
		| f1 :: rest1, f2 :: rest2 ->
			f1.cf_name = f2.cf_name
			&& (try shallow_eq f1.cf_type f2.cf_type with Not_found -> false)
			&& loop rest1 rest2
		| [], [] -> true
	in
	let fields1 = PMap.fold (fun field fields -> field :: fields) a1.a_components []
	and fields2 = PMap.fold (fun field fields -> field :: fields) a2.a_components []
	and sort_compare f1 f2 = compare f1.cf_name f2.cf_name in
	a1.a_components == a2.a_components ||
	List.length fields1 = List.length fields2 &&
	loop (List.sort sort_compare fields1) (List.sort sort_compare fields2)

type mutation =
	| MutAdd of tclass_field list * tclass_field
	| MutRem of tclass_field list * int

type related_mutation = {
	rm_evolutions : mutation_value list;
	rm_base : tclass_field list;
}
and mutation_value =
	| MutValueAdd of tclass_field
	| MutValueRem of int

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

let eq_mutation m1 m2 =
	match m1,m2 with
	| MutAdd(base1,cf1), MutAdd(base2,cf2) -> cf1.cf_name = cf2.cf_name && s_mutation m1 = s_mutation m2
	| MutRem(base1,i1), MutRem(base2,i2) -> i1 = i2 && s_mutation m1 = s_mutation m2
	| _,_ -> false

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

module ChainTbl = struct

	type 'a t = {
		chain_starts : ('a chain_node) list;
		indexor : 'a indexor;
	}

	and 'a indexor = ((string, 'a chain_node) PMap.t) ref

	and 'a chain_node = {
		key : string;
		item : 'a;
		mutable next : 'a chain_node;
		mutable size : int ref; (* amount of chained nodes for transversal iterations *)
	}

	let transverse_chain (f : 'a -> unit) (from : 'a chain_node) = 
		let rec loop node remaining nl =
			match remaining with 
			| 0 -> ()
			| _ ->
				f node.item;
				loop node.next (remaining - 1) nl
		in
		loop from !(from.size) []

	let transverse_starts (f : 'a chain_node -> unit) (c : 'a t) =
		let rec loop nl = match nl with | [] -> () | n :: nl -> 
			f n;
			loop nl
		in
		loop c.chain_starts

	let map_starts (f : 'a -> 'x) (c : 'a t) : 'x list =
		let list = ref [] in
		transverse_starts (fun v -> list := (f v.item) :: !list) c;
		!list
	
	(*
		`list_of_chain chain` is `0; 1; ...; (len-1)`.
	*)
	let list_of_chain (chain : 'a chain_node) : (string * 'a) list = 
		let rec loop node remaining nl =
			match remaining with
			| 0 -> nl
			| _ ->
				(node.key,node.item) :: loop node.next (remaining - 1) nl
		in
		loop chain !(chain.size) []
	
	let s_chain (chain : 'a chain_node) =
		let nl = list_of_chain chain in
		let s = TPrinting.Printer.s_list "-" (fun (k,_) -> k) nl in
		s ^ ":" ^ string_of_int !(chain.size)

	let fill (m : (string,'a) PMap.t) (nl : ('a chain_node) list) (indexor : 'a indexor) =
		let fold key item (node : ('a chain_node) option) =
			let chain =
				if PMap.mem key !indexor then begin
					let node = PMap.find key !indexor in
					node
				end else begin
					let rec node = {
						key = key;
						item = item;
						next = node;
						size = ref 1;
					} in
					indexor := PMap.add key node !indexor;
					node
				end
			in
			match node with
			| Some node when not (node.size == chain.size) (* ref comparison *) ->
				(* connect chains *)
				let new_size = !(node.size) + !(chain.size) in
				let size_ref = node.size in
				let transversal_update (from : 'a chain_node) : 'a chain_node =
					let rec loop node remaining =
						node.size <- size_ref;
						match remaining with 
						| 1 -> node
						| _ -> loop node.next (remaining - 1)
					in
					loop from !(from.size);
				in
				let prev = transversal_update chain in

				(* chain with previous *)
				prev.next <- node.next;
				node.next <- chain;
				
				(* update size value *)
				size_ref := new_size;
				
				Some chain
			| _ ->
				(* single for now *)
				Some chain
		in
		match PMap.foldi fold m None with
			| Some node ->
				node :: nl
			| None ->
				nl (* is an empty archetypes *)

	(*
		`unload_filter f c` returns a list containing every chain of `c` such as `f c0; f c1; ...; f cN`.
		
		It is the opposite of `init_filter xl f`.
	*)
	let unload_filter (f : (string,'a) PMap.t -> 'b) (c : 'a t) : 'b list = 
		let rec loop nl acc =
			match nl with
			| n :: nl when PMap.mem n.key !(c.indexor) -> begin
				let nodes = list_of_chain n in
				let m = ref PMap.empty in
				List.iter (fun (key,item) ->
					(* De-index each chain's node *)
					c.indexor := PMap.remove key !(c.indexor);
					m := PMap.add key item !m;
				) nodes;
				loop nl (f !m :: acc)
			end
			| _ -> acc
		in
		loop c.chain_starts []
	
	let create (_ : unit) : 'a t =
		{
			indexor = ref PMap.empty;
			chain_starts = [];
		}


	(*
		`init_filter xl f` is `init (List.map f xl)`.
	*)
	let init_filter (xl : 'x list) (f : 'x -> (string, 'a) PMap.t) : 'a t =
		let rec loop l nl indexor = match l with
			| [] -> nl
			| x :: l -> loop l (fill (f x) nl indexor) indexor
		in
		let idx = ref PMap.empty in {
			indexor = idx;
			chain_starts = loop xl [] idx;
		}
	
	(*
		`init ml` is `List.fold_right add ml (create())`.
	*)
	let init (ml : ((string, 'a) PMap.t) list) : 'a t =
		init_filter ml (fun m -> m)
	
	(*
		`add m c` returns a table containing the same chains as `c`, plus the new chains produced by `m`.
		
		If an element of `m` was already chained in `c`, the elements of `m` links with the existing chain.
		Otherwise, a new chain for `m` is pruduced.
	*)
	let add (m : (string, 'a) PMap.t) (c : 'a t) : tclass_field t =
		{
			indexor = c.indexor;
			chain_starts = fill m c.chain_starts c.indexor;
		}

	(*
		`add_list_filter ml f c` is `add_list (List.map f xl) c`.
	*)
	let add_list_filter (xl : 'x list) (f : 'x -> (string, 'a) PMap.t) (c : 'a t) : 'a t =
		let rec loop l nl indexor = match l with
			| [] -> nl
			| x :: l -> loop l (fill (f x) nl indexor) indexor
		in {
			indexor = c.indexor;
			chain_starts = loop xl [] c.indexor;
		}

	(*
		`add_list ml c` is `List.fold_right add ml c`.
	*)
	let add_list (ml : ((string, 'a) PMap.t) list) (c : 'a t) : 'a t =
		add_list_filter ml (fun m -> m) c
	
end
