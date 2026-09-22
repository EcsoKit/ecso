(* ecsoConstraints.ml - Custom type constraint handling for ecso.Entity *)

open Ast
open Type
open Common
open Globals
open TUnification
open EcsoMeta
open EcsoTypes

module EcsoConstraints = struct
    
    (* Store original unify functions *)
    let original_unify = ref None
    let original_cast_or_unify_raise = ref None
    let original_monomorph_bind_ref = ref None
    
    (* Check if abstract is ecso.Entity *)
    let is_entity_abstract (a : tabstract) : bool =
        Meta.has entity_constraint a.a_meta
    
    (* Custom unification handler for Entity *)
    let custom_unify (uctx : unification_context) (t1 : t) (t2 : t) : unit =
        let t1_followed = follow t1 in
        let t2_followed = follow t2 in
        
        (* Check if either type is Entity *)
        match t1_followed, t2_followed with
        | TAnon(fields), TAbstract(a, _) when is_entity_abstract a ->
            ();
            let _ =
                match t1 with
                | TAnon _ -> ()
                | _ -> (* Typedef *)
                    print_endline (Printer.s_type t1 ^ " to " ^ Printer.s_type t2)
                    (* assert false *)
            in
            let nomatch_component =
                let kind = TType.Var { v_read = AccNormal; v_write = AccNormal; } in
                let t = TDynamic(None) in
                Gencommon.mk_class_field "$nomatch" t true null_pos kind []
            in
            let unify = 
                match !original_unify with
                | Some f -> f uctx
                | None -> (!Type.unify_ref) uctx
            in 
            (* unify t1 (TAnon([])) *)
            ()
        (* 
            | _, TAbstract(a, _) when is_entity_abstract a ->
                raise (Unify_error [Cannot_unify (t1, t2)])
            | TAbstract(a, _), _ when is_entity_abstract a ->
                raise (Unify_error [Cannot_unify (t1, t2)])
        *)
        | _ ->
            (* Fall back to original *)
            match !original_unify with
            | Some f -> f uctx t1 t2
            | None -> (!Type.unify_ref) uctx t1 t2

    let custom_cast_or_unify_raise (ctx : Typecore.typer) ?uctx t e p : texpr =
        
        (* look for constraint ecso.Entity *)
        
        let is_anon _ = match e.eexpr with
            | TObjectDecl(fields) -> true
            | _ -> false
        in

        let casting_to_type_k = TPrinting.s_type_kind t in
        let casting_to_type = TPrinting.Printer.s_type t in 
        let expr_to_cast = TPrinting.s_expr_pretty false "   " false (Printer.s_type) e in
        let texpr_to_cast = TPrinting.Printer.s_type e.etype in

        let original _ =
            (* Fall back to original *)
            match !original_cast_or_unify_raise with
            | Some f -> f ctx ?uctx t e p
            | None -> (!Typecore.cast_or_unify_raise_ref) ctx ?uctx t e p
        in

        (* 
            check the t respect the type constraint tm if it has ecso.Entity
            fallbacks to default behavior.
            TODO:   if it has ecso.Entity, we should still invoke origin() 
                    with a way to make unification to ecso.Entity allowed
                    only after check_mono's validation.
        *)
        let check_mono t tm etype =

            (* let type_str = match tm.tm_type with | Some t -> Printer.s_type t | None -> "" in *)
            
            (* if is_anon() then
                print_endline ("cast_or_unify(TMono<"^type_str^">) " ^ casting_to_type ^ " " ^ expr_to_cast); *)
            
            let has_entity_constraint = 
                let tcl = list_filter_map (fun dc ->
                    match dc with
                    | MType (TAbstract(a, _), None) when is_entity_abstract a -> 
                        Some (snd a.a_path)
                    | _ ->
                        None
                ) tm.tm_down_constraints
                in
                if List.length tcl = 0 then None
                else Some(List.nth tcl 0)
            in

            (* Work around to implement custom non-generic type parameter checks for the constraint ecso.Entity *)
            begin match has_entity_constraint with
            | Some(entity_constraint) ->
                (* assert false; *) (* off for now TEMP to see if this error is first or after this runs:
                    specs/Constraints.hx:90: characters 24-25 : Unknown<0> : ecso.Entity has no field x
                    
                    - it is first
                    - archtype_of_type receives ecso.Entity to make an archetype
                    - the place of it is legit (so we must make it work)

                    WIP HERE:
                    Soluce 1) Entity -> Entity<Archetype> to preserve its real type
                    Soluce 2) unify {} -> Entity should result into {} 

                *)
                (* check e can be an entity archetype *)

                print_endline ("=====");
                print_endline ("check: " ^ Printer.s_type t ^ " compat with anon structure archetype");
                begin match EcsoTypes.archetype_of_type_opt [] etype e.epos with
                | Some _ ->
                    e
                | None ->
                    (* raise (Generic_Exception (("Could not determine type for parameter " ^ s), p)); *)
                    
                    print_endline ("curfield: " ^ ctx.curfield.cf_name);
                    print_endline ("entity_constraint: " ^ entity_constraint);
                    print_endline ("casting_to_type: " ^ casting_to_type);
                    print_endline ("expr_to_cast: " ^ expr_to_cast ^ " : " ^ texpr_to_cast);
                    
                    let s = Printer.s_type t in
                    let stm = entity_constraint in
                    let stype = Printer.s_type etype in
                    Error.typing_error ("[ECSO] " ^ stype ^ " should be " ^ entity_constraint ^ ":" ^ stm) p;
                    Error.typing_error ("[ECSO] Could not determine type for parameter " ^ entity_constraint) p
                end 
            | None ->
                original()
            end
        in

        (* Check if either type is Entity *)
        (* TODO: is there Haxe functions we could use to custom_cast_or_unify_raise deeply ? *)
        (* TODO: if etype is a generic type parameter, we should not raise *)
        let rec check_loop t etype = match (follow t, follow etype) with
            | (TFun (args,ret), TFun (args',ret')) ->
                (* WARNING: when looping, we should make sure to invoke original() to not deviates from Haxe vanila typing *)
                List.iter2 (fun (arg,opt,t) (arg',opt',etype) -> 
                    check_loop t etype;
                    ()
                ) args args';
                original()
            | (TMono tm, etype) ->  (* Unknown<0> e.g. in type parameters *)
                check_mono t tm etype
                
                (* List.iter (fun dc -> 
                    match dc with
                    | MMono (tm, Some(name)) -> print_endline ("MMono-" ^ name)
                    | MMono (tm, None) -> print_endline ("MMono")
                    | MField cf -> print_endline ("MField-" ^ cf.cf_name)
                    | MType (t,Some(name)) -> print_endline ("MType-"^name)
                    | MType (t,None) -> print_endline ("MType")
                    | MOpenStructure -> print_endline ("MOpenStructure")
                    | MEmptyStructure -> print_endline ("MEmptyStructure")
                ) tm.tm_down_constraints; *)

            | _ ->
                original()


                (* print_endline ("cast_or_unify("^casting_to_type_k^") " ^ casting_to_type ^ " " ^ expr_to_cast); *)
                
                (* List.iter (fun ttp ->
                    if ttp.ttp_name = "Entity" then assert false;
                    if ttp.ttp_name = "ecso.Entity" then assert false;
                    if ttp.ttp_name = "ecso_Entity" then assert false;
                    if ttp.ttp_name = "Entity<Any>" then assert false;
                    if ttp.ttp_name = "Entity<Dynamic>" then assert false;
                    if ttp.ttp_name = "ecso.Entity<Any>" then assert false;
                    if ttp.ttp_name = "ecso.Entity<Dynamic>" then assert false;
                    if ttp.ttp_name = "ecso_Entity<Any>" then assert false;
                    if ttp.ttp_name = "ecso_Entity<Dynamic>" then assert false; ()
                ) ctx.type_params; *)

                (* if is_anon() then
                    print_endline ("cast_or_unify(_____) " ^ TPrinting.Printer.s_type t ^ " " ^ TPrinting.s_expr_pretty false "   " false (Printer.s_type) e); *)
                
        in
        if has_class_field_flag ctx.curfield CfGeneric then
            original()
        else 
            check_loop t e.etype
        
    let rec custom_monomorph_bind_ref m t =
		(* Fall back to original *)
        match !original_monomorph_bind_ref with
        | Some f -> f m t
        | None -> (!Type.monomorph_bind_ref) m t
    
    (* Install custom unification hook *)
    let install () : unit =
        if !original_unify = None then begin
            original_unify := Some !Type.unify_ref;
            Type.unify_ref := custom_unify
        end;
        if !original_cast_or_unify_raise = None then begin
            original_cast_or_unify_raise := Some !Typecore.cast_or_unify_raise_ref;
            Typecore.cast_or_unify_raise_ref := custom_cast_or_unify_raise
        end;
        if !original_monomorph_bind_ref = None then begin
            original_monomorph_bind_ref := Some !Type.monomorph_bind_ref;
            Type.monomorph_bind_ref := custom_monomorph_bind_ref
        end;
        ()
        
end
