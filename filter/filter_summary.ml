(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *)

open Printf

open Debug
open File_util
open Opname
open Term
open Term_util
open Simple_print
open Filter_util
open Filter_type

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The summary contains information about
 *     1. included modules
 *     2. new theorems
 *     3. new terms
 *)
type summary_item =
   Rewrite of rewrite_info
 | CondRewrite of cond_rewrite_info
 | Axiom of axiom_info
 | Rule of ruleInfo
 | Opname of opnameInfo
 | MLTerm of term
 | Condition of term
 | Parent of module_path
 | Module of string * module_info
 | DForm of term list * term
 | Prec of string
 | Id of int
 | Resource of resource_info
 | InheritedResource of resource_info
 | Infix of string
   
and axiom_info = { axiom_name : string; axiom_stmt : term }
and rewrite_info = { rw_name : string; rw_redex : term; rw_contractum : term }
and cond_rewrite_info =
   { crw_name : string;
     crw_params : param list;
     crw_args : term list;
     crw_redex : term;
     crw_contractum : term
   }
and ruleInfo =
   { rule_name : string;
     rule_params : param list;
     rule_stmt : meta_term
   }
and opnameInfo = { opname_name : string; opname_term : term }
                 
and resource_info =
   { resource_name : string;
     resource_extract_type : Ast.ctyp;
     resource_improve_type : Ast.ctyp;
     resource_data_type : Ast.ctyp
   }

and param =
   ContextParam of string
 | VarParam of string
 | TermParam of term

(*
 * We keep lists of modules in a table.
 * Each module has a name, a path, and a summry.
 *)
and module_table_entry =
   { mod_name : string;
     mod_fullname : module_path;
     mod_summary : summary_item list
   }

(*
 * The database is just a list of modules.
 *)
and module_table = module_table_entry list

(*
 * For the official base, we also include the load path.
 *)
and module_base = module_table ref

(*
 * The info about a specific module is just a list of items.
 *)
and module_info = summary_item list

(************************************************************************
 * MODULE PATHS                                                         *
 ************************************************************************)

let rec string_of_path = function
   [] ->
      (* This should never happen because paths always point to something *)
      raise (EmptyModulePath "string_of_path")
 | [h] -> h
 | h::t -> h ^ "/" ^ (string_of_path t)

(*
 * Output a path to an ml file.
 *)
let output_path oport =
   let rec aux = function
      [] -> raise (EmptyModulePath "output_path")
    | [h] -> output_string oport (String.capitalize h)
    | h::t ->
         output_string oport h;
         output_string oport ".";
         aux t
   in
      aux

(************************************************************************
 * MODULE SUMMARY                                                       *
 ************************************************************************)

(*
 * Find the summary for a submodule.
 *)
let find_sub_module summary path =
   let rec walk sum = function
      [] -> sum
    | name::rest ->
         let rec search = function
            [] -> raise (CantFind (string_of_path path))
          | (Module (n, s))::_ when n = name ->
               walk s rest
          | _::t ->
               search t
         in
            search sum
   in
      walk summary path

(************************************************************************
 * ACCESS								*
 ************************************************************************)

(*
 * Find an axiom from the summary.
 *)
let find_axiom summary name =
   let test = function
      Axiom { axiom_name = n } -> n = name
    | Rule { rule_name = n } -> n = name
    | _ -> false
   in
      try Some (List_util.find summary test) with
         _ -> None

(*
 * Find a rewrite in the summary.
 *)
let find_rewrite summary name =
   let test = function
      Rewrite { rw_name = n } -> n = name
    | CondRewrite { crw_name = n } -> n = name
    | _ -> false
   in
      try Some (List_util.find summary test) with
         _ -> None

(*
 * Find a condition.
 *)
let find_mlterm summary t =
   let name = opname_of_term t in
   let test = function
      MLTerm t' -> (opname_of_term t') = name
    | _ -> false
   in
      try Some (List_util.find summary test) with
         _ -> None

(*
 * Find a condition.
 *)
let find_condition summary t =
   let name = opname_of_term t in
   let test = function
      Condition t' -> (opname_of_term t') = name
    | _ -> false
   in
      try Some (List_util.find summary test) with
         _ -> None

(*
 * Find a display form.
 *)
let find_dform summary t =
   let name = opname_of_term t in
   let test = function
      DForm (options, t') -> generalizes t t'
    | _ -> false
   in
      try Some (List_util.find summary test) with
         _ -> None

(*
 * Find a precedence.
 *)
let find_prec summary name =
   let test = function
      Prec s -> s = name
    | _ -> false
   in
      try Some (List_util.find summary test) with
         _ -> None

(*
 * Find the identifier.
 *)
let rec find_id = function
   h::t ->
      begin
         match h with
            Id i -> i
          | _ -> find_id t
      end
 | [] -> raise Not_found

(*
 * Get all the resources.
 *)
let rec get_resources = function
   (Resource x)::t ->
      x::(get_resources t)
 | (InheritedResource x)::t ->
      x::(get_resources t)
 | _::t ->
      get_resources t
 | [] -> []

(*
 * Get infix directives.
 *)
let rec get_infixes = function
   h::t ->
      begin
         match h with
            Infix s -> s::(get_infixes t)
          | _ -> get_infixes t
      end
 | [] -> []

(************************************************************************
 * CREATION/MODIFICATION						*
 ************************************************************************)

(*
 * Initial base.
 *)
let new_module_base () =  ref []

(*
 * Find a module by its pointer.
 *)
let rec find_module base info =
   let rec aux = function
      h::t ->
         if h.mod_summary == info then
            h
         else
            aux t
    | [] ->
         raise Not_found
   in
      aux !base

(*
 * Get the module name.
 *)
let module_name base info =
   let entry = find_module base info in
      entry.mod_name

let module_fullname base info =
   let entry = find_module base info in
      entry.mod_fullname

(*
 * New info struct.
 *)
let new_module_info () = []

(*
 * Coerce the info.
 *)
let info_items info = info

(*
 * Normalize all terms in the info.
 *)
let normalize_param = function
   TermParam t -> TermParam (normalize_term t)
 | p -> p

let rec normalize_info_item = function
   Rewrite { rw_name = name; rw_redex = redex; rw_contractum = con } ->
      Rewrite { rw_name = name;
                rw_redex = normalize_term redex;
                rw_contractum = normalize_term con
      }

 | CondRewrite { crw_name = name;
                 crw_params = params;
                 crw_args = args;
                 crw_redex = redex;
                 crw_contractum = con
   } ->
      CondRewrite { crw_name = name;
                    crw_params = List.map normalize_param params;
                    crw_args = List.map normalize_term args;
                    crw_redex = normalize_term redex;
                    crw_contractum = normalize_term con
      }

 | Axiom { axiom_name = name; axiom_stmt = t } ->
      Axiom { axiom_name = name; axiom_stmt = normalize_term t }

 | Rule { rule_name = name; rule_params = params; rule_stmt = t } ->
      Rule { rule_name = name;
             rule_params = List.map normalize_param params;
             rule_stmt = normalize_mterm t
      }
      
 | Opname { opname_name = name; opname_term = t } ->
      Opname { opname_name = name; opname_term = normalize_term t }
      
 | MLTerm t ->
      MLTerm (normalize_term t)
      
 | Condition t ->
      Condition (normalize_term t)
      
 | (Parent _) as p ->
      p
      
 | Module (name, info) ->
      Module (name, normalize_info info)
      
 | DForm (l, t) ->
      DForm (List.map normalize_term l, normalize_term t)
      
 | (Prec _) as p ->
      p
      
 | (Id _) as p ->
      p
      
 | (Resource _) as p ->
      p
      
 | (InheritedResource _) as p ->
      p
      
 | (Infix _) as p ->
      p

and normalize_info info =
   List.map normalize_info_item info

(*
 * Add some info to the base.
 *)
let push_module base name path info =
   base :=
      { mod_name = name;
        mod_fullname = path;
        mod_summary = info
      } :: !base

(*
 * Add a command to the info.
 *)
let add_command info item =
   item::info

(*
 * Find a module in the base.
 *)
let ignore_id = -1

let find_info base name id =
   let test { mod_name = n; mod_summary = info } =
      n = name & (id = ignore_id or find_id info = id)
   in
      try Some (List_util.find !base test).mod_summary with
	 _ -> None

(*
 * Load a module by name.
 *)
let find_module base mpath id =
   match mpath with
      [] -> raise (EmptyModulePath "find_module")
    | top::rest ->
         (* See if we already have a summary of this module *)
	 match find_info base top id with
	    None -> None
	  | Some sum -> Some (find_sub_module sum rest)

(************************************************************************
 * UTILITIES								*
 ************************************************************************)

(*
 * Extract the context var arguments.
 *)
let collect_cvars l =
   let rec aux = function
      (ContextParam v)::t -> v::(aux t)
    | _::t -> aux t
    | [] -> []
   in
      Array.of_list (aux l)

let collect_vars l =
   let rec aux = function
      (VarParam v)::t -> v::(aux t)
    | _::t -> aux t
    | [] -> []
   in
      Array.of_list (aux l)

let collect_non_vars =
   let rec aux = function
      (TermParam x)::t -> x::(aux t)
    | _::t -> aux t
    | [] -> []
   in
      aux

(************************************************************************
 * SUBTYPING                                                            *
 ************************************************************************)

(*
 * Print an error and raise an exception.
 *)
let implem_error s =
   raise (Failure s)

(*
 * List version generalizing terms.
 *)
let generalizes_list tl1 tl2 =
   List.for_all2 generalizes tl1 tl2

(*
 * Check parameter lists.
 *)
let check_params int_params imp_params =
   let check int_param imp_param =
      match int_param, imp_param with
         ContextParam _, ContextParam _
       | VarParam _, VarParam _
       | TermParam _, TermParam _ ->
            true
       | _ ->
            false
   in
      List.for_all2 check int_params imp_params

(*
 * Check that a rewrite is justified.
 *)
let check_rewrite info implem =
   let { rw_name = name; rw_redex = redex; rw_contractum = con } = info in
   let rec search = function
      [] ->
         implem_error (sprintf "Rewrite %s: not implemented" name)
    | h::t ->
         match h with
            Rewrite { rw_name = name'; rw_redex = redex'; rw_contractum = con' } ->
               if name = name' then
                  if alpha_equal redex' redex then
                     if alpha_equal con' con then
                        ()
                     else
                        implem_error (sprintf "Rewrite %s: contractum mismatch:\n%s\nshould be\n%s\n" (**)
                                         name (string_of_term con') (string_of_term con))
                  else
                     implem_error (sprintf "Rewrite %s: redex mismatch:\n%s\nshould be\n%s\n" (**)
                                      name (string_of_term redex') (string_of_term redex))
               else
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Conditions in implementation must be weaker than in the interface.
 *)
let check_cond_rewrite info implem =
   let { crw_name = name;
         crw_params = params;
         crw_args = args;
         crw_redex = redex;
         crw_contractum = contractum
       } = info
   in
   let rec search = function
      [] ->
         implem_error (sprintf "Cond_rewrite %s: not implemented" name)
    | h::t ->
         match h with
            CondRewrite { crw_name = name';
                          crw_params = params';
                          crw_args = args';
                          crw_redex = redex';
                          crw_contractum = contractum'
            } ->
               if name = name' then
                  if not (check_params params' params) then
                     implem_error (sprintf "Cond_rewrite %s: param list mismatch" name)
                  else if not (generalizes_list args' args) then
                     implem_error (sprintf "Cond_rewrite %s: arg list does not generalize" name)
                  else if not (alpha_equal redex' redex & alpha_equal contractum contractum') then
                     implem_error (sprintf "Cond_rewrite %s: specification mismatch" name)
                  else
                     ()
               else
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Axiom must be more general.
 *)
let check_axiom info implem =
   let { axiom_name = name; axiom_stmt = stmt } = info in
   let rec search = function
      [] ->
         implem_error (sprintf "Axiom %s: not implemented" name)
    | h::t ->
         match h with
            Axiom { axiom_name = name'; axiom_stmt = stmt' } ->
               if name' = name then
                  if generalizes stmt' stmt then
                     ()
                  else
                     implem_error (sprintf "Axiom %s: type mismatch" name)
               else
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Rule must be more general.
 *)
let check_rule info implem =
   let { rule_name = name; rule_params = params; rule_stmt = stmt } = info in
   let rec search = function
      [] ->
         implem_error (sprintf "Rule %s: not implemented" name)
    | h::t ->
         match h with
            Rule { rule_name = name'; rule_params = params'; rule_stmt = stmt' } ->
               if name' = name then
                  if not (check_params params' params) then
                     implem_error (sprintf "Rule %s: argument lists do not match" name)
                  else if not (meta_alpha_equal stmt' stmt) then
                     let s' = string_of_mterm stmt' in
                     let s = string_of_mterm stmt in
                        implem_error (sprintf "Rule %s: specification mismatch:\n%s\nis not equal to\n%s\n" (**)
                                         name s' s)
                  else
                     ()
               else
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Opnames must be equal.
 *)
let check_opname info implem =
   let { opname_name = name; opname_term = term } = info in
   let rec search = function
      [] ->
         implem_error (sprintf "Opname %s: not implemented" name)
    | h::t ->
         match h with
            Opname { opname_name = name'; opname_term = term' } ->
               if name' = name then
                  if alpha_equal term' term then
                     ()
                  else
                     implem_error (sprintf "Opname %s: specification mismatch" name)
               else
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * MLterms must match.
 *)
let string_of_term t =
   string_of_opname_list (dest_opname (opname_of_term t))

let check_mlterm term implem =
   let rec search = function
      [] ->
         implem_error (sprintf "MLTerm %s: not implemented" (string_of_term term))
    | h::t ->
         match h with
            MLTerm term' ->
               if not (alpha_equal term' term) then
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Coniditions must match.
 *)
let check_condition term implem =
   let rec search = function
      [] ->
         implem_error (sprintf "Condition %s: not implemented" (string_of_term term))
    | h::t ->
         match h with
            Condition term' ->
               if not (alpha_equal term' term) then
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Parent names must match.
 *)
let check_parent path implem =
   let rec search = function
      [] ->
         implem_error (sprintf "Include %s: not implemented" (string_of_path path))
    | h::t ->
         match h with
            Parent path' ->
               if path' <> path then
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Display forms.
 *)
let check_dform tags term implem =
   let rec search = function
      [] ->
         implem_error (sprintf "DForm %s: not implemented" (string_of_term term))
    | h::t ->
         match h with
            DForm (tags', term') ->
               if alpha_equal term' term then
                  if List.for_all2 alpha_equal tags' tags then
                     ()
                  else
                     implem_error (sprintf "DForm %s: tag mismatch" (string_of_term term))
               else
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Precedence declaration.
 *)
let check_prec name implem =
   let rec search = function
      [] ->
         implem_error (sprintf "Prec %s: not implemented" name)
    | h::t ->
         match h with
            Prec name' ->
               if name' <> name then
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Resource checking.
 *)
let check_resource info implem =
   let { resource_name = name } = info in
   let rec search = function
      [] ->
         implem_error (sprintf "Resource %s: not implemented" name)
    | h::t ->
         match h with
            Resource { resource_name = name' } ->
               if name' <> name then
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Resource checking.
 *)
let check_inherited_resource info implem =
   let { resource_name = name } = info in
   let rec search = function
      [] ->
         implem_error (sprintf "Resource %s: not implemented" name)
    | h::t ->
         match h with
            InheritedResource { resource_name = name' } ->
               if name' <> name then
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Infix declarations.
 *)
let check_infix name implem =
   let rec search = function
      [] ->
         implem_error (sprintf "Infix %s: not implemented" name)
    | h::t ->
         match h with
            Infix name' ->
               if name' <> name then
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Module definitions.
 *)
let rec check_module name info implem =
   let rec search = function
      [] ->
         implem_error (sprintf "Module %s: not implemented" name)
    | h::t ->
         match h with
            Module (name', info') ->
               if name' = name then
                  check_implementation info info'
               else
                  search t
          | _ ->
               search t
   in
      search implem

(*
 * Check that an item is implemented.
 *)
and check_implemented implem interf =
   match interf with
      Rewrite info ->
         check_rewrite info implem
    | CondRewrite info ->
         check_cond_rewrite info implem
    | Axiom info ->
         check_axiom info implem
    | Rule info ->
         check_rule info implem
    | Opname info ->
         check_opname info implem
    | MLTerm info ->
         check_mlterm info implem
    | Condition info ->
         check_condition info implem
    | Parent info ->
         check_parent info implem
    | Module (name, info) ->
         check_module name info implem
    | DForm (flags, term) ->
         check_dform flags term implem
    | Prec name ->
         check_prec name implem
    | Resource info ->
         check_resource info implem
    | InheritedResource info ->
         check_inherited_resource info implem
    | Infix name ->
         check_infix name implem
    | Id _ ->
         ()

(*
 * Check that the implementation satisfies the interface.
 * This means that every item in the interface must be implemented.
 *)
and check_implementation implem interf =
   List.iter (check_implemented implem) interf

(*
 * Printing.
 *)
let rec tab i =
   if i = 0 then
      ()
   else
      begin
         output_char stderr ' ';
         tab (i - 1)
      end

let eprint_info l =
   let rec print tabstop entry =
      tab tabstop;
      match entry with
         Rewrite { rw_name = name } ->
            eprintf "Rewrite: %s\n" name
       | CondRewrite { crw_name = name } ->
            eprintf "CondRewrite: %s\n" name
       | Axiom { axiom_name = name } ->
            eprintf "Axiom: %s\n" name
       | Rule { rule_name = name } ->
            eprintf "Rule: %s\n" name
       | Opname { opname_name = name } ->
            eprintf "Opname: %s\n" name
       | MLTerm t ->
            eprintf "MLTerm: %s\n" (string_of_term t)
       | Condition t ->
            eprintf "Condition: %s\n" (string_of_term t)
       | Parent path ->
            eprintf "Parent: %s\n" (string_of_path path)
       | Module (name, info) ->
            eprintf "Module: %s\n" name;
            List.iter (print (tabstop + 3)) info
       | DForm (_, t) ->
            eprintf "Dform: %s\n" (string_of_term t)
       | Prec name ->
            eprintf "PRecedence: %s\n" name
       | Resource { resource_name = name } ->
            eprintf "Resource: %s\n" name
       | InheritedResource { resource_name = name } ->
            eprintf "InheritedResource: %s\n" name
       | Infix name ->
            eprintf "Infix: %s\n" name
       | Id id ->
            eprintf "Id: 0x%08x\n" id
   in
      List.iter (print 2) l;
      flush stderr

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:50:58  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.3  1996/10/23 15:17:56  jyh
 * First working version of dT tactic.
 *
 * Revision 1.2  1996/09/25 22:51:59  jyh
 * Initial "tactical" commit.
 *
 * Revision 1.1  1996/09/02 19:43:13  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
