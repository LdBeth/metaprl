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
(*
 * The summary contains information about
 *     1. included modules
 *     2. new theorems
 *     3. new terms
 *)
type 'a summary_item =
   Rewrite of 'a rewrite_info
 | CondRewrite of 'a cond_rewrite_info
 | Axiom of 'a axiom_info
 | Rule of 'a rule_info
 | Opname of opname_info
 | MLTerm of term
 | Condition of term
 | Parent of module_path
 | Module of string * 'a module_info
 | DForm of dform_info
 | Prec of string
 | Id of int
 | Resource of resource_info
 | InheritedResource of resource_info
 | Infix of string
 | SummaryItem of term
   
and 'a rewrite_info =
   { rw_name : string;
     rw_redex : term;
     rw_contractum : term;
     rw_proof : 'a
   }
and 'a cond_rewrite_info =
   { crw_name : string;
     crw_params : param list;
     crw_args : term list;
     crw_redex : term;
     crw_contractum : term;
     crw_proof : 'a
   }
and 'a axiom_info =
   { axiom_name : string;
     axiom_stmt : term;
     axiom_proof : 'a
   }
and 'a rule_info =
   { rule_name : string;
     rule_params : param list;
     rule_stmt : meta_term;
     rule_proof : 'a
   }
and opname_info =
   { opname_name : string;
     opname_term : term
   }
and dform_info =
   { dform_options : term list;
     dform_redex : term;
     dform_def : term option
   }
and resource_info =
   { resource_name : string;
     resource_extract_type : MLast.ctyp;
     resource_improve_type : MLast.ctyp;
     resource_data_type : MLast.ctyp
   }
and param =
   ContextParam of string
 | VarParam of string
 | TermParam of term

(*
 * The info about a specific module is just a list of items.
 *)
and 'a module_info =
   { info_list : 'a summary_item list;
     info_length : int
   }

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
 * PRINTING                                                             *
 ************************************************************************)

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

(*
 * Print a single entry.
 *)
let eprint_entry print_info = function
   SummaryItem _ ->
      eprintf "SummaryItem\n"
 | Rewrite { rw_name = name } ->
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
 | Module (name, { info_list = info }) ->
      eprintf "Module: %s\n" name;
      print_info info
 | DForm { dform_redex = t } ->
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

(*
 * Non-recursive print.
 *)
let eprint_command command =
   let print_info info =
      ()
   in
      eprint_entry print_info command

(*
 * Recursive printing.
 *)
let eprint_info { info_list = l } =
   let rec print tabstop entry =
      let print_info info =
         List.iter (print (tabstop + 3)) info
      in
         tab tabstop;
         eprint_entry print_info entry
   in
      List.iter (print 2) l;
      flush stderr

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
            search sum.info_list
   in
      walk summary path

(************************************************************************
 * ACCESS								*
 ************************************************************************)

(*
 * Find an axiom from the summary.
 *)
let find_axiom { info_list = summary } name =
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
let find_rewrite { info_list = summary } name =
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
let find_mlterm { info_list = summary } t =
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
let find_condition { info_list = summary } t =
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
let find_dform { info_list = summary } t =
   let name = opname_of_term t in
   let test = function
      DForm { dform_options = options; dform_redex = t' } -> generalizes t t'
    | _ -> false
   in
      try Some (List_util.find summary test) with
         _ -> None

(*
 * Find a precedence.
 *)
let find_prec { info_list = summary } name =
   let test = function
      Prec s -> s = name
    | _ -> false
   in
      try Some (List_util.find summary test) with
         _ -> None

(*
 * Find the identifier.
 *)
let find_id { info_list = summary } =
   let rec search = function
      h::t ->
         begin
            match h with
               Id i -> i
             | _ -> search t
         end
    | [] ->
         raise Not_found
   in
      search summary

(*
 * Get all the resources.
 *)
let get_resources { info_list = summary } =
   let rec search = function
      (Resource x)::t ->
         x::search t
    | (InheritedResource x)::t ->
         x::search t
    | _::t ->
         search t
    | [] -> []
   in
      search summary

(*
 * Get infix directives.
 *)
let get_infixes { info_list = summary } =
   let rec search = function
      h::t ->
         begin
            match h with
               Infix s -> s::search t
             | _ -> search t
         end
    | [] ->
         []
   in
      search summary

(************************************************************************
 * CREATION/MODIFICATION						*
 ************************************************************************)

(*
 * New info struct.
 *)
let new_module_info () = { info_list = []; info_length = 0 }

(*
 * Coerce the info.
 *)
let info_items { info_list = info } = info

(*
 * Normalize all terms in the info.
 *)
let normalize_param = function
   TermParam t -> TermParam (normalize_term t)
 | p -> p

let rec normalize_info_item normalize_proof = function
   SummaryItem t ->
      SummaryItem (normalize_term t)

 | Rewrite { rw_name = name; rw_redex = redex; rw_contractum = con; rw_proof = pf } ->
      Rewrite { rw_name = name;
                rw_redex = normalize_term redex;
                rw_contractum = normalize_term con;
                rw_proof = normalize_proof pf
      }

 | CondRewrite { crw_name = name;
                 crw_params = params;
                 crw_args = args;
                 crw_redex = redex;
                 crw_contractum = con;
                 crw_proof = pf
   } ->
      CondRewrite { crw_name = name;
                    crw_params = List.map normalize_param params;
                    crw_args = List.map normalize_term args;
                    crw_redex = normalize_term redex;
                    crw_contractum = normalize_term con;
                    crw_proof = normalize_proof pf
      }

 | Axiom { axiom_name = name; axiom_stmt = t; axiom_proof = pf } ->
      Axiom { axiom_name = name;
              axiom_stmt = normalize_term t;
              axiom_proof = normalize_proof pf
      }

 | Rule { rule_name = name;
          rule_params = params;
          rule_stmt = t;
          rule_proof = pf
   } ->
      Rule { rule_name = name;
             rule_params = List.map normalize_param params;
             rule_stmt = normalize_mterm t;
             rule_proof = normalize_proof pf
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
      Module (name, normalize_info info normalize_proof)
      
 | DForm { dform_options = options;
           dform_redex = redex;
           dform_def = def
   } ->
      DForm { dform_options = List.map normalize_term options;
              dform_redex = normalize_term redex;
              dform_def =
                 match def with
                    Some t ->
                       Some (normalize_term t)
                  | None ->
                       None
      }
      
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

and normalize_info { info_list = info; info_length = index } normalize_proof =
   { info_list = List.map (normalize_info_item normalize_proof) info;
     info_length = index
   }

(*
 * Add a command to the info.
 *)
let add_command { info_list = info; info_length = length } item =
   { info_list = item::info; info_length = length + 1 }, length

let get_command { info_list = info; info_length = length } index =
   List.nth info (length - index - 1)

(*
 * Join some new commands to the list.
 * The new commands are expected to be in the same order
 * as the old commands, and all the old commands must appear in the
 * new list.  Check it!
 *)
let set_commands { info_list = info } items =
   let items = List.rev items in
   let rec check = function
      (item::items) as l, item'::items' ->
         if item = item' then
            check (items, items')
         else
            check (l, items')
    | [], _ ->
         ()
    | (entry :: _), [] ->
         let eprint_info _ =
            ()
         in
            eprintf "Filter_summary.set_commands: new command list is not a superset\n";
            eprintf "This command is not included: ";
            eprint_entry eprint_info entry;
            eflush stderr;
            raise (Failure "Filter_summary.set_commands")
   in
      if debug_filter_cache then
         begin
            eprintf "Saved commands:\n";
            List_util.rev_iter eprint_command info;
            eprintf "\nNew commands:\n";
            List_util.rev_iter eprint_command items;
            flush stderr
         end;

      check (info, items);
      { info_list = items; info_length = List.length items }

(*
 * Convert the proofs.
 *)
let rec proof_map_item f = function
   SummaryItem t ->
      SummaryItem t

 | Rewrite { rw_name = name; rw_redex = redex; rw_contractum = con; rw_proof = pf } ->
      Rewrite { rw_name = name;
                rw_redex = redex;
                rw_contractum = con;
                rw_proof = f "Rewrite" pf
      }

 | CondRewrite { crw_name = name;
                 crw_params = params;
                 crw_args = args;
                 crw_redex = redex;
                 crw_contractum = con;
                 crw_proof = pf
   } ->
      CondRewrite { crw_name = name;
                    crw_params = params;
                    crw_args = args;
                    crw_redex = redex;
                    crw_contractum = con;
                    crw_proof = f "CondRewrite" pf
      }

 | Axiom { axiom_name = name; axiom_stmt = t; axiom_proof = pf } ->
      Axiom { axiom_name = name;
              axiom_stmt = t;
              axiom_proof = f "Axiom" pf
      }

 | Rule { rule_name = name;
          rule_params = params;
          rule_stmt = t;
          rule_proof = pf
   } ->
      Rule { rule_name = name;
             rule_params = params;
             rule_stmt = t;
             rule_proof = f "Rule" pf
      }
      
 | Module (name, info) ->
      Module (name, proof_map f info)
      
 | Opname name ->
      Opname name
 | MLTerm t ->
      MLTerm t
 | Condition c ->
      Condition c
 | Parent p ->
      Parent p
 | DForm d ->
      DForm d
 | Prec p ->
      Prec p
 | Id id ->
      Id id
 | Resource r ->
      Resource r
 | InheritedResource r ->
      InheritedResource r
 | Infix op ->
      Infix op

and proof_map f { info_list = info; info_length = index } =
   { info_list = List.map (proof_map_item f) info;
     info_length = index
   }

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
            DForm { dform_options = tags'; dform_redex =  term' } ->
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
      SummaryItem _ ->
         ()
    | Rewrite info ->
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
    | DForm { dform_options = flags; dform_redex = term } ->
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
and check_implementation { info_list = implem } { info_list = interf } =
   List.iter (check_implemented implem) interf

(*
 * $Log$
 * Revision 1.3  1997/09/12 17:21:38  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.2  1997/08/06 16:17:32  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
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
