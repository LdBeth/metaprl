(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Printf

open Mp_debug
open File_util
open Opname
open Refiner_sig
open Precedence

open Filter_util
open Filter_type
open Filter_ocaml

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_summary%t" eflush

let debug_summary =
   create_debug (**)
      { debug_name = "summary";
        debug_description = "display prl summary operations";
        debug_value = false
      }

let debug_match =
   create_debug (**)
      { debug_name = "match";
        debug_description = "print term on proof copying errors";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The summary contains information about everything in the file.
 * We use the summary for both interfaces and implementations.
 * If the implementation requires a term for the definition,
 * we use a term option so that the term does not have to be
 * provided in the interface.
 *
 * A MagicBlock is a block of code that we use to compute
 * a magic number.  The magic number changes whenever the code changes.
 *)
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item =
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
      ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info) summary_item_type

(*
 * The info about a specific module is just a list of items.
 *)
and ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info =
   { info_list : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc list }

(*
 * Pair it with a location.
 *)
and ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc =
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item * (int * int)

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
 | ToploopItem _ ->
      eprintf "ToploopItem\n"
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
 | MLRewrite { mlterm_name = name } ->
      eprintf "MLRewrite: %s\n" name
 | MLAxiom { mlterm_name = name } ->
      eprintf "MLAxiom: %s\n" name
 | Parent { parent_name = path } ->
      eprintf "Parent: %s\n" (string_of_path path)
 | Module (name, { info_list = info }) ->
      eprintf "Module: %s\n" name;
      print_info info
 | DForm { dform_name = name } ->
      eprintf "Dform: %s\n" name
 | Prec name ->
      eprintf "Precedence: %s\n" name
 | PrecRel { prec_rel = rel; prec_left = left; prec_right = right } ->
      let rels =
         match rel with
            NoRelation -> "<norelation>"
          | LTRelation -> "<"
          | EQRelation -> "="
          | GTRelation -> ">"
      in
         eprintf "Precedence: %s %s %s\n" left rels right
 | Resource { resource_name = name } ->
      eprintf "Resource: %s\n" name
 | Infix name ->
      eprintf "Infix: %s\n" name
 | Id id ->
      eprintf "Id: 0x%08x\n" id
 | MagicBlock { magic_name = name } ->
      eprintf "Magic: %s\n" name

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
   let rec print tabstop (entry, _) =
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
      [] ->
         sum
    | name::rest ->
         let rec search = function
            [] ->
               raise (Failure ("Filter_summary.find_sub_module: can't find " ^ (string_of_path path)))
          | ((Module (n, s)), _)::t ->
               eprintf "Filter_summary.find_sub_module: checking %s = %s%t" n name eflush;
               if n = name then
                  walk s rest
               else
                  search t
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
 * List all the parents.
 *)
let parents { info_list = summary } =
   let rec collect = function
      (item, _) :: t ->
         begin
            match item with
               Parent { parent_name = name } ->
                  name :: collect t
             | _ ->
                  collect t
         end
    | [] ->
         []
   in
      collect summary

(*
 * Test for an axiom.
 *)
let test_axiom name (item, _) =
   match item with
      Axiom { axiom_name = n } ->
         n = name
    | Rule { rule_name = n } ->
         n = name
    | _ ->
         false

let find_axiom { info_list = summary } name =
   try Some (List_util.find (test_axiom name) summary) with
      Not_found ->
         None

(*
 * Find a rewrite in the summary.
 *)
let test_rewrite name (item, _) =
   match item with
      Rewrite { rw_name = n } ->
         n = name
    | CondRewrite { crw_name = n } ->
         n = name
    | _ ->
         false

let find_rewrite { info_list = summary } name =
   try Some (List_util.find (test_rewrite name) summary) with
      Not_found ->
         None

(*
 * Find a condition.
 *)
let test_mlrewrite name = function
   MLRewrite { mlterm_name = name' }, _ ->
      name' = name
 | _ ->
      false

let find_mlrewrite { info_list = summary } name =
   try Some (List_util.find (test_mlrewrite name) summary) with
      Not_found ->
         None

let test_mlaxiom name = function
   MLAxiom { mlterm_name = name' }, _ ->
      name' = name
 | _ ->
      false

let find_mlaxiom { info_list = summary } name =
   try Some (List_util.find (test_mlaxiom name) summary) with
      Not_found ->
         None

(*
 * Find a module.
 *)
let test_module name (item, _) =
   match item with
      Module (name', _) ->
         name' = name
    | _ ->
         false

let find_module { info_list = summary } name =
   try Some (List_util.find (test_module name) summary) with
      Not_found ->
         None

(*
 * Find a display form.
 *)
let test_dform name (item, _) =
   match item with
      DForm { dform_name = name' } ->
         name' = name
    | _ ->
         false

let find_dform { info_list = summary } name =
   try Some (List_util.find (test_dform name) summary) with
      _ -> None

(*
 * Find a precedence.
 *)
let test_prec name (item, _) =
   match item with
      Prec s ->
         s = name
    | _ ->
         false

let find_prec { info_list = summary } name =
   try Some (List_util.find (test_prec name) summary) with
      Not_found ->
         None

(*
 * Find the identifier.
 *)
let find_id { info_list = summary } =
   let rec search = function
      (h, _)::t ->
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
      (Resource x, _)::t ->
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
      (h, _)::t ->
         begin
            match h with
               Infix s -> s::search t
             | _ -> search t
         end
    | [] ->
         []
   in
      search summary

(*
 * Get the proofs.
 *)
let get_proofs { info_list = summary } =
   let rec collect proofs = function
      (h, _)::t ->
         let proofs =
            match h with
               Axiom { axiom_name = name; axiom_proof = pf } ->
                  (name, pf) :: proofs
             | Rule { rule_name = name; rule_proof = pf } ->
                  (name, pf) :: proofs
             | Rewrite { rw_name = name; rw_proof = pf } ->
                  (name, pf) :: proofs
             | CondRewrite { crw_name = name; crw_proof = pf } ->
                  (name, pf) :: proofs
             | _ ->
                  proofs
         in
            collect proofs t
    | [] ->
         List.rev proofs
   in
      collect [] summary

(*
 * Find any item, by name.
 *)
let find { info_list = summary } name =
   let test (item, _) =
      match item with
         Axiom { axiom_name = n } ->
            n = name
       | Rule { rule_name = n } ->
            n = name
       | Rewrite { rw_name = n } ->
            n = name
       | CondRewrite { crw_name = n } ->
            n = name
       | MLRewrite { mlterm_name = n } ->
            n = name
       | MLAxiom { mlterm_name = n } ->
            n = name
       | DForm { dform_name = n } ->
            n = name
       | Prec s ->
            s = name
       | _ ->
            false
   in
      List_util.find test summary

(*
 * Set an item by name.
 *)
let set_command info item =
   let test =
      match fst item with
         Axiom { axiom_name = name } ->
            test_axiom name
       | Rule { rule_name = name } ->
            test_axiom name
       | Rewrite { rw_name = name } ->
            test_rewrite name
       | CondRewrite { crw_name = name } ->
            test_rewrite name
       | MLRewrite { mlterm_name = name } ->
            test_mlrewrite name
       | MLAxiom { mlterm_name = name } ->
            test_mlaxiom name
       | DForm { dform_name = name } ->
            test_dform name
       | Prec s ->
            test_prec s
       | _ ->
            (fun _ -> false)
   in
      { info_list = List_util.replace_first test item info.info_list }

(************************************************************************
 * CREATION/MODIFICATION						*
 ************************************************************************)

(*
 * New info struct.
 *)
let new_module_info () =
   { info_list = [] }

(*
 * Coerce the info.
 *)
let info_items { info_list = info } =
   List.rev info

(*
 * Optional application.
 *)
let opt_apply f = function
   Some x ->
      Some (f x)
 | None ->
      None

(*
 * Convert a resource.
 *)
let convert_resource convert
    { resource_name = name;
      resource_extract_type = extract;
      resource_improve_type = improve;
      resource_data_type = data;
      resource_arg_type = arg
    } =
   { resource_name = name;
     resource_extract_type = convert.ctyp_f extract;
     resource_improve_type = convert.ctyp_f improve;
     resource_data_type = convert.ctyp_f data;
     resource_arg_type = convert.ctyp_f arg
   }

(*
 * Normalize all terms in the info.
 *)
let summary_map (convert : ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1,
                            'term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) convert)
=
   (* Map the terms inside of params *)
   let param_map = function
      TermParam t -> TermParam (convert.term_f t)
    | VarParam s -> VarParam s
    | ContextParam s -> ContextParam s
   in

   let res_map = function
   (*
      [] -> []
    | [(name, exprs)::tl] ->
         name, List.map convert.expr_f exprs :: res_map tl
   *) _ -> [] (* we should not use res_map until we recreate .prlb files *)
   in

   (* Map a summary item *)
   let rec item_map (item, loc) =
      let item =
         match item with
            SummaryItem t ->
               SummaryItem (convert.item_f t)
          | ToploopItem t ->
               ToploopItem (convert.item_f t)

          | Rewrite { rw_name = name; rw_redex = redex; rw_contractum = con; rw_proof = pf ; rw_resources = res } ->
               Rewrite { rw_name = name;
                         rw_redex = convert.term_f redex;
                         rw_contractum = convert.term_f con;
                         rw_proof = convert.proof_f name pf;
                         rw_resources = res_map res;
               }

          | CondRewrite { crw_name = name;
                          crw_params = params;
                          crw_args = args;
                          crw_redex = redex;
                          crw_contractum = con;
                          crw_proof = pf;
                          crw_resources = res
            } ->
               CondRewrite { crw_name = name;
                             crw_params = List.map param_map params;
                             crw_args = List.map convert.term_f args;
                             crw_redex = convert.term_f redex;
                             crw_contractum = convert.term_f con;
                             crw_proof = convert.proof_f name pf;
                             crw_resources = res_map res
               }

          | Axiom { axiom_name = name; axiom_stmt = t; axiom_proof = pf; axiom_resources = res } ->
               Axiom { axiom_name = name;
                       axiom_stmt = convert.term_f t;
                       axiom_proof = convert.proof_f name pf;
                       axiom_resources = res_map res
               }

          | Rule { rule_name = name;
                   rule_params = params;
                   rule_stmt = t;
                   rule_proof = pf;
                   rule_resources = res
            } ->
               Rule { rule_name = name;
                      rule_params = List.map param_map params;
                      rule_stmt = convert.meta_term_f t;
                      rule_proof = convert.proof_f name pf;
                      rule_resources = res_map res
               }

          | Opname { opname_name = name; opname_term = t } ->
               Opname { opname_name = name; opname_term = convert.term_f t }

          | MLRewrite t ->
               (* HACK! remove this when we start using ASCII theory files *)
               let t =
                  if Obj.size (Obj.repr t) = 3 then
                     let (term, contracta, def) = Obj.magic t in
                        { mlterm_name = "unknown";
                          mlterm_params = [];
                          mlterm_term = term;
                          mlterm_contracta = contracta;
                          mlterm_def = def;
                          mlterm_resources = []
                        }
                  else
                     t
               in
               let { mlterm_name = name;
                     mlterm_params = params;
                     mlterm_term = term;
                     mlterm_contracta = cons;
                     mlterm_def = def;
                     mlterm_resources = res
                   } = t
               in
                  MLRewrite { mlterm_name = name;
                              mlterm_params = List.map param_map params;
                              mlterm_term = convert.term_f term;
                              mlterm_contracta = List.map convert.term_f cons;
                              mlterm_def = opt_apply convert.expr_f def;
                              mlterm_resources = res_map res
                  }

          | MLAxiom t ->
               (* HACK! remove this when we start using ASCII theory files *)
               let t =
                  if Obj.size (Obj.repr t) = 3 then
                     let (term, contracta, def) = Obj.magic t in
                        { mlterm_name = "unknown";
                          mlterm_params = [];
                          mlterm_term = term;
                          mlterm_contracta = contracta;
                          mlterm_def = def;
                          mlterm_resources = []
                        }
                  else
                     t
               in
               let { mlterm_name = name;
                     mlterm_params = params;
                     mlterm_term = term;
                     mlterm_contracta = cons;
                     mlterm_def = def;
                     mlterm_resources = res
                   } = t
               in
                  MLAxiom { mlterm_name = name;
                            mlterm_params = List.map param_map params;
                            mlterm_term = convert.term_f term;
                            mlterm_contracta = List.map convert.term_f cons;
                            mlterm_def = opt_apply convert.expr_f def;
                            mlterm_resources = res_map res
                  }

          | Parent { parent_name = path;
                     parent_opens = opens;
                     parent_resources = resources
            } ->
               Parent { parent_name = path;
                        parent_opens = opens;
                        parent_resources = List.map (convert_resource convert) resources
               }

          | Module (name, info) ->
               Module (name, map info)

          | DForm { dform_name = name;
                    dform_modes = modes;
                    dform_options = options;
                    dform_redex = redex;
                    dform_def = def
            } ->
               let def' =
                  match def with
                     NoDForm ->
                        NoDForm
                   | TermDForm t ->
                        TermDForm (convert.term_f t)
                   | MLDForm { dform_ml_printer = printer;
                               dform_ml_buffer = buffer;
                               dform_ml_contracta = cons;
                               dform_ml_code = code
                     } ->
                        MLDForm { dform_ml_printer = printer;
                                  dform_ml_buffer = buffer;
                                  dform_ml_contracta = List.map convert.term_f cons;
                                  dform_ml_code = convert.expr_f code
                        }
               in
                  DForm { dform_name = name;
                          dform_modes = modes;
                          dform_options = options;
                          dform_redex = convert.term_f redex;
                          dform_def = def'
                  }

          | Prec x ->
               Prec x

          | PrecRel rel ->
               PrecRel rel

          | Id id ->
               Id id

          | Resource resource ->
               Resource (convert_resource convert resource)

          | Infix name ->
               Infix name

          | MagicBlock { magic_name = name;
                         magic_code = items
            } ->
               MagicBlock { magic_name = name;
                            magic_code = List.map convert.item_f items
               }
      in
         item, loc

   and map { info_list = info } =
      { info_list = List.map item_map info }
   in
      map

(*
 * Add a command to the info.
 *)
let add_command { info_list = info } item =
   { info_list = item::info }

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

module FilterSummaryTerm (ToTerm : RefinerSig) =
struct
   open ToTerm.Term
   open ToTerm.TermType
   open ToTerm.TermOp
   open ToTerm.TermMan
   open ToTerm.TermSubst
   open ToTerm.TermMeta
   open ToTerm.RefineError

   module SimplePrint = Simple_print.MakeSimplePrint (ToTerm);;
   open SimplePrint

   (*
    * These are the possible opnames.
    *)
   let mk_opname =
      let op = Opname.mk_opname "Summary" Opname.nil_opname in
         fun s -> Opname.mk_opname s op

   let rewrite_op                  = mk_opname "rewrite"
   let cond_rewrite_op             = mk_opname "cond_rewrite"
   let axiom_op                    = mk_opname "axiom"
   let rule_op                     = mk_opname "rule"
   let res_op                      = mk_opname "resource_defs"
   let opname_op                   = mk_opname "opname"
   let mlrewrite_op                = mk_opname "mlrewrite"
   let mlaxiom_op                  = mk_opname "mlaxiom"
   let parent_op                   = mk_opname "parent"
   let module_op                   = mk_opname "module"
   let dform_op                    = mk_opname "dform"
   let prec_op                     = mk_opname "prec"
   let prec_rel_op                 = mk_opname "prec_rel"
   let id_op                       = mk_opname "id"
   let resource_op                 = mk_opname "resource"
   let infix_op                    = mk_opname "infix"
   let magic_block_op              = mk_opname "magic_block"
   let summary_item_op             = mk_opname "summary_item"
   let toploop_item_op             = mk_opname "toploop_item"

   (*
    * Meta term conversions.
    *)
   let meta_theorem_op     = mk_opname "meta_theorem"
   let meta_implies_op     = mk_opname "meta_implies"
   let meta_function_op    = mk_opname "meta_function"
   let meta_iff_op         = mk_opname "meta_iff"
   let meta_labeled_op     = mk_opname "meta_labeled"

   let rec meta_term_of_term t =
      let opname = opname_of_term t in
         if Opname.eq opname meta_theorem_op then
            MetaTheorem (one_subterm t)
         else if Opname.eq opname meta_implies_op then
            let a, b = two_subterms t in
               MetaImplies (meta_term_of_term a, meta_term_of_term b)
         else if Opname.eq opname meta_function_op then
            let v, a, b = three_subterms t in
               MetaFunction (v, meta_term_of_term a, meta_term_of_term b)
         else if Opname.eq opname meta_iff_op then
            let a, b = two_subterms t in
               MetaIff (meta_term_of_term a, meta_term_of_term b)
         else if Opname.eq opname meta_labeled_op then
            let t' = dest_term t in
            let o' = dest_op t'.term_op in
            match o'.op_params, t'.term_terms with
               [p], [t] ->
                  begin match dest_param p, dest_bterm t with
                     String l, { bvars = []; bterm = t } ->
                        MetaLabeled (l,meta_term_of_term t)
                   | _ -> raise (Failure "Filter_summary.meta_term_of_term: term is not a meta term")
                  end
             | _ -> raise (Failure "Filter_summary.meta_term_of_term: term is not a meta term")
         else
            raise (Failure "Filter_summary.meta_term_of_term: term is not a meta term")

   let rec term_of_meta_term = function
      MetaTheorem t ->
         mk_simple_term meta_theorem_op [t]
    | MetaImplies (a, b) ->
         mk_simple_term meta_implies_op [term_of_meta_term a; term_of_meta_term b]
    | MetaFunction (v, a, b) ->
         mk_simple_term meta_function_op [v; term_of_meta_term a; term_of_meta_term b]
    | MetaIff (a, b) ->
         mk_simple_term meta_iff_op [term_of_meta_term a; term_of_meta_term b]
    | MetaLabeled (l, t) ->
         mk_term (make_op { op_name = meta_labeled_op; op_params = [ make_param (String l) ]}) [mk_simple_bterm (term_of_meta_term t)]

   (*************
    * DESTRUCTION
    *)

   (*
    * Destruct a location.
    *)
   let loc_op = mk_opname "location"

   let dest_loc t =
      match (dest_number_number_dep0_any_term t) with
          (Mp_num.Int i, Mp_num.Int j, t) -> t, (i, j)
        | _ -> raise (Failure "dest_loc: location is not an integer")

   let dest_loc_string t =
      match dest_number_number_string_dep0_any_term t with
          Mp_num.Int i, Mp_num.Int j, s, t -> t, (i, j), s
        | _ -> raise (Failure "dest_loc_string: location is not an integer")

   (*
    * Dform options.
    *)
   let dform_inherit_op  = mk_opname "inherit_df"
   let dform_prec_op     = mk_opname "prec_df"
   let dform_parens_op   = mk_opname "parens_df"
   let dform_internal_op = mk_opname "internal_df"
   let dform_mode_op     = mk_opname "mode_df"

   let dest_dform_opt tl =
      let modes = ref [] in
      let options = ref [] in
      let push l x = l := x :: !l in
      let dest_opt t =
         let opname = opname_of_term t in
            if Opname.eq opname dform_inherit_op then
               push options DFormInheritPrec
            else if Opname.eq opname dform_prec_op then
               push options (DFormPrec (dest_string_param t))
            else if Opname.eq opname dform_parens_op then
               push options DFormParens
            else if Opname.eq opname dform_internal_op then
               push options DFormInternal
            else if Opname.eq opname dform_mode_op then
               push modes (dest_string_param t)
            else
               raise (Failure "Dform option is not valid")
      in
         List.iter dest_opt tl;
         List.rev !modes, List.rev !options

   (*
    * Dform definitions.
    *)
   let dform_none_op   = mk_opname "df_none"
   let dform_term_op   = mk_opname "df_term"
   let dform_ml_op     = mk_opname "df_ml"

   let dest_dform_def convert t =
      let opname = opname_of_term t in
         if Opname.eq opname dform_none_op then
            NoDForm
         else if Opname.eq opname dform_term_op then
            TermDForm (convert.term_f (one_subterm t))
         else if Opname.eq opname dform_ml_op then
            let printer, buffer, cons, expr = dest_string_string_dep0_dep0_any_term t in
               MLDForm { dform_ml_printer   = printer;
                         dform_ml_buffer    = buffer;
                         dform_ml_contracta = List.map convert.term_f (dest_xlist cons);
                         dform_ml_code      = convert.expr_f expr
                       }
         else
            raise (Failure "Dform term is not valid")

   (*
    * Optional args.
    *)
   let some_op = mk_opname "some"
   let none_op = mk_opname "none"

   let dest_opt f t =
      let opname = opname_of_term t in
         if Opname.eq opname some_op then
            Some (f (one_subterm t))
         else if Opname.eq opname none_op then
            None
         else
            raise (Failure "not an option")

   let dest_opt_pair f t =
      let opname = opname_of_term t in
         if Opname.eq opname some_op then
            Some (f (two_subterms t))
         else if Opname.eq opname none_op then
            None
         else
            raise (Failure "not an option")

   (*
    * All parameters should be strings.
    *)
   let dest_string_param_list t =
      let l = dest_xlist t in
         List.map dest_string_param (dest_xlist t)

   (*
    * Get a parameter.
    *)
   let context_param_op    = mk_opname "context_param"
   let var_param_op        = mk_opname "var_param"
   let term_param_op       = mk_opname "term_param"

   let dest_param convert t =
      let string_param = function
         [param] ->
            begin
               match dest_param param with
                  String s ->
                     s
                | _ ->
                     raise (Failure "Parameter is not a string")
            end
       | _ ->
            raise (Failure "Wrong number of parameters")
      in
      let { term_op = op } = dest_term t in
      let { op_name = opname; op_params = params } = dest_op op in
         if Opname.eq opname context_param_op then
            ContextParam (string_param params)
         else if Opname.eq opname var_param_op then
            VarParam (string_param params)
         else if Opname.eq opname term_param_op then
            TermParam (convert.term_f (one_subterm t))
         else
            raise (Failure "Illegal parameter")

   (*
    * Get the parameter list.
    *)
   let dest_params convert t =
      List.map (dest_param convert) (dest_xlist t)

   (*
    * Get the list of resource updates
    *)
   let dest_resource_term expr_f t =
      let t, loc, name = dest_loc_string t in
         loc, name, List.map expr_f (dest_xlist t)

   let dest_res convert t =
      List.map (dest_resource_term convert.expr_f) (dest_xlist t)

   (*
    * Collect a rewrite.
    *)
   let rec dest_rewrite convert t =
      let name = dest_string_param t in
      let redex, contractum, proof, res = four_subterms t in
         Rewrite { rw_name = name;
                   rw_redex = convert.term_f redex;
                   rw_contractum = convert.term_f contractum;
                   rw_proof = convert.proof_f name proof;
                   rw_resources = dest_res convert res
         }

   (*
    * Conditional rewrite.
    *)
   and dest_cond_rewrite convert t =
      let name = dest_string_param t in
      let params, args, redex, contractum, proof, res = six_subterms t in
         CondRewrite { crw_name = name;
                       crw_params = dest_params convert params;
                       crw_args = List.map convert.term_f (dest_xlist args);
                       crw_redex = convert.term_f redex;
                       crw_contractum = convert.term_f contractum;
                       crw_proof = convert.proof_f name proof;
                       crw_resources = dest_res convert res
         }

   (*
    * Axiom.
    *)
   and dest_axiom convert t =
      let name = dest_string_param t in
      let stmt, proof, res = three_subterms t in
         Axiom { axiom_name = name;
                 axiom_stmt = convert.term_f stmt;
                 axiom_proof = convert.proof_f name proof;
                 axiom_resources = dest_res convert res
         }

   (*
    * Rule.
    *)
   and dest_rule convert t =
      let name = dest_string_param t in
      let params, stmt, proof, res = four_subterms t in
         Rule { rule_name = name;
                rule_params = dest_params convert params;
                rule_stmt = convert.meta_term_f stmt;
                rule_proof = convert.proof_f name proof;
                rule_resources = dest_res convert res
         }

   (*
    * Opname.
    *)
   and dest_opname convert t =
      let name = dest_string_param t in
      let term = one_subterm t in
         Opname { opname_name = name;
                  opname_term = convert.term_f term
         }

   (*
    * ML Term.
    *)
   and dest_mlrewrite convert t =
      let name = dest_string_param t in
      let params, term, cons, expr, resources = five_subterms t in
         MLRewrite { mlterm_name = name;
                     mlterm_params = dest_params convert params;
                     mlterm_term = convert.term_f term;
                     mlterm_contracta = List.map convert.term_f (dest_xlist cons);
                     mlterm_def = dest_opt convert.expr_f expr;
                     mlterm_resources = dest_res convert resources
         }

   and dest_mlaxiom convert t =
      let name = dest_string_param t in
      let params, term, cons, expr, resources = five_subterms t in
         MLAxiom { mlterm_name = name;
                   mlterm_params = dest_params convert params;
                   mlterm_term = convert.term_f term;
                   mlterm_contracta = List.map convert.term_f (dest_xlist cons);
                   mlterm_def = dest_opt convert.expr_f expr;
                   mlterm_resources = dest_res convert resources
         }

   (*
    * Parent declaration.
    *)
   and dest_parent convert t =
      let path, opens, resources = three_subterms t in
         Parent { parent_name = dest_string_param_list path;
                  parent_opens = List.map dest_string_param_list (dest_xlist opens);
                  parent_resources = List.map (dest_resource_aux convert) (dest_xlist resources)
         }

   (*
    * Enclosed module.
    *)
   and dest_module convert t =
      let name = dest_string_param t in
      let items = dest_xlist (one_subterm t) in
         Module (name, of_term_list convert items)

   (*
    * Display form.
    *)
   and dest_dform convert t =
      let options, redex, def = three_subterms t in
      let name = dest_string_param t in
      let modes, options = dest_dform_opt (dest_xlist options) in
         DForm { dform_name = name;
                 dform_modes = modes;
                 dform_options =  options;
                 dform_redex = convert.term_f redex;
                 dform_def = dest_dform_def convert def
         }

   (*
    * Precedence.
    *)
   and dest_prec convert t =
      Prec (dest_string_param t)

   and dest_prec_rel convert t =
      match dest_string_param_list t with
         [rel; left; right] ->
            let rel =
               match rel with
                  "none" ->
                     NoRelation
                | "lt" ->
                     LTRelation
                | "eq" ->
                     EQRelation
                | "gt" ->
                     GTRelation
                | _ ->
                     raise (Failure ("dest_prec_rel: undefined relation: " ^ rel))
            in
               PrecRel { prec_rel = rel;
                         prec_left = left;
                         prec_right = right
               }
       | _ ->
            raise (Failure "dest_prec_rel: bogus format")

   (*
    * Identifier.
    *)
   and dest_id convert t =
     let n = (dest_number_any_term t) in
       match n with
           Mp_num.Int i -> Id i
         | _ -> raise (Failure "dest_id: can't handle things other than Mp_num.Int")

   (*
    * Resource.
    *)
   and dest_resource_aux convert t =
      let name = dest_string_param t in
      let extract, improve, data, arg = four_subterms t in
         { resource_name = name;
           resource_extract_type = convert.ctyp_f extract;
           resource_improve_type = convert.ctyp_f improve;
           resource_data_type = convert.ctyp_f data;
           resource_arg_type = convert.ctyp_f arg
         }

   and dest_resource convert t =
      Resource (dest_resource_aux convert t)

   (*
    * Infix.
    *)
   and dest_infix convert t =
      Infix (dest_string_param t)

   (*
    * Magic block of items.
    *)
   and dest_magic_block convert t =
      MagicBlock { magic_name = dest_string_param t;
                   magic_code = List.map convert.item_f (dest_xlist (one_subterm t))
      }

   (*
    * SummaryItem.
    *)
   and dest_summary_item convert t =
      SummaryItem (convert.item_f (one_subterm t))

   and dest_toploop_item convert t =
      ToploopItem (convert.item_f (one_subterm t))

   and dest_term_aux
       (convert : (term, term, term, term, term, term,
                   'term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) convert)
       (t : term) =
      let opname = opname_of_term t in
         try
            let info =
               if Opname.eq opname rewrite_op then
                  dest_rewrite convert t
               else if Opname.eq opname cond_rewrite_op then
                  dest_cond_rewrite convert t
               else if Opname.eq opname axiom_op then
                  dest_axiom convert t
               else if Opname.eq opname rule_op then
                  dest_rule convert t
               else if Opname.eq opname opname_op then
                  dest_opname convert t
               else if Opname.eq opname mlrewrite_op then
                  dest_mlrewrite convert t
               else if Opname.eq opname mlaxiom_op then
                  dest_mlaxiom convert t
               else if Opname.eq opname parent_op then
                  dest_parent convert t
               else if Opname.eq opname module_op then
                  dest_module convert t
               else if Opname.eq opname dform_op then
                  dest_dform convert t
               else if Opname.eq opname prec_op then
                  dest_prec convert t
               else if Opname.eq opname prec_rel_op then
                  dest_prec_rel convert t
               else if Opname.eq opname id_op then
                  dest_id convert t
               else if Opname.eq opname resource_op then
                  dest_resource convert t
               else if Opname.eq opname infix_op then
                  dest_infix convert t
               else if Opname.eq opname magic_block_op then
                  dest_magic_block convert t
               else if Opname.eq opname summary_item_op then
                  dest_summary_item convert t
               else if Opname.eq opname toploop_item_op then
                  dest_toploop_item convert t
               else
                  raise (Failure "term is not found")
            in
               Some info
         with
            Failure _
          | RefineError (_, TermMatchError _) ->
               eprintf "Filter_summary.dest_term: incorrect syntax for %s%t" (string_of_opname opname) eflush;
               None

   and dest_term_loc
       (convert : (term, term, term, term, term, term,
                   'term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) convert)
       (t : term) =
      let t, loc = dest_loc t in
         match dest_term_aux convert t with
            Some t ->
               Some (t, loc)
          | None ->
               None

   (*
    * Make a module from the term list.
    *)
   and of_term_list
       (convert : (term, term, term, term, term, term,
                   'term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) convert)
       (terms : term list) =
      let items = List_util.some_map (dest_term_loc convert) terms in
         ({ info_list = List.rev items } : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info)

   (**************
    * CONSTRUCTION
    *)

   (*
    * Make a location.
    *)
   let mk_loc (i, j) t =
      mk_number_number_dep0_term loc_op (Mp_num.Int i) (Mp_num.Int j) t

   (*
    * Make a optional arg.
    *)
   let mk_opt f = function
      Some t ->
         mk_simple_term some_op [f t]
    | None ->
         mk_simple_term none_op []

   let mk_opt_pair f = function
      Some (t1, t2) ->
         let t1, t2 = f (t1, t2) in
            mk_simple_term some_op [t1; t2]
    | None ->
         mk_simple_term none_op []

   (*
    * Make a term with a string parameter.
    *)
   let mk_string_param_term opname s terms =
      let param = make_param (String s) in
      let op = mk_op opname [param] in
      let bterms = List.map (fun t -> mk_bterm [] t) terms in
         mk_term op bterms

   let mk_loc_string_term op (start, finish) name t =
      mk_number_number_string_dep0_term op (Mp_num.Int start) (Mp_num.Int finish) name t

   (*
    * Make a term with only strings parameters.
    *)
   let mk_strings_term opname l =
      mk_xlist_term (List.map (fun s -> mk_string_param_term opname s []) l)

   (*
    * Parameters.
    *)
   let mk_param convert = function
      ContextParam s ->
         mk_string_param_term context_param_op s []
    | VarParam s ->
         mk_string_param_term var_param_op s []
    | TermParam t ->
         mk_simple_term term_param_op [convert.term_f t]

   let mk_params convert params =
      mk_xlist_term (List.map (mk_param convert) params)

   (*
    * Display form options.
    *)
   let mk_dform_mode mode =
      mk_string_param_term dform_mode_op mode []

   let mk_dform_opt = function
      DFormInheritPrec ->
         mk_simple_term dform_prec_op []
    | DFormPrec s ->
         mk_string_param_term dform_prec_op s []
    | DFormParens ->
         mk_simple_term dform_parens_op []
    | DFormInternal ->
         mk_simple_term dform_internal_op []

   (*
    * Dform definitions.
    *)
   let mk_dform_def convert = function
      NoDForm ->
         mk_simple_term dform_none_op []
    | TermDForm t ->
         mk_simple_term dform_term_op [convert.term_f t]
    | MLDForm { dform_ml_printer = printer;
                dform_ml_buffer = buffer;
                dform_ml_contracta = cons;
                dform_ml_code = expr
      } ->
         let cons = mk_xlist_term (List.map convert.term_f cons) in
         let expr = convert.expr_f expr in
            mk_string_string_dep0_dep0_term dform_ml_op printer buffer cons expr

   (*
    * Precedence relation.
    *)
   let mk_prec_rel_term rel left right =
      let rel =
         match rel with
            NoRelation ->
               "none"
          | LTRelation ->
               "lt"
          | EQRelation ->
               "eq"
          | GTRelation ->
               "gt"
      in
         mk_strings_term prec_rel_op [rel; left; right]

   (*
    * Term conversions.
    *)
   let rec term_of_summary_item convert t =
      mk_simple_term summary_item_op [convert.item_f t]

   and term_of_toploop_item convert t =
      mk_simple_term toploop_item_op [convert.item_f t]

   and term_of_res expr_f (loc, name, args) =
      mk_loc_string_term res_op loc name (mk_xlist_term (List.map expr_f args))

   and term_of_resources convert res =
      mk_xlist_term (List.map (term_of_res convert.expr_f) res)

   and term_of_rewrite convert { rw_name = name;
                                 rw_redex = redex;
                                 rw_contractum = con;
                                 rw_proof = pf;
                                 rw_resources = res
       } =
      mk_string_param_term rewrite_op name [convert.term_f redex;
                                            convert.term_f con;
                                            convert.proof_f name pf;
                                            term_of_resources convert res]

   and term_of_cond_rewrite convert { crw_name = name;
                                      crw_params = params;
                                      crw_args = args;
                                      crw_redex = redex;
                                      crw_contractum = con;
                                      crw_proof = pf;
                                      crw_resources = res
       } =
      mk_string_param_term cond_rewrite_op name [mk_params convert params;
                                                 mk_xlist_term (List.map convert.term_f args);
                                                 convert.term_f redex;
                                                 convert.term_f con;
                                                 convert.proof_f name pf;
                                                 term_of_resources convert res]

   and term_of_axiom convert { axiom_name = name;
                               axiom_stmt = t;
                               axiom_proof = pf;
                               axiom_resources = res
       } =
      mk_string_param_term axiom_op name [convert.term_f t; convert.proof_f name pf]

   and term_of_rule convert { rule_name = name;
                              rule_params = params;
                              rule_stmt = t;
                              rule_proof = pf;
                              rule_resources = res
       } =
      mk_string_param_term rule_op name [mk_params convert params;
                                         convert.meta_term_f t;
                                         convert.proof_f name pf;
                                         term_of_resources convert res]

   and term_of_opname convert { opname_name = name; opname_term = term } =
      mk_string_param_term opname_op name [convert.term_f term]

   and term_of_mlrewrite convert { mlterm_name = name;
                                   mlterm_params = params;
                                   mlterm_term = term;
                                   mlterm_contracta = cons;
                                   mlterm_def = expr_opt;
                                   mlterm_resources = res
       } =
      mk_string_param_term mlrewrite_op name (**)
         [mk_params convert params;
          convert.term_f term;
          mk_xlist_term (List.map convert.term_f cons);
          mk_opt convert.expr_f expr_opt;
          term_of_resources convert res]

   and term_of_mlaxiom convert { mlterm_name = name;
                                 mlterm_params = params;
                                 mlterm_term = term;
                                 mlterm_contracta = cons;
                                 mlterm_def = expr_opt;
                                 mlterm_resources = res
       } =
      mk_string_param_term mlaxiom_op name (**)
         [mk_params convert params;
          convert.term_f term;
          mk_xlist_term (List.map convert.term_f cons);
          mk_opt convert.expr_f expr_opt;
          term_of_resources convert res]

   and term_of_parent convert { parent_name = path;
                                parent_opens = opens;
                                parent_resources = resources
       } =
      mk_simple_term parent_op (**)
         [mk_strings_term parent_op path;
          mk_xlist_term (List.map (mk_strings_term parent_op) opens);
          mk_xlist_term (List.map (term_of_resource convert) resources)]

   and term_of_module convert name info =
      mk_string_param_term module_op name [mk_xlist_term (term_list convert info)]

   and term_of_dform convert { dform_name = name;
                               dform_modes = modes;
                               dform_options = options;
                               dform_redex = redex;
                               dform_def = def
       } =
      let modes = List.map mk_dform_mode modes in
      let options = List.map mk_dform_opt options in
         mk_string_param_term dform_op name [mk_xlist_term (modes @ options);
                                             convert.term_f redex;
                                             mk_dform_def convert def]

   and term_of_prec p =
      mk_string_param_term prec_op p []

   and term_of_prec_rel { prec_rel = rel; prec_left = left; prec_right = right } =
      mk_prec_rel_term rel left right

   and term_of_id id =
      mk_number_term id_op (Mp_num.Int id)

   and term_of_resource convert
       { resource_name = name;
         resource_extract_type = extract;
         resource_improve_type = improve;
         resource_data_type = data;
         resource_arg_type = arg
       } =
      mk_string_param_term resource_op name [convert.ctyp_f extract;
                                             convert.ctyp_f improve;
                                             convert.ctyp_f data;
                                             convert.ctyp_f arg]

   and term_of_infix op =
      mk_string_param_term infix_op op []

   and term_of_magic_block convert { magic_name = name; magic_code = items } =
      mk_string_param_term magic_block_op name [mk_xlist_term (List.map convert.item_f items)]

   (*
    * Convert the items to a term.
    *)
   and term_list_aux (convert : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item, term,
                                 term, term, term, term, term) convert) = function
      SummaryItem t ->
         term_of_summary_item convert t
    | ToploopItem t ->
         term_of_toploop_item convert t
    | Rewrite rw ->
         term_of_rewrite convert rw
    | CondRewrite crw ->
         term_of_cond_rewrite convert crw
    | Axiom ax ->
         term_of_axiom convert ax
    | Rule rw ->
         term_of_rule convert rw
    | Opname opname ->
         term_of_opname convert opname
    | MLRewrite t ->
         term_of_mlrewrite convert t
    | MLAxiom cond ->
         term_of_mlaxiom convert cond
    | Parent par ->
         term_of_parent convert par
    | Module (name, info) ->
         term_of_module convert name info
    | DForm df ->
         term_of_dform convert df
    | Prec p ->
         term_of_prec p
    | PrecRel rel ->
         term_of_prec_rel rel
    | Id id ->
         term_of_id id
    | Resource rsrc ->
         term_of_resource convert rsrc
    | Infix op ->
         term_of_infix op
    | MagicBlock magic ->
         term_of_magic_block convert magic

   and term_list_loc convert (t, loc) =
      mk_loc loc (term_list_aux convert t)

   and term_list (convert : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
                             term, term, term, term, term, term) convert)
       ({ info_list = info } : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info) =
      (List.map (term_list_loc convert) (List.rev info) : term list)

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
      List_util.for_all2 generalizes tl1 tl2

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
         List_util.for_all2 check int_params imp_params

   (*
    * Check that a rewrite is justified.
    *)
   let check_rewrite
       (info : ('term1, 'proof1, 'expr1) rewrite_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
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
   let check_cond_rewrite
       (info : ('term, 'proof1, 'expr1) cond_rewrite_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
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
   let check_axiom
       (info : ('term1, 'proof1, 'expr1) axiom_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
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
   let check_rule
       (info : ('term1, 'meta_term1, 'proof1, 'expr1) rule_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
      let { rule_name = name; rule_params = params; rule_stmt = stmt } = info in
      let rec search = function
         [] ->
            implem_error (sprintf "Rule %s: not implemented" name)
       | h::t ->
            match h with
               Rule { rule_name = name'; rule_params = params'; rule_stmt = stmt' } ->
                  let stmt' = strip_mfunction stmt' in
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
   let check_opname
       (info : 'term1 opname_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
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
   let check_mlrewrite
       ({ mlterm_name = name; mlterm_term = term } : ('term1, 'expr1) mlterm_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
      let rec search = function
         [] ->
            implem_error (sprintf "MLRewrite %s: not implemented" name)
       | h::t ->
            match h with
               MLRewrite { mlterm_name = name'; mlterm_term = term' } when name' = name ->
                  if not (alpha_equal term' term) then
                     implem_error (sprintf "MLRewrite %s: definition does not match" name)
             | _ ->
                  search t
      in
         search implem

   let check_mlaxiom
       ({ mlterm_name = name; mlterm_term = term } : ('term1, 'expr1) mlterm_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
      let rec search = function
         [] ->
            implem_error (sprintf "MLAxiom %s: not implemented" name)
       | h::t ->
            match h with
               MLAxiom { mlterm_name = name'; mlterm_term = term' } when name' = name ->
                  if not (alpha_equal term' term) then
                     implem_error (sprintf "MLAxiom %s: definition does not match" name)
             | _ ->
                  search t
      in
         search implem

   (*
    * Parent names must match.
    *)
   let check_parent
       (path : module_path)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
      let rec search = function
         [] ->
            implem_error (sprintf "Include %s: not implemented" (string_of_path path))
       | h::t ->
            match h with
               Parent { parent_name = path' } ->
                  if path' <> path then
                     search t
             | _ ->
                  search t
      in
         search implem

   (*
    * Display forms.
    *)
   let check_dform
       (tags : dform_option list)
       (term : term)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
      let rec search = function
         [] ->
            implem_error (sprintf "DForm %s: not implemented" (string_of_term term))
       | h::t ->
            match h with
               DForm { dform_options = tags'; dform_redex =  term' } ->
                  if alpha_equal term' term then
                     if tags' = tags then
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
   let check_prec (name : string)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
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
   let check_resource
       (info : 'ctyp1 resource_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
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
    * Infix declarations.
    *)
   let check_infix (name : string)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
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
    * Match the ids.
    *)
   let check_id (id : int)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
      let rec search = function
         [] ->
            implem_error "Id: not implemented"
       | h::t ->
            match h with
               Id id' ->
                  if id' <> id then
                     implem_error (sprintf "Implementation is out of date (recompile)")
             | _ ->
                  search t
      in
         search implem

   (*
    * Module definitions.
    *)
   let rec check_module
       (name : string)
       (info : ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1) module_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list) =
      let rec search = function
         [] ->
            implem_error (sprintf "Module %s: not implemented" name)
       | h::t ->
            match h with
               Module (name', info') ->
                  if name' = name then
                     check_implementation info' info
                  else
                     search t
             | _ ->
                  search t
      in
         search implem

   (*
    * Check that an item is implemented.
    *)
   and check_implemented
       (implem : ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) summary_item list)
       ((interf : ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1) summary_item), _) =
      match interf with
         SummaryItem _
       | ToploopItem _ ->
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
       | MLRewrite info ->
            check_mlrewrite info implem
       | MLAxiom info ->
            check_mlaxiom info implem
       | Parent { parent_name = path } ->
            check_parent path implem
       | Module (name, info) ->
            check_module name info implem
       | DForm { dform_options = flags; dform_redex = term } ->
            check_dform flags term implem
       | Prec name ->
            check_prec name implem
       | PrecRel _ ->
            ()
       | Resource info ->
            check_resource info implem
       | Infix name ->
            check_infix name implem
       | Id id ->
            check_id id implem
       | MagicBlock _ ->
            ()

   (*
    * Check that the implementation satisfies the interface.
    * This means that every item in the interface must be implemented.
    *)
   and check_implementation { info_list = implem } { info_list = interf } =
      List.iter (check_implemented (List.map fst implem)) interf

   (************************************************************************
    * PROOF COPYING                                                        *
    ************************************************************************)

   (*
    * Each of these functions takes two arguments:
    * the entry that was parsed from the .ml file,
    * and the entry that was read from the .prlb file.
    * The proofs from the saved .prlb files are copied.
    *)

   (*
    * Copy a rewrite proof if it exists.
    *)
   let copy_rw_proof copy_proof rw info2 =
      let { rw_name = name;
            rw_redex = redex1;
            rw_contractum = contractum1;
            rw_proof = proof1;
            rw_resources = res1;
          } = rw
      in
      let rw =
         match find_rewrite info2 name with
            Some (Rewrite { rw_redex = redex2;
                            rw_contractum = contractum2;
                            rw_proof = proof2
                  }, _) ->
               if not (alpha_equal redex1 redex2 & alpha_equal contractum1 contractum2) then
                  eprintf "copy_proof: warning: rewrites %s do not match%t" name eflush;
               { rw_name = name;
                 rw_redex = redex1;
                 rw_contractum = contractum1;
                 rw_proof = copy_proof proof1 proof2;
                 rw_resources = res1
               }
          | _ ->
               rw
      in
         Rewrite rw

   (*
    * Copy the cond_rewrite proof.
    *)
   let copy_crw_proof copy_proof crw info2 =
      let { crw_name = name;
            crw_params = params1;
            crw_args = args1;
            crw_redex = redex1;
            crw_contractum = contractum1;
            crw_proof = proof1;
            crw_resources = res1;
          } = crw
      in
      let crw =
         match find_rewrite info2 name with
            Some (CondRewrite { crw_params = params2;
                                crw_args = args2;
                                crw_redex = redex2;
                                crw_contractum = contractum2;
                                crw_proof = proof2
                  }, _) ->
               if not (check_params params1 params2)
                  or not (generalizes_list args2 args1)
                  or not (alpha_equal redex1 redex2)
                  or not (alpha_equal contractum1 contractum2)
               then
                  eprintf "copy_proof: warning: cond_rewrites %s do not match%t" name eflush;
               { crw_name = name;
                 crw_params = params1;
                 crw_args = args1;
                 crw_redex = redex1;
                 crw_contractum = contractum1;
                 crw_proof = copy_proof proof1 proof2;
                 crw_resources = res1;
               }
          | _ ->
               crw
      in
         CondRewrite crw

   (*
    * Copy the axioms.
    *)
   let copy_axiom_proof copy_proof ax info2 =
      let { axiom_name = name;
            axiom_stmt = stmt1;
            axiom_proof = proof1;
            axiom_resources = res1
          } = ax
      in
      let ax =
         match find_axiom info2 name with
            Some (Axiom { axiom_stmt = stmt2;
                          axiom_proof = proof2
                  }, _) ->
               if not (alpha_equal stmt1 stmt2) then
                  eprintf "copy_proof: warning: axioms %s do not match%t" name eflush;
               { axiom_name = name;
                 axiom_stmt = stmt1;
                 axiom_proof = copy_proof proof1 proof2;
                 axiom_resources = res1
               }
          | _ ->
               ax
      in
         Axiom ax

   (*
    * Copy the proof in a rule.
    *)
   let copy_rule_proof copy_proof rule info2 =
      let { rule_name = name;
            rule_params = params1;
            rule_stmt = stmt1;
            rule_proof = proof1;
            rule_resources = res1
          } = rule
      in
      let rule =
         match find_axiom info2 name with
            Some (Rule { rule_params = params2;
                         rule_stmt = stmt2;
                         rule_proof = proof2
                  }, _) ->
               if not (meta_alpha_equal stmt1 stmt2) then
                  begin
                     eprintf "copy_proof: warning: rules %s do not match%t" name eflush;
                     if !debug_match then
                        begin
                           eprintf "Term 1:\n\t";
                           prerr_simple_mterm stmt1;
                           eflush stderr;
                           eprintf "Term 2:\n\t";
                           prerr_simple_mterm stmt2;
                           eflush stderr
                        end
                  end;
               { rule_name = name;
                 rule_params = params1;
                 rule_stmt = stmt1;
                 rule_proof = copy_proof proof1 proof2;
                 rule_resources = res1
               }
          | _ ->
               rule
      in
         Rule rule

   (*
    * Copy the proofs from the second implementation into the first.
    *)
   let rec copy_proofs copy_proof { info_list = info1 } info2 =
      let copy copy_proof (item, loc) =
         let item =
            match item with
               Rewrite rw ->
                  copy_rw_proof copy_proof rw info2
             | CondRewrite crw ->
                  copy_crw_proof copy_proof crw info2
             | Axiom ax ->
                  copy_axiom_proof copy_proof ax info2
             | Rule rule ->
                  copy_rule_proof copy_proof rule info2
             | Opname info ->
                  Opname info
             | MLRewrite t ->
                  MLRewrite t
             | MLAxiom t ->
                  MLAxiom t
             | Parent p ->
                  Parent p
             | Module (name, info) ->
                  copy_module_proof copy_proof name info info2
             | DForm df ->
                  DForm df
             | Prec s ->
                  Prec s
             | PrecRel r ->
                  PrecRel r
             | Id i ->
                  Id i
             | Resource r ->
                  Resource r
             | Infix s ->
                  Infix s
             | SummaryItem item ->
                  SummaryItem item
             | ToploopItem item ->
                  ToploopItem item
             | MagicBlock info ->
                  MagicBlock info
         in
            item, loc
      in
         { info_list = List.map (copy copy_proof) info1 }

   and copy_module_proof copy_proof name info info2 =
      match find_module info2 name with
         Some (Module (_, info2), _) ->
            Module (name, copy_proofs copy_proof info info2)
       | _ ->
            Module (name, info)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
