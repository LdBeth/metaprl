(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_symbol
open Lm_printf

open Opname
open Term_sig
open Refiner_sig
open Precedence
open Dform
open Lexing
open Term_ty_sig

open Filter_util
open Filter_type

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_summary%t"

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
type ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item =
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info) summary_item_type

(*
 * The info about a specific module is just a list of items.
 *)
and ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info =
   { info_list : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc list }

(*
 * Pair it with a location.
 *)
and ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc =
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item * MLast.loc

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

let print_ty_param out = function
   TyNumber ->
      fprintf out "<N>"
 | TyString ->
      fprintf out "<S>"
 | TyToken t ->
      fprintf out "<T>"
 | TyLevel ->
      fprintf out "<L>"
 | TyVar ->
      fprintf out "<V>"
 | TyQuote ->
      fprintf out "@"

let print_ty_bterm out { ty_bvars = bvars } =
   fprintf out "<%d>" (List.length bvars)

let print_ty_term out ty_term =
   let { ty_opname = opname;
         ty_params = params;
         ty_bterms = bterms
       } = ty_term
   in
      fprintf out "%s[%a]{%a}" (**)
         (string_of_opname opname)
         (print_any_list print_ty_param) params
         (print_any_list print_ty_bterm) bterms

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
 | InputForm { rw_name = name } ->
      eprintf "IForm: %s\n" name
 | CondRewrite { crw_name = name } ->
      eprintf "CondRewrite: %s\n" name
 | Rule { rule_name = name } ->
      eprintf "Rule: %s\n" name
 | DeclareTypeClass (opname, _, _) ->
      eprintf "DeclareTypeClass: %s" (string_of_opname opname)
 | DeclareType (ty_term, _) ->
      eprintf "DeclareType: %a" print_ty_term ty_term
 | DeclareTerm ty_term ->
      eprintf "DeclareTerm: %a" print_ty_term ty_term
 | DefineTerm (ty_term, _) ->
      eprintf "DefineTerm: %a" print_ty_term ty_term
 | DeclareTypeRewrite _ ->
      eprintf "DeclareTypeRewrite"
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
 | Resource (name, _) ->
      eprintf "Resource: %s\n" name
 | Improve { improve_name = name } ->
      eprintf "Improve %s with ..." name
 | MLGramUpd (Infix name)
 | MLGramUpd (Suffix name) ->
      eprintf "Infix/Suffix: %s\n" name
 | Id id ->
      eprintf "Id: 0x%08x\n" id
 | MagicBlock { magic_name = name } ->
      eprintf "Magic: %s\n" name
 | Comment t ->
      eprintf "Comment\n"
 | PRLGrammar _ ->
      eprintf "PRLGrammar\n"

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
let test_axiom name = function
   (Rule { rule_name = n }, _) -> n = name
 | _ -> false

let find_axiom { info_list = summary } name =
   try Some (Lm_list_util.find (test_axiom name) summary) with
      Not_found ->
         None

(*
 * Find a rewrite in the summary.
 *)
let test_rewrite name (item, _) =
   match item with
      Rewrite { rw_name = n }
    | CondRewrite { crw_name = n } ->
         n = name
    | _ ->
         false

let find_rewrite { info_list = summary } name =
   try Some (Lm_list_util.find (test_rewrite name) summary) with
      Not_found ->
         None

(*
 * Find a input form rewrite in the summary.
 *)
let test_iform name (item, _) =
   match item with
      InputForm { rw_name = n } ->
         n = name
    | _ ->
         false

let find_iform { info_list = summary } name =
   try Some (Lm_list_util.find (test_iform name) summary) with
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
   try Some (Lm_list_util.find (test_mlrewrite name) summary) with
      Not_found ->
         None

let test_mlaxiom name = function
   MLAxiom { mlterm_name = name' }, _ ->
      name' = name
 | _ ->
      false

let find_mlaxiom { info_list = summary } name =
   try Some (Lm_list_util.find (test_mlaxiom name) summary) with
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
   try Some (Lm_list_util.find (test_module name) summary) with
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
   try Some (Lm_list_util.find (test_dform name) summary) with
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
   try Some (Lm_list_util.find (test_prec name) summary) with
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
let get_resources info =
   let rec search = function
      (Resource (name, r), _)::t ->
         (name,r)::search t
    | _::t ->
         search t
    | [] -> []
   in
      search info.info_list

(*
 * Get infix directives.
 *)
let get_infixes { info_list = summary } =
   let rec search = function
      (h, _)::t ->
         begin
            match h with
               MLGramUpd s -> Infix.Set.add (search t) s
             | _ -> search t
         end
    | [] ->
         Infix.Set.empty
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
               Rule { rule_name = name; rule_proof = pf }
             | Rewrite { rw_name = name; rw_proof = pf }
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
         Rule { rule_name = n }
       | Rewrite { rw_name = n }
       | CondRewrite { crw_name = n }
       | MLRewrite { mlterm_name = n }
       | InputForm { rw_name = n }
       | MLAxiom { mlterm_name = n }
       | DForm { dform_name = n }
       | Prec n ->
            n = name
       | _ ->
            false
   in
      Lm_list_util.find test summary

(*
 * Set an item by name.
 *)
let set_command info item =
   let test =
      match fst item with
         Rule { rule_name = name } ->
            test_axiom name
       | Rewrite { rw_name = name }
       | CondRewrite { crw_name = name } ->
            test_rewrite name
       | InputForm { rw_name = name } ->
            test_iform name
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
      { info_list = Lm_list_util.replace_first test item info.info_list }

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

let filter pred { info_list = info } =
   { info_list = Lm_list_util.filter pred info }

(*
 * Optional application.
 *)
let opt_apply f = function
   Some x ->
      Some (f x)
 | None ->
      None

let convert_resource convert
   (name, {
      resource_input = input;
      resource_output = output
   }) =
   name, {
      resource_input = convert.ctyp_f input;
      resource_output = convert.ctyp_f output
   }

let hash =
   let hash_aux (i: int) item = Hashtbl.hash_param 1000 10000 (i, item) in
      (fun info -> List.fold_left hash_aux 0 info.info_list)

(*
 * Normalize all terms in the info.
 *)
let summary_map (convert : ('term1, 'meta_term1, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1,
                            'term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) convert)
=
   (* Map the terms inside of params *)
   let param_map = function
      TermParam t -> TermParam (convert.term_f t)
    | (IntParam _ | AddrParam _) as p -> p
   in

   let convert_bnd = function
      s, BindTerm t -> s, BindTerm (convert.term_f t)
    | _, BindOpname _
    | _, BindNum _ as d -> d
   in

   let convert_bnd_expr expr =
      { item_item = convert.expr_f expr.item_item;
        item_bindings = List.map convert_bnd expr.item_bindings;
      }
   in

   let res_map res =
      { item_item = List.map (fun (loc, name, exprs) -> (loc, name, List.map convert.expr_f exprs)) res.item_item;
        item_bindings = List.map convert_bnd res.item_bindings;
      }
   in

   let convert_ty_param ty_param =
      match ty_param with
         TyToken t ->
            TyToken (convert.term_f t)
       | TyNumber
       | TyString
       | TyLevel
       | TyVar
       | TyQuote as param ->
            param
   in
   let convert_ty_bterm ty_bterm =
      let { ty_bvars = bvars; ty_bterm = term } = ty_bterm in
         { ty_bvars = List.map convert.term_f bvars;
           ty_bterm = convert.term_f term
         }
   in
   let convert_ty_term ty_term =
      let { ty_term   = term;
            ty_opname = opname;
            ty_params = params;
            ty_bterms = bterms;
            ty_type   = ty
          } = ty_term
      in
         { ty_term   = convert.term_f term;
           ty_opname = opname;
           ty_params = List.map convert_ty_param params;
           ty_bterms = List.map convert_ty_bterm bterms;
           ty_type   = convert.term_f ty
         }
   in
   let convert_term_def term_def =
      let { term_def_name = name;
            term_def_value = t;
            term_def_resources = res
          } = term_def
      in
         { term_def_name = name;
           term_def_value = convert.term_f t;
           term_def_resources = res_map res
         }
   in

   (* Map a summary item *)
   let rec item_map (item, loc) =
      let item =
         match item with
            SummaryItem item ->
               SummaryItem {
                  item_bindings = List.map convert_bnd item.item_bindings;
                  item_item = convert.item_f item.item_item;
               }

          | ToploopItem t ->
               ToploopItem (convert.item_f t)

          | Rewrite { rw_name = name; rw_redex = redex; rw_contractum = con; rw_proof = pf ; rw_resources = res } ->
               Rewrite { rw_name = name;
                         rw_redex = convert.term_f redex;
                         rw_contractum = convert.term_f con;
                         rw_proof = convert.proof_f name pf;
                         rw_resources = res_map res;
               }

          | InputForm { rw_name = name; rw_redex = redex; rw_contractum = con; rw_proof = pf ; rw_resources = res } ->
               InputForm { rw_name = name;
                           rw_redex = convert.term_f redex;
                           rw_contractum = convert.term_f con;
                           rw_proof = convert.proof_f name pf;
                           rw_resources = res_map res;
               }

          | CondRewrite crw ->
               CondRewrite { crw_name = crw.crw_name;
                             crw_params = List.map param_map crw.crw_params;
                             crw_args = List.map convert.term_f crw.crw_args;
                             crw_redex = convert.term_f crw.crw_redex;
                             crw_contractum = convert.term_f crw.crw_contractum;
                             crw_proof = convert.proof_f crw.crw_name crw.crw_proof;
                             crw_resources = res_map crw.crw_resources;
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

          | DeclareType (ty_term, ty_opname) ->
               DeclareType (convert_ty_term ty_term, ty_opname)
          | DeclareTerm ty_term ->
               DeclareTerm (convert_ty_term ty_term)
          | DefineTerm (ty_term, term_def) ->
               DefineTerm (convert_ty_term ty_term, convert_term_def term_def)
          | DeclareTypeRewrite (redex, contractum) ->
               DeclareTypeRewrite (convert.term_f redex, convert.term_f contractum)

          | MLRewrite t ->
               MLRewrite { t with
                           mlterm_params = List.map param_map t.mlterm_params;
                           mlterm_term = convert.term_f t.mlterm_term;
                           mlterm_def = opt_apply convert_bnd_expr t.mlterm_def;
                           mlterm_resources = res_map t.mlterm_resources;
               }

          | MLAxiom t ->
               MLAxiom { t with
                         mlterm_params = List.map param_map t.mlterm_params;
                         mlterm_term = convert.term_f t.mlterm_term;
                         mlterm_def = opt_apply convert_bnd_expr t.mlterm_def;
                         mlterm_resources = res_map t.mlterm_resources;
               }

          | Parent par ->
               Parent { par with parent_resources = List.map (convert_resource convert) par.parent_resources }

          | Module (name, info) ->
               Module (name, map info)

          | DForm df ->
               let def =
                  match df.dform_def with
                     NoDForm ->
                        NoDForm
                   | TermDForm t ->
                        TermDForm (convert.term_f t)
                   | MLDForm mldf ->
                        MLDForm { mldf with dform_ml_code = convert_bnd_expr mldf.dform_ml_code }
               in
                  DForm { df with dform_redex = convert.term_f df.dform_redex;
                                  dform_def = def
                  }

          | Prec _
          | PrecRel _
          | Id _
          | MLGramUpd _
          | DeclareTypeClass _
          | PRLGrammar _ as item ->
               item

          | Resource (name, r) ->
               Resource (name, convert.resource_f r)

          | Improve { improve_name = name; improve_expr = expr } ->
               Improve { improve_name = name; improve_expr = convert_bnd_expr expr }

          | MagicBlock { magic_name = name;
                         magic_code = items
            } ->
               MagicBlock { magic_name = name;
                            magic_code = List.map convert.item_f items
               }

          | Comment t ->
               Comment (convert.term_f t)

      in
         item, loc

   and map info = { info_list = List.map item_map info.info_list } in
      map

(*
 * Add a command to the info.
 *)
let add_command { info_list = info } item =
   { info_list = item::info }

let add_prefix_commands { info_list = info } items =
   { info_list = info @ List.rev items }

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

(*
 * These are the possible opnames.
 *)
let mk_opname =
   let op = Opname.mk_opname "Summary" Opname.nil_opname in
      fun s -> Opname.mk_opname s op

let rewrite_op                 = mk_opname "rewrite"
let input_form_op              = mk_opname "input_form"
let cond_rewrite_op            = mk_opname "cond_rewrite"
let rule_op                    = mk_opname "rule"
let res_op                     = mk_opname "resource_defs"
let declare_typeclass_op       = mk_opname "declare_typeclass"
let declare_type_op            = mk_opname "declare_type"
let declare_term_op            = mk_opname "declare_term"
let declare_type_rewrite_op    = mk_opname "declare_type_rewrite"
let ty_term_op                 = mk_opname "ty_term"
let term_def_op                = mk_opname "term_def"
let parent_kind_op             = mk_opname "parent_kind"
let define_term_op             = mk_opname "define_term"
let ty_param_op                = mk_opname "ty_param"
let ty_bterm_op                = mk_opname "ty_bterm"
let mlrewrite_op               = mk_opname "mlrewrite"
let mlaxiom_op                 = mk_opname "mlaxiom"
let parent_op                  = mk_opname "parent"
let module_op                  = mk_opname "module"
let dform_op                   = mk_opname "dform"
let prec_op                    = mk_opname "prec"
let prec_rel_op                = mk_opname "prec_rel"
let id_op                      = mk_opname "id"
let comment_op                 = mk_opname "comment"
let resource_op                = mk_opname "resource"
let infix_op                   = mk_opname "infix"
let suffix_op                  = mk_opname "suffix"
let magic_block_op             = mk_opname "magic_block"
let summary_item_op            = mk_opname "summary_item"
let term_binding_op            = mk_opname "term_binding"
let opname_binding_op          = mk_opname "opname_binding"
let num_binding_op             = mk_opname "num_binding"
let toploop_item_op            = mk_opname "toploop_item"
let improve_op                 = mk_opname "improve"

(* XXX HACK: ASCII_IO format <= 1.0.17 had "opname" and "definition" *)
let opname_op                  = mk_opname "opname"
let definition_op              = mk_opname "definition"


(*
 * Meta term conversions.
 *)
let meta_theorem_op     = mk_opname "meta_theorem"
let meta_implies_op     = mk_opname "meta_implies"
let meta_function_op    = mk_opname "meta_function"
let meta_iff_op         = mk_opname "meta_iff"
let meta_labeled_op     = mk_opname "meta_labeled"

module FilterSummaryTerm (ToTerm : RefinerSig) =
struct
   open ToTerm.Term
   open ToTerm.TermType
   open ToTerm.TermOp
   open ToTerm.TermMan
   open ToTerm.TermSubst
   open ToTerm.TermShape
   open ToTerm.TermTy
   open ToTerm.TermMeta
   open ToTerm.RefineError

   module SimplePrint = Simple_print.MakeSimplePrint (ToTerm);;
   open SimplePrint

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
                        MetaLabeled (l, meta_term_of_term t)
                   | Var l, { bvars = []; bterm = t } ->
                        MetaLabeled (string_of_symbol l, meta_term_of_term t)
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
    * XXX: TODO: This converts the old-style location data into the modern one.
    * Ideally, we should be able to embed location data as comments (bug 256).
    *)
   let loc_op = mk_opname "location"

   let dest_loc t =
      let i, j, t = dest_number_number_dep0_any_term t in
         if Lm_num.is_integer_num i && Lm_num.is_integer_num j then
            t, (mk_proper_loc i j)
         else
            raise (Failure "dest_loc: location is not an integer")

   let dest_loc_string t =
      let i, j, s, t = dest_number_number_string_dep0_any_term t in
         if Lm_num.is_integer_num i && Lm_num.is_integer_num j then
            t, (mk_proper_loc i j), s
         else
            raise (Failure "dest_loc_string: location is not an integer")

   (*
    * PRL bindings
    *)

   let rec dest_bindings convert bnds t =
      let opname = opname_of_term t in
         if Opname.eq opname term_binding_op then
            let v, t, t' = dest_dep0_dep1_any_term t in
               dest_bindings convert ((string_of_symbol v, BindTerm (convert.term_f t))::bnds) t'
         else if Opname.eq opname opname_binding_op then
            let v, t, t' = dest_dep0_dep1_any_term t in
               dest_bindings convert ((string_of_symbol v, BindOpname (opname_of_term t))::bnds) t'
         else if Opname.eq opname num_binding_op then
            let n, v, t = dest_number_dep1_any_term t in
               dest_bindings convert ((string_of_symbol v, BindNum n) :: bnds) t
         else
            bnds, t

   let dest_bnd_item convert item_f t =
      let bnds, t = dest_bindings convert [] t in {
         item_bindings = bnds;
         item_item = item_f t;
      }

   let dest_bnd_expr convert t = dest_bnd_item convert convert.expr_f t

   (*
    * Dform options.
    *
    * XXX HACK: the dform_internal_op is there only for backwards compativility with
    * old files (ASCII formats v. <= 1.0.11)
    *)
   let dform_inherit_op     = mk_opname "inherit_df"
   let dform_prec_op        = mk_opname "prec_df"
   let dform_parens_op      = mk_opname "parens_df"
   let dform_mode_op        = mk_opname "mode_df"
   let dform_except_mode_op = mk_opname "except_mode_df"
   let dform_internal_op    = mk_opname "internal_df" (* XXX: HACK: obsolete *)

   let dest_dform_opt tl =
      let modes = ref [] in
      let except = ref [] in
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
            else if Opname.eq opname dform_mode_op && (!except)=[] then
               push modes (dest_string_param t)
            else if Opname.eq opname dform_except_mode_op && (!modes)=[] then
               push except (dest_string_param t)
            else if Opname.eq opname dform_internal_op then (* XXX: HACK: obsolete *)
               ()
            else
               raise (Invalid_argument "Dform option is not valid")
      in
         List.iter dest_opt tl;
         begin match !modes, !except with
            [], [] -> AllModes
          | [], except -> ExceptModes except
          | modes, [] -> Modes modes
          | _ -> raise (Invalid_argument "dest_dform_opt")
         end, List.rev !options

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
            let printer, buffer, expr = dest_string_string_dep0_any_term t in
               MLDForm { dform_ml_printer   = printer;
                         dform_ml_buffer    = buffer;
                         dform_ml_code      = dest_bnd_expr convert expr
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
      List.map dest_string_param (dest_xlist t)

   (*
    * Get a parameter.
    *)
   let int_param_op    = mk_opname "int_param"
   let addr_param_op    = mk_opname "addr_param"
   let term_param_op       = mk_opname "term_param"

   let dest_rule_param =
      let var_param pl =
         match dest_params pl with
            [Var v] -> v
          | _ -> raise (Failure "Invalid parameters")
      in fun convert t ->
      let { term_op = op } = dest_term t in
      let { op_name = opname; op_params = params } = dest_op op in
         if Opname.eq opname int_param_op then
            IntParam (var_param params)
         else if Opname.eq opname addr_param_op then
            AddrParam (var_param params)
         else if Opname.eq opname term_param_op then
            TermParam (convert.term_f (one_subterm t))
         else
            raise (Failure "Illegal parameter")

   (*
    * Get the parameter list.
    * XXX HACK: the try wrapper is only there for backwards compativility with
    * old files (ASCII formats <= 1.0.15)
    *)
   let dest_rule_params convert t =
      try
         List.map (dest_rule_param convert) (dest_xlist t)
      with
         Failure _ ->
            []

   (*
    * Get the list of resource updates
    *)
   let dest_resource_term expr_f t =
      let t, loc, name = dest_loc_string t in
         loc, name, List.map expr_f (dest_xlist t)

   let dest_res_inner convert t =
      List.map (dest_resource_term convert.expr_f) (dest_xlist t)

   let dest_res convert t =
      dest_bnd_item convert (dest_res_inner convert) t

   (*
    * Collect a rewrite.
    *)
   let rec dest_rewrite convert t =
      let name = dest_string_param t in
      let redex, contractum, proof, res = four_subterms t in
         { rw_name = name;
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
                       crw_params = dest_rule_params convert params;
                       crw_args = List.map convert.term_f (dest_xlist args);
                       crw_redex = convert.term_f redex;
                       crw_contractum = convert.term_f contractum;
                       crw_proof = convert.proof_f name proof;
                       crw_resources = dest_res convert res
         }

   (*
    * Rule.
    *)
   and dest_rule convert t =
      let name = dest_string_param t in
      let params, stmt, proof, res = four_subterms t in
         Rule { rule_name = name;
                rule_params = dest_rule_params convert params;
                rule_stmt = convert.meta_term_f stmt;
                rule_proof = convert.proof_f name proof;
                rule_resources = dest_res convert res
         }

   and dest_ty_param convert t =
      let name = dest_string_param t in
         match name with
            "n" ->
               TyNumber
          | "s" ->
               TyString
          | "t" ->
               TyToken (convert.term_f (one_subterm t))
          | "l" ->
               TyLevel
          | "v" ->
               TyVar
          | "@" ->
               TyQuote
          | _ ->
               raise (Failure ("illegal class parameter: " ^ name))

   and dest_ty_bterm convert t =
      let bvars, bterm = two_subterms t in
         { ty_bvars = List.map convert.term_f (dest_xlist bvars);
           ty_bterm = convert.term_f bterm
         }

   and dest_ty_def convert t =
      let term, opname, params, bterms, ty = five_subterms t in
         { ty_term   = convert.term_f term;
           ty_opname = opname_of_term opname;
           ty_params = List.map (dest_ty_param convert) (dest_xlist params);
           ty_bterms = List.map (dest_ty_bterm convert) (dest_xlist bterms);
           ty_type   = convert.term_f ty
         }

   and dest_term_def convert t =
      let name = dest_string_param t in
      let t, res = two_subterms t in
         { term_def_name = name;
           term_def_value = convert.term_f t;
           term_def_resources = dest_res convert res
         }

   and dest_typeclass_parent t =
      let s = dest_string_param t in
         match s with
            "extends" ->
               ParentExtends (opname_of_term (one_subterm t))
          | "include" ->
               ParentInclude (opname_of_term (one_subterm t))
          | "none" ->
               ParentNone
          | _ ->
               raise (Failure ("bad kind parent: " ^ s))

   and dest_declare_typeclass convert t =
      let typeclass_opname, typeclass_type_opname, parent = three_subterms t in
      let typeclass_opname = opname_of_term typeclass_opname in
      let typeclass_type_opname = opname_of_term typeclass_type_opname in
      let parent = dest_typeclass_parent parent in
         DeclareTypeClass (typeclass_opname, typeclass_type_opname, parent)

   and dest_declare_type convert t =
      let ty_def, ty_opname = two_subterms t in
      let ty_def = dest_ty_def convert ty_def in
      let ty_opname = opname_of_term ty_opname in
         DeclareType (ty_def, ty_opname)

   and dest_declare_term convert t =
      DeclareTerm (dest_ty_def convert (one_subterm t))

   and dest_define_term convert t =
      let ty_def, term_def = two_subterms t in
         DefineTerm (dest_ty_def convert ty_def, dest_term_def convert term_def)

   and dest_declare_type_rewrite_term convert t =
      let redex, contractum = two_subterms t in
         DeclareTypeRewrite (convert.term_f redex, convert.term_f contractum)

   (*
    * ML Term.
    *)
   and dest_mlrewrite convert t =
      let name = dest_string_param t in
      let params, term, expr, resources = four_subterms t in
         MLRewrite { mlterm_name = name;
                     mlterm_params = dest_rule_params convert params;
                     mlterm_term = convert.term_f term;
                     mlterm_def = dest_opt (dest_bnd_expr convert) expr;
                     mlterm_resources = dest_res convert resources
         }

   and dest_mlaxiom convert t =
      let name = dest_string_param t in
      let params, term, expr, resources = four_subterms t in
         MLAxiom { mlterm_name = name;
                   mlterm_params = dest_rule_params convert params;
                   mlterm_term = convert.term_f term;
                   mlterm_def = dest_opt (dest_bnd_expr convert) expr;
                   mlterm_resources = dest_res convert resources
         }

   (*
    * Parent declaration.
    * XXX HACK: The "if" is here because parent terms used to have 3 subterms in old files
    * (ASCII IO format <= 1.0.8)
    *)
   and dest_parent convert t =
      let path, resources =
         if is_dep0_dep0_term parent_op t then
            two_subterms t
         else
            let p, _, r = three_subterms t in
               p, r
      in
         Parent { parent_name = dest_string_param_list path;
                  parent_resources = List.map (dest_resource_sig convert) (dest_xlist resources)
         }

   and dest_summary_item convert t =
      SummaryItem (dest_bnd_item convert convert.item_f (one_subterm t))

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
      match dest_params (dest_op (dest_term t).term_op).op_params with
         [String rel; String left; String right] ->
            let rel =
               match rel with
                  "none" -> NoRelation
                | "lt" -> LTRelation
                | "eq" -> EQRelation
                | "gt" -> GTRelation
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
         if Lm_num.is_integer_num n then Id (Lm_num.int_of_num n)
         else raise (Invalid_argument "Filter_summary.dest_id: not an int")

   (*
    * Resources.
    *)
   and dest_resource_sig convert t =
      let name = dest_string_param t in
      let input, output = two_subterms t in
      name, {
         resource_input = convert.ctyp_f input;
         resource_output = convert.ctyp_f output
      }

   and dest_resource convert t =
      Resource (dest_string_param t, convert.resource_f (one_subterm t))

   and dest_improve convert t =
      Improve {
         improve_name = dest_string_param t;
         improve_expr = dest_bnd_expr convert (one_subterm t)
      }

   (*
    * Magic block of items.
    *)
   and dest_magic_block convert t =
      MagicBlock { magic_name = dest_string_param t;
                   magic_code = List.map convert.item_f (dest_xlist (one_subterm t))
      }

   and dest_term_aux
       (convert : (term, term, term, term, term, term, term,
                   'term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) convert)
       (t : term) =
      let opname = opname_of_term t in
         try
            let info =
               if Opname.eq opname rewrite_op then
                  Rewrite (dest_rewrite convert t)
               else if Opname.eq opname cond_rewrite_op then
                  dest_cond_rewrite convert t
               else if Opname.eq opname rule_op then
                  dest_rule convert t
               else if Opname.eq opname dform_op then
                  dest_dform convert t
               else if Opname.eq opname declare_typeclass_op then
                  dest_declare_typeclass convert t
               else if Opname.eq opname declare_type_op then
                  dest_declare_type convert t
               else if Opname.eq opname declare_term_op then
                  dest_declare_term convert t
               else if Opname.eq opname define_term_op then
                  dest_define_term convert t
               else if Opname.eq opname declare_type_rewrite_op then
                  dest_declare_type_rewrite_term convert t
               else if Opname.eq opname mlrewrite_op then
                  dest_mlrewrite convert t
               else if Opname.eq opname parent_op then
                  dest_parent convert t
               else if Opname.eq opname prec_op then
                  dest_prec convert t
               else if Opname.eq opname prec_rel_op then
                  dest_prec_rel convert t
               else if Opname.eq opname id_op then
                  dest_id convert t
               else if Opname.eq opname comment_op then
                  Comment (convert.term_f (one_subterm t))
               else if Opname.eq opname resource_op then
                  dest_resource convert t
               else if Opname.eq opname improve_op then
                  dest_improve convert t
               else if Opname.eq opname infix_op then
                  MLGramUpd (Infix (dest_string_param t))
               else if Opname.eq opname suffix_op then
                  MLGramUpd (Suffix (dest_string_param t))
               else if Opname.eq opname magic_block_op then
                  dest_magic_block convert t
               else if Opname.eq opname summary_item_op then
                  dest_summary_item convert t
               else if Opname.eq opname toploop_item_op then
                  ToploopItem (convert.item_f (one_subterm t))
               else if Opname.eq opname module_op then
                  dest_module convert t
               else if Opname.eq opname mlaxiom_op then
                  dest_mlaxiom convert t
               else if Opname.eq opname input_form_op then
                  InputForm (dest_rewrite convert t)
               else
                  raise (Failure "term is not found")
            in
               Some info
         with
            Failure _ ->
               (* XXX HACK: the "if" line is for ASCII_IO format <= 1.0.17 *)
               if not ((Opname.eq opname opname_op) || (Opname.eq opname definition_op)) then
                  eprintf "Filter_summary.dest_term: incorrect syntax for %s%t" (string_of_opname opname) eflush;
               None
          | RefineError (x, TermMatchError (t', _)) ->
               raise (RefineError (x, TermPairError (t, t')))

   and dest_term_loc
       (convert : (term, term, term, term, term, term, term,
                   'term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) convert)
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
       (convert : (term, term, term, term, term, term, term,
                   'term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) convert)
       (terms : term list) =
      let items = Lm_list_util.some_map (dest_term_loc convert) terms in
         ({ info_list = List.rev items } : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info)

   (**************
    * CONSTRUCTION
    *)

   (*
    * Make a location.
    * XXX: TODO: This converts the modern location data into the old-style one.
    * Ideally, we should be able to embed location data as comments (bug 256).
    *)
   let mk_loc (i, j) t =
      mk_number_number_dep0_term loc_op (Lm_num.num_of_int i.pos_cnum) (Lm_num.num_of_int j.pos_cnum) t

   let mk_loc_string_term op (start, finish) name t =
      mk_number_number_string_dep0_term op (Lm_num.num_of_int start.pos_cnum) (Lm_num.num_of_int finish.pos_cnum) name t

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

   (*
    * Make a term with only strings parameters.
    *)
   let mk_strings_term opname l =
      mk_xlist_term (List.map (fun s -> mk_string_param_term opname s []) l)

   (*
    * Parameters.
    *)
   let mk_param convert = function
      IntParam v ->
         mk_term (mk_op int_param_op [make_param (Var v)]) []
    | AddrParam v ->
         mk_term (mk_op addr_param_op [make_param (Var v)]) []
    | TermParam t ->
         mk_simple_term term_param_op [convert.term_f t]

   let mk_params convert params =
      mk_xlist_term (List.map (mk_param convert) params)

   (*
    * PRL bindings
    *)
   let rec term_of_bindings convert t = function
      [] -> t
    | (v, BindTerm t') :: tl ->
         term_of_bindings convert (mk_dep0_dep1_term term_binding_op (Lm_symbol.add v) (convert.term_f t') t) tl
    | (v, BindOpname op) :: tl ->
         let t' = mk_simple_term op [] in
            term_of_bindings convert (mk_dep0_dep1_term opname_binding_op (Lm_symbol.add v) t' t) tl
    | (v, BindNum n) :: tl ->
         term_of_bindings convert (mk_number_dep1_term num_binding_op n (Lm_symbol.add v) t) tl

   and mk_bnd_expr convert expr =
      term_of_bindings convert (convert.expr_f expr.item_item) expr.item_bindings

   (*
    * Display form options.
    *)
   let mk_dform_mode mode =
      mk_string_param_term dform_mode_op mode []

   let mk_dform_except_mode mode =
      mk_string_param_term dform_except_mode_op mode []

   let mk_dform_opt = function
      DFormInheritPrec ->
         mk_simple_term dform_prec_op []
    | DFormPrec s ->
         mk_string_param_term dform_prec_op s []
    | DFormParens ->
         mk_simple_term dform_parens_op []

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
                dform_ml_code = expr
      } ->
         mk_string_string_dep0_term dform_ml_op printer buffer (mk_bnd_expr convert expr)

   (*
    * Precedence relation.
    *)
   let mk_prec_rel_term rel left right =
      let rel =
         match rel with
            NoRelation -> "none"
          | LTRelation -> "lt"
          | EQRelation -> "eq"
          | GTRelation -> "gt"
      in
         mk_term (mk_op prec_rel_op (List.map ToTerm.Term.make_param [String rel; String left; String right])) []

   (*
    * Term conversions.
    *)
   let rec term_of_res expr_f (loc, name, args) =
      mk_loc_string_term res_op loc name (mk_xlist_term (List.map expr_f args))

   and term_of_resources convert res =
      term_of_bindings convert (mk_xlist_term (List.map (term_of_res convert.expr_f) res.item_item)) res.item_bindings

   and term_of_rewrite op convert { rw_name = name;
                                    rw_redex = redex;
                                    rw_contractum = con;
                                    rw_proof = pf;
                                    rw_resources = res
       } =
      mk_string_param_term op name [convert.term_f redex;
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

   and term_of_mlrewrite convert { mlterm_name = name;
                                   mlterm_params = params;
                                   mlterm_term = term;
                                   mlterm_def = expr_opt;
                                   mlterm_resources = res
       } =
      mk_string_param_term mlrewrite_op name (**)
         [mk_params convert params;
          convert.term_f term;
          mk_opt (mk_bnd_expr convert) expr_opt;
          term_of_resources convert res]

   and term_of_mlaxiom convert { mlterm_name = name;
                                 mlterm_params = params;
                                 mlterm_term = term;
                                 mlterm_def = expr_opt;
                                 mlterm_resources = res
       } =
      mk_string_param_term mlaxiom_op name (**)
         [mk_params convert params;
          convert.term_f term;
          mk_opt (mk_bnd_expr convert) expr_opt;
          term_of_resources convert res]

   and term_of_parent convert { parent_name = path;
                                parent_resources = resources
       } =
      mk_simple_term parent_op (**)
         [mk_strings_term parent_op path;
          mk_xlist_term (List.map (term_of_resource_sig convert) resources)]

   and term_of_dform convert { dform_name = name;
                               dform_modes = modes;
                               dform_options = options;
                               dform_redex = redex;
                               dform_def = def
       } =
      let modes = match modes with
         Modes modes -> List.map mk_dform_mode modes
       | ExceptModes modes -> List.map mk_dform_except_mode modes
       | AllModes -> []
       | PrimitiveModes -> []
      in
      let options = List.map mk_dform_opt options in
         mk_string_param_term dform_op name [mk_xlist_term (modes @ options);
                                             convert.term_f redex;
                                             mk_dform_def convert def]

   and term_of_resource_sig convert
      (name, { resource_input = input;
               resource_output = output
       }) =
      mk_string_param_term resource_op name [
         convert.ctyp_f input;
         convert.ctyp_f output
      ]

   and term_of_typeclass_parent = function
      ParentExtends opname ->
         let t = mk_term (mk_op opname []) [] in
            mk_string_param_term parent_kind_op "extends" [t]
    | ParentInclude opname ->
         let t = mk_term (mk_op opname []) [] in
            mk_string_param_term parent_kind_op "include" [t]
    | ParentNone ->
         mk_string_param_term parent_kind_op "none" []

   and term_of_declare_typeclass convert opname type_opname parent =
      let t1 = mk_term (mk_op opname []) [] in
      let t2 = mk_term (mk_op type_opname []) [] in
      let p = term_of_typeclass_parent parent in
         mk_simple_term declare_typeclass_op [t1; t2; p]

   and mk_ty_param_term convert param =
      match param with
         TyNumber ->
            mk_string_param_term ty_param_op "n" []
       | TyString ->
            mk_string_param_term ty_param_op "s" []
       | TyToken t ->
            mk_string_param_term ty_param_op "t" [convert.term_f t]
       | TyLevel ->
            mk_string_param_term ty_param_op "l" []
       | TyVar ->
            mk_string_param_term ty_param_op "v" []
       | TyQuote ->
            mk_string_param_term ty_param_op "@" []

   and mk_ty_bterm_term convert bterm =
      let { ty_bvars = bvars; ty_bterm = term } = bterm in
      let bvars = mk_xlist_term (List.map convert.term_f bvars) in
      let term = convert.term_f term in
         mk_simple_term ty_bterm_op [bvars; term]

   and mk_ty_term_term convert ty_term =
      let { ty_term   = term;
            ty_opname = opname;
            ty_params = params;
            ty_bterms = bterms;
            ty_type   = ty
          } = ty_term
      in
      let term = convert.term_f term in
      let opname = mk_term (mk_op opname []) [] in
      let params = mk_xlist_term (List.map (mk_ty_param_term convert) params) in
      let bterms = mk_xlist_term (List.map (mk_ty_bterm_term convert) bterms) in
      let ty = convert.term_f ty in
         mk_simple_term ty_term_op [term; opname; params; bterms; ty]

   and mk_term_def_term convert term_def =
      let { term_def_name = name;
            term_def_value = term;
            term_def_resources = res
          } = term_def
      in
      let term = convert.term_f term in
      let res = term_of_resources convert res in
         mk_string_param_term term_def_op name [term; res]

   and term_of_declare_type convert ty_term ty_opname =
      let term = mk_ty_term_term convert ty_term in
      let ty_opname = mk_term (mk_op ty_opname []) [] in
         mk_simple_term declare_type_op [term; ty_opname]

   and term_of_declare_term convert ty_term =
      let term = mk_ty_term_term convert ty_term in
         mk_simple_term declare_term_op [term]

   and term_of_define_term convert ty_term term_def =
      let ty_term = mk_ty_term_term convert ty_term in
      let term_def = mk_term_def_term convert term_def in
         mk_simple_term define_term_op [ty_term; term_def]

   and term_of_declare_type_rewrite convert redex contractum =
      mk_simple_term declare_type_rewrite_op  [convert.term_f redex; convert.term_f contractum]

   and term_of_item convert item =
      mk_simple_term summary_item_op [term_of_bindings convert (convert.item_f item.item_item) item.item_bindings]

   (*
    * Convert the items to a term.
    *)
   and term_list_aux (convert : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item, term,
                                 term, term, term, term, term, term) convert) = function
      SummaryItem item ->
         term_of_item convert item
    | ToploopItem item ->
         mk_simple_term toploop_item_op [convert.item_f item]
    | Rewrite rw ->
         term_of_rewrite rewrite_op convert rw
    | InputForm rw ->
         term_of_rewrite input_form_op convert rw
    | CondRewrite crw ->
         term_of_cond_rewrite convert crw
    | Rule rw ->
         term_of_rule convert rw
    | DeclareTypeClass (opname, type_opname, parent) ->
         term_of_declare_typeclass convert opname type_opname parent
    | DeclareType (ty_term, ty_opname) ->
         term_of_declare_type convert ty_term ty_opname
    | DeclareTerm ty_term ->
         term_of_declare_term convert ty_term
    | DefineTerm (ty_term, term_def) ->
         term_of_define_term convert ty_term term_def
    | DeclareTypeRewrite (redex, contractum) ->
         term_of_declare_type_rewrite convert redex contractum
    | MLRewrite t ->
         term_of_mlrewrite convert t
    | MLAxiom cond ->
         term_of_mlaxiom convert cond
    | Parent par ->
         term_of_parent convert par
    | Module (name, info) ->
         mk_string_param_term module_op name [mk_xlist_term (term_list convert info)]
    | DForm df ->
         term_of_dform convert df
    | Prec p ->
         mk_string_param_term prec_op p []
    | PrecRel { prec_rel = rel; prec_left = left; prec_right = right } ->
         mk_prec_rel_term rel left right
    | Id id ->
         mk_number_term id_op (Lm_num.num_of_int id)
    | Resource (name, r) ->
         mk_string_param_term resource_op name [convert.resource_f r]
    | Improve { improve_name = name; improve_expr = expr } ->
         mk_string_param_term improve_op name [mk_bnd_expr convert expr]
    | MLGramUpd (Infix op) ->
         mk_string_param_term infix_op op []
    | MLGramUpd (Suffix op) ->
         mk_string_param_term suffix_op op []
    | MagicBlock { magic_name = name; magic_code = items } ->
         mk_string_param_term magic_block_op name [mk_xlist_term (List.map convert.item_f items)]
    | Comment t ->
         mk_simple_term comment_op [convert.term_f t]
    | PRLGrammar _ ->
         raise (Invalid_argument "Filter_summary.term_list_aux")

   and term_list_loc convert (t, loc) =
      mk_loc loc (term_list_aux convert t)

   and term_list (convert : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
                             term, term, term, term, term, term, term) convert)
       ({ info_list = info } : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info) =
      List.fold_left (fun terms item ->
            (*
             * XXX: TODO:
             * This is the place where we drop things that are part of .cmoz, but should
             * not be included in .prlb/.prla
             * At some point we should be dropping all the stuff that can be recovered from
             * .ml[i] in here
             *)
            match item with
               PRLGrammar _, _ ->
                  (* Don't include the grammar *)
                  terms
             | _ ->
                  term_list_loc convert item :: terms) [] info

   (************************************************************************
    * SUBTYPING                                                            *
    ************************************************************************)

   (*
    * Print an error and raise an exception.
    *)
   let implem_error s =
      raise (IterfImplemMismatch s)

   (*
    * Check parameter lists.
    *)
   let check_params int_params imp_params =
      let check int_param imp_param =
         match int_param, imp_param with
            IntParam _, IntParam _
          | AddrParam _, AddrParam _
          | TermParam _, TermParam _ ->
               true
          | _ ->
               false
      in
         Lm_list_util.for_all2 check int_params imp_params

   (*
    * Check that a rewrite is justified.
    *)
   let check_rewrite loc
       (info : ('term1, 'proof1, 'expr1) rewrite_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let { rw_name = name; rw_redex = redex; rw_contractum = con } = info in
      let rec search = function
         [] ->
            implem_error (sprintf "Rewrite %s: not implemented" name)
       | Rewrite { rw_name = name'; rw_redex = redex'; rw_contractum = con' } :: _ when name = name' ->
            redex', con'
       | DefineTerm (ty_term, { term_def_name = name'; term_def_value = con' }) :: _ when name = name' ->
            (term_of_ty ty_term), con'
       | _ :: t ->
            search t
      in
      let redex', con' = search implem in
         if not (alpha_equal redex' redex) then
            implem_error (sprintf "Rewrite %s: redex mismatch:\n%s\nshould be\n%s\n" (**)
                             name (string_of_term redex') (string_of_term redex));
         if not (alpha_equal con' con) then
            implem_error (sprintf "Rewrite %s: contractum mismatch:\n%s\nshould be\n%s\n" (**)
                             name (string_of_term con') (string_of_term con));
         items

   let check_iform loc
       (info : ('term1, 'proof1, 'expr1) rewrite_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let { rw_name = name; rw_redex = redex; rw_contractum = con } = info in
      let rec search = function
         [] ->
            implem_error (sprintf "Rewrite %s: not implemented" name)
       | InputForm { rw_name = name'; rw_redex = redex'; rw_contractum = con' } :: _ when name = name' ->
            if alpha_equal redex' redex then
               if alpha_equal con' con then
                  ()
               else
                  implem_error (sprintf "InputForm %s: contractum mismatch:\n%s\nshould be\n%s\n" (**)
                                   name (string_of_term con') (string_of_term con))
            else
               implem_error (sprintf "InputForm %s: redex mismatch:\n%s\nshould be\n%s\n" (**)
                                name (string_of_term redex') (string_of_term redex))
       | _ :: t ->
            search t
      in
         search implem;
         items

   (*
    * Conditions in implementation must be weaker than in the interface.
    *)
   let check_cond_rewrite loc
       (info : ('term, 'proof1, 'expr1) cond_rewrite_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
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
                     else if not (List.length args = List.length args' && List.for_all2 alpha_equal args' args) then
                        implem_error (sprintf "Cond_rewrite %s: arg lists mismatch" name)
                     else if not (alpha_equal redex' redex & alpha_equal contractum contractum') then
                        implem_error (sprintf "Cond_rewrite %s: specification mismatch" name)
                     else
                        ()
                  else
                     search t
             | _ ->
                  search t
      in
         search implem;
         items

   (*
    * Rule must be more general.
    *)
   let check_rule loc
       (info : ('term1, 'meta_term1, 'proof1, 'expr1) rule_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let { rule_name = name; rule_params = params; rule_stmt = stmt } = info in
      let rec search = function
         [] ->
            implem_error (sprintf "Rule %s: not implemented" name)
       | Rule { rule_name = name'; rule_params = params'; rule_stmt = stmt' } :: t ->
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
       | _ :: t ->
            search t
      in
         search implem;
         items

   let check_declare_typeclass loc (**)
          (opname : opname)
          (type_opname : opname)
          (parent : typeclass_parent)
          (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
          (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let rec search = function
         [] ->
            (DeclareTypeClass (opname, type_opname, parent), loc) :: items
       | DeclareTypeClass (opname', type_opname', parent') :: _
         when Opname.eq opname' opname ->
            let parent_matches =
               match parent, parent' with
                  ParentExtends opname, ParentExtends opname'
                | ParentInclude opname, ParentInclude opname' ->
                     Opname.eq opname opname'
                | ParentNone, ParentNone ->
                     true
                | _ ->
                     false
            in
               if Opname.eq type_opname type_opname' && parent_matches then
                  items
               else
                  implem_error (sprintf "declare kind '%s' mismatch" (string_of_opname opname))
       | _ :: t ->
            search t
      in
         search implem

   let check_declare_type loc (**)
          (ty_term : ('term1, 'term1) poly_ty_term)
          (ty_opname : opname)
          (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
          (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let term = term_of_ty ty_term in
      let shape = shape_of_term term in
      let rec search = function
         [] ->
            (DeclareType (ty_term, ty_opname), loc) :: items
       | DeclareType (ty_term', ty_opname') :: t ->
            let term' = term_of_ty ty_term' in
            let shape' = shape_of_term term' in
               if ToTerm.TermShape.eq shape' shape then
                  if ToTerm.TermTy.eq_ty ty_term' ty_term && Opname.eq ty_opname' ty_opname then
                     items
                  else
                     implem_error (sprintf "declare type '%s' mismatch" (string_of_opname (opname_of_term term)))
               else
                  search t
       | _ :: t ->
            search t
      in
         search implem

   let check_declare_term loc (**)
          (ty_term : ty_term)
          (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
          (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let term = term_of_ty ty_term in
      let shape = shape_of_term term in
      let rec search = function
         [] ->
            (DeclareTerm ty_term, loc) :: items
       | DeclareTerm ty_term' :: t
       | DefineTerm (ty_term', _) :: t ->
            let term' = term_of_ty ty_term' in
            let shape' = shape_of_term term' in
               if ToTerm.TermShape.eq shape' shape then
                  if ToTerm.TermTy.eq_ty ty_term' ty_term then
                     items
                  else
                     implem_error (sprintf "declare '%s' mismatch" (string_of_shape shape))
               else
                  search t
       | _ :: t ->
            search t
      in
         search implem

   let check_define_term loc (**)
          (ty_term : ty_term)
          (term_def : ('expr1, 'term1) term_def)
          (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
          (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let term = term_of_ty ty_term in
      let shape = shape_of_term term in
      let rec search = function
         [] ->
            implem_error (sprintf "Definition %s: not implemented" (string_of_shape shape))
       | DefineTerm (ty_term', term_def') :: t ->
            let term' = term_of_ty ty_term' in
            let shape' = shape_of_term term' in
               if ToTerm.TermShape.eq shape' shape then begin
                  if not (ToTerm.TermTy.eq_ty ty_term' ty_term) then
                     implem_error (sprintf "define '%s' mismatch" (string_of_shape shape));
                  if not (alpha_equal term_def'.term_def_value term_def.term_def_value) then
                     implem_error (sprintf "define '%s' definition mismatch" (string_of_shape shape))
               end
               else
                  search t
       | _ :: t ->
            search t
      in
         search implem;
         items

   let check_declare_type_rewrite loc (**)
          (redex      : 'term1)
          (contractum : 'term1)
          (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
          (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let redex_opname = opname_of_term redex in
      let rec search = function
         [] ->
            (DeclareTypeRewrite (redex, contractum), loc) :: items
       | DeclareTypeRewrite (redex', contractum') :: _
         when alpha_equal redex redex' ->
            if alpha_equal contractum contractum' then
               items
            else
               implem_error (sprintf "declare type rewrite '%s' mismatch" (string_of_opname redex_opname))
       | _ :: t ->
            search t
      in
         search implem

   (*
    * Token classes.
    *)
   let rec compare_string_lists l1 l2 =
      match l1, l2 with
         [], [] ->
            true
       | ((h1 : string) :: l1), ((h2 : string) :: l2) ->
            h1 = h2 && compare_string_lists l1 l2
       | [], _
       | _, [] ->
            false

   (*
    * MLterms must match.
    *)
   let check_mlrewrite loc (**)
       ({ mlterm_name = name; mlterm_term = term } : ('term1, 'expr1) mlterm_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let rec search = function
         [] ->
            implem_error (sprintf "MLRewrite %s: not implemented" name)
       | MLRewrite { mlterm_name = name'; mlterm_term = term' } :: _ when name' = name ->
            if not (alpha_equal term' term) then
               implem_error (sprintf "MLRewrite %s: definition does not match" name)
       | _ :: t ->
            search t
      in
         search implem;
         items

   let check_mlaxiom loc (**)
       ({ mlterm_name = name; mlterm_term = term } : ('term1, 'expr1) mlterm_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let rec search = function
         [] ->
            implem_error (sprintf "MLAxiom %s: not implemented" name)
       | MLAxiom { mlterm_name = name'; mlterm_term = term' } :: _ when name' = name ->
            if not (alpha_equal term' term) then
               implem_error (sprintf "MLAxiom %s: definition does not match" name)
       | _ :: t ->
            search t
      in
         search implem;
         items

   (*
    * Parent names must match.
    *)
   let check_parent loc (**)
       (path : module_path)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let rec search = function
         [] ->
            implem_error (sprintf "Extends %s: not implemented" (string_of_path path))
       | Parent { parent_name = path' } :: _ when path = path' ->
            ()
       | _ :: t ->
            search t
      in
         search implem;
         items

   (*
    * Display forms.
    *)
   let check_dform loc (**)
       (tags : dform_option list)
       (term : term)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let rec search = function
         [] ->
            implem_error (sprintf "DForm %s: not implemented" (string_of_term term))
       | DForm { dform_options = tags'; dform_redex =  term' } :: _
         when alpha_equal term' term ->
            if tags' = tags then
               ()
            else
               implem_error (sprintf "DForm %s: tag mismatch" (string_of_term term))
       | _ :: t ->
            search t
      in
         search implem;
         items

   (*
    * Precedence declaration.
    *)
   let check_prec loc (name : string)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
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
         search implem;
         items

   (*
    * Resource checking.
    *)
   let rec check_resource loc name implem items =
      match implem with
         [] ->
            implem_error (sprintf "Resource %s: not implemented" name)
       | Resource ( name', _ ) :: _  when name = name' ->
            items
       | _ :: t ->
            check_resource loc name t items

   (*
    * Infix declarations.
    *)
   let check_gupd loc (upd : grammar_update)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
      let rec search = function
         [] ->
            implem_error (match upd with
                             Infix name -> sprintf "Infix %s: not implemented" name
                           | Suffix name -> sprintf "Suffix %s: not implemented" name)
       | MLGramUpd upd' :: _ when upd = upd' ->
            items
       | _ :: t ->
            search t
      in
         search implem

   (*
    * Match the ids.
    *)
   let rec check_id loc id implem items =
      match implem with
         [] ->
            (Id id, loc) :: items
       | Id id'::_ ->
            if id' = id then
               items
            else
               implem_error (sprintf "Implementation is out of date (recompile)")
       | _ :: t ->
            check_id loc id t items

   (*
    * Module definitions.
    *)
   let rec check_module loc (**)
       (name : string)
       (info : ('term1, 'meta_term1, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1) module_info)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list) =
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
         search implem;
         items

   (*
    * Check that an item is implemented.
    *)
   and check_implemented
       (items : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item_loc list)
       (implem : ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) summary_item list)
       ((interf : ('term1, 'meta_term1, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1) summary_item), loc) =
      match interf with
         Rewrite info ->
            check_rewrite loc info implem items
       | InputForm info ->
            check_iform loc info implem items
       | CondRewrite info ->
            check_cond_rewrite loc info implem items
       | Rule info ->
            check_rule loc info implem items
       | DeclareTypeClass (opname, type_opname, parent) ->
            check_declare_typeclass loc opname type_opname parent implem items
       | DeclareType (ty_term, ty_opname) ->
            check_declare_type loc ty_term ty_opname implem items
       | DeclareTerm ty_term ->
            check_declare_term loc ty_term implem items
       | DefineTerm (ty_term, term_def) ->
            check_define_term loc ty_term term_def implem items
       | DeclareTypeRewrite (redex, contractum) ->
            check_declare_type_rewrite loc redex contractum implem items
       | MLRewrite info ->
            check_mlrewrite loc info implem items
       | MLAxiom info ->
            check_mlaxiom loc info implem items
       | Parent { parent_name = path } ->
            check_parent loc path implem items
       | Module (name, info) ->
            check_module loc name info implem items
       | DForm { dform_options = flags; dform_redex = term } ->
            check_dform loc flags term implem items
       | Prec name ->
            check_prec loc name implem items
       | Resource (name, _) ->
            check_resource loc name implem items
       | Improve _ ->
            raise (Invalid_argument "Filter_summary.check_implemented")
       | MLGramUpd upd ->
            check_gupd loc upd implem items
       | Id id ->
            check_id loc id implem items
       | SummaryItem _
       | ToploopItem _
       | PrecRel _
       | MagicBlock _
       | Comment _
       | PRLGrammar _ ->
            items

   (*
    * Check that the implementation satisfies the interface.
    * This means that every item in the interface must be implemented.
    *)
   and check_implementation { info_list = implem } { info_list = interf } =
      let implem = List.map fst implem in
         List.fold_left (fun items item -> check_implemented items implem item) [] interf

   (************************************************************************
    * COMMENT PARSING                                                      *
    ************************************************************************)

   (*
    * Parse each of the comments.
    *)
   let parse_comments parse_comment { info_list = info } =
      let rec parse = function
         item :: items ->
            let items = parse items in
            let item =
               match item with
                  Comment t, loc ->
                     Comment (parse_comment loc t), loc
                | _ ->
                     item
            in
               item :: items
       | [] ->
            []
      in
         { info_list = parse info }

   (************************************************************************
    * PROOF COPYING                                                        *
    ************************************************************************)

   let changed_warning =
      ":\n\twas recently changed, potential mismatch with an existing proof."

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
      Rewrite (
         match find_rewrite info2 rw.rw_name with
            Some (Rewrite { rw_redex = redex;
                            rw_contractum = contractum;
                            rw_proof = proof
                  }, _) ->
               if not (alpha_equal rw.rw_redex redex & alpha_equal rw.rw_contractum contractum) then
                  eprintf "Copy_proof: warning: rewrite %s%s%t" rw.rw_name changed_warning eflush;
               { rw with rw_proof = copy_proof rw.rw_proof proof }
          | _ ->
               rw
      )

   (*
    * Copy the cond_rewrite proof.
    *)
   let copy_crw_proof copy_proof crw info2 =
      CondRewrite (
         match find_rewrite info2 crw.crw_name with
            Some (CondRewrite { crw_redex = redex;
                                crw_contractum = contractum;
                                crw_proof = proof
                  }, _) ->
               if not (alpha_equal crw.crw_redex redex)
                  or not (alpha_equal crw.crw_contractum contractum)
               then
                  eprintf "Copy_proof: warning: cond_rewrite %s%s%t" crw.crw_name changed_warning eflush;
               { crw with crw_proof = copy_proof crw.crw_proof proof }
          | _ ->
               crw
      )

   (*
    * Copy the proof in a rule.
    *)
   let copy_rule_proof copy_proof item info2 =
      Rule (
         match find_axiom info2 item.rule_name with
            Some (Rule { rule_stmt = stmt;
                         rule_proof = proof
                  }, _) ->
               if not (meta_alpha_equal item.rule_stmt stmt) then
                  begin
                     eprintf "Copy_proof: warning: rule %s%s%t" item.rule_name changed_warning eflush;
                     if !debug_match then
                        begin
                           eprintf "Term 1:\n\t";
                           prerr_simple_mterm item.rule_stmt;
                           eflush stderr;
                           eprintf "Term 2:\n\t";
                           prerr_simple_mterm stmt;
                           eflush stderr
                        end
                  end;
               { item with rule_proof = copy_proof item.rule_proof proof }
          | _ ->
               item
      )

   (*
    * Copy the proofs from the second implementation into the first.
    *)
   let rec copy_proofs copy_proof { info_list = info1 } info2 =
      let copy (item, loc) =
         let item =
            match item with
               Rewrite rw ->
                  copy_rw_proof copy_proof rw info2
             | CondRewrite crw ->
                  copy_crw_proof copy_proof crw info2
             | Rule item ->
                  copy_rule_proof copy_proof item info2
             | Module (name, info) ->
                  copy_module_proof copy_proof name info info2
             | DeclareTypeClass _
             | DeclareType _
             | DeclareTerm _
             | DefineTerm _
             | DeclareTypeRewrite _
             | MLRewrite _
             | MLAxiom _
             | Parent _
             | DForm _
             | Prec _
             | PrecRel _
             | Id _
             | Comment _
             | Resource _
             | Improve _
             | MLGramUpd _
             | SummaryItem _
             | ToploopItem _
             | MagicBlock _
             | InputForm _
             | PRLGrammar _ ->
                  item
         in
            item, loc
      in
         { info_list = List.map copy info1 }

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
