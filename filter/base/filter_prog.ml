(*
 * Conversion from module_info to program text.
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

open Printf

open Mp_debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Precedence
open Simple_print
open Mp_resource

open Free_vars
open Filter_type
open Filter_util
open Filter_ast
open Filter_cache
open Filter_summary_type
open Filter_summary_util
open Filter_summary

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_prog%t"

let debug_filter_prog =
   create_debug (**)
      { debug_name = "filter_prog";
        debug_description = "display operations that convert ML to terms";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Signature for extract module.
 *)
module type ExtractSig =
sig
   type proof
   type arg

   val extract_sig :
      arg ->
      (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (module_path * string * MLast.ctyp resource_sig) list ->
      string -> (MLast.sig_item * (int * int)) list

   val extract_str :
      arg ->
      (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (term, meta_term, proof proof_type, MLast.expr, MLast.ctyp, MLast.expr, MLast.str_item) module_info ->
      (module_path * string * MLast.ctyp resource_sig) list ->
      string -> (MLast.str_item * (int * int)) list
end

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

(*
 * Axiom.
 *)
let refiner_expr loc =
   <:expr< Refiner.Refiner.Refine >>

let refiner_patt loc =
   <:patt< Refiner.Refiner.Refine >>

let refiner_ctyp loc =
   <:ctyp< Refiner.Refiner.Refine >>

let rewriter_expr loc =
   <:expr< Refiner.Refiner.Rewrite >>

let rewriter_patt loc =
   <:patt< Refiner.Refiner.Rewrite >>

let tactic_type_expr loc =
   <:expr< Tactic_type.Tactic >>

let tactic_type_ctyp loc =
   <:ctyp< Tactic_type.Tactic >>

let rewrite_type_expr loc =
   <:expr< Tactic_type.Rewrite >>

let rewrite_type_ctyp loc =
   <:ctyp< Tactic_type.Rewrite >>

let dest_msequent_expr loc =
   <:expr< $refiner_expr loc$ . dest_msequent >>

(*
 * Rule.
 *)
let create_rule_expr loc =
   <:expr< $refiner_expr loc$ . create_rule >>

let prim_rule_expr loc =
   <:expr< $refiner_expr loc$ . prim_rule >>

let derived_rule_expr loc =
   <:expr< $refiner_expr loc$ . derived_rule >>

let delayed_rule_expr loc =
   <:expr< $refiner_expr loc$ . delayed_rule >>

let create_ml_rule_expr loc =
   <:expr< $refiner_expr loc$ . create_ml_rule >>

let compile_rule_expr loc =
   <:expr< $tactic_type_expr loc$ . compile_rule >>

let compile_labeled_rule_expr loc =
   <:expr< $tactic_type_expr loc$ . compile_labeled_rule >>

let tactic_of_rule_expr loc =
   <:expr< $tactic_type_expr loc$ . tactic_of_rule >>

let tactic_ctyp loc =
   <:ctyp< $tactic_type_ctyp loc$ . tactic >>

(*
 * Rewrite.
 *)
let rewrite_ctyp loc =
   <:ctyp< $rewrite_type_ctyp loc$ . conv >>

let create_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . create_rewrite >>

let create_input_form_expr loc =
   <:expr< $refiner_expr loc$ . create_input_form >>

let prim_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . prim_rewrite >>

let derived_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . derived_rewrite >>

let delayed_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . delayed_rewrite >>

let create_ml_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . create_ml_rewrite >>

let create_ml_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . create_ml_cond_rewrite >>

let rewrite_of_rewrite_expr loc =
   <:expr< $rewrite_type_expr loc$ . rewrite_of_rewrite >>

(*
 * Conditional rewrite.
 *)
let cond_rewrite_ctyp loc =
   <:ctyp< $rewrite_type_ctyp loc$ . conv >>

(*
   let sarray = <:ctyp< array string >> in
   let term = <:ctyp< list (Refiner.Refiner.Term.term) >> in
   let arg = <:ctyp< ($sarray$ * $term$) >> in
      <:ctyp< $arg$ -> $rewrite_ctyp loc$ >>
 *)

let create_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . create_cond_rewrite >>

let prim_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . prim_cond_rewrite >>

let derived_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . derived_cond_rewrite >>

let delayed_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . delayed_cond_rewrite >>

let rewrite_of_cond_rewrite_expr loc =
   <:expr< $rewrite_type_expr loc$ . rewrite_of_cond_rewrite >>

let apply_redex_expr loc =
   <:expr< $rewriter_expr loc$ . apply_redex >>

let construct_redex_expr loc =
   <:expr< Refiner.Refiner.TermMan.construct_redex >>

let compile_redex_expr loc =
   <:expr< $rewriter_expr loc$ . compile_redex >>

let compile_redices_expr loc =
   <:expr< $rewriter_expr loc$ . compile_redices >>

let compile_contractum_expr loc =
   <:expr< $rewriter_expr loc$ . compile_contractum >>

let make_contractum_expr loc =
   <:expr< $rewriter_expr loc$ . make_contractum >>

let strict_expr loc =
   <:expr< $rewriter_expr loc$ . Strict >>

let relaxed_expr loc =
   <:expr< $rewriter_expr loc$ . Relaxed >>

(*
 * Other expressions.
 *)
let label_refiner_expr loc =
   <:expr< $refiner_expr loc$ . label_refiner >>

let refiner_ctyp loc =
   <:ctyp< $refiner_ctyp loc$ . refiner >>

let join_refiner_expr loc =
   <:expr< $refiner_expr loc$ . join_refiner >>

let join_mode_base_expr loc =
   <:expr< Dform_print.join_mode_base >>

let dformer_ctyp loc =
   <:ctyp< Dform_print.dform_mode_base >>

let get_resource_name name =
   "get_" ^ name ^ "_resource"

let input_type name =
   "_$" ^ name ^ "_resource_input"

let dform_name_patt loc =
   <:patt< Dform.dform_name >>

let dform_pattern_patt loc =
   <:patt< Dform.dform_pattern >>

let dform_options_patt loc =
   <:patt< Dform.dform_options >>

let dform_print_patt loc =
   <:patt< Dform.dform_print >>

let dform_term_patt loc =
   <:patt< Dform.dform_term >>

let dform_printer_patt loc =
   <:patt< Dform.dform_printer >>

let dform_items_patt loc =
   <:patt< Dform.dform_items >>

let dform_buffer_patt loc =
   <:patt< Dform.dform_buffer >>

let dform_expansion_expr loc =
   <:expr< Dform.DFormExpansion >>

let dform_printer_expr loc =
   <:expr< Dform.DFormPrinter >>

let dform_parens_expr loc =
   <:expr< Dform.DFormParens >>

let dform_internal_expr loc =
   <:expr< Dform.DFormInternal >>

let dform_prec_expr loc s =
   <:expr< Dform.DFormPrec $lid:s$ >>

let dform_inherit_prec_expr loc =
   <:expr< Dform.DFormInheritPrec >>

let dform_inherit_prec_expr loc =
   <:expr< Dform.DFormInheritPrec >>

let create_dform_modes_expr loc =
   <:expr< Dform_print.create_dform_modes >>

let create_dform_except_modes_expr loc =
   <:expr< Dform_print.create_dform_except_modes >>

let create_dform_all_expr loc =
   <:expr< Dform_print.create_dform_all >>

let refiner_id = "refiner"
let dformer_id = "dformer"

let local_refiner_id = "_$global_refiner"
let local_dformer_id = "_$global_dformer"
let stack_id = "_$rewrite_stack"

let null_refiner_expr loc name =
   <:expr< $refiner_expr loc$ . null_refiner $str: name$ >>

let null_mode_base_expr loc =
   <:expr< Dform_print.null_mode_base >>

let nil_array loc =
   <:expr< [| $list:[]$ |] >>

let nil_list loc =
   <:expr< [] >>

let precedence_ctyp loc =
   <:ctyp< Precedence.precedence >>

let new_prec_expr loc =
   <:expr< Precedence.new_prec >>

let add_lt_expr loc =
   <:expr< Precedence.add_lt >>

let add_eq_expr loc =
   <:expr< Precedence.add_eq >>

(*
 * Each rule gets a refiner associated with it, with the following name.
 *)
let refiner_value loc =
   <:expr< Refiner.Refiner.Refine.refiner_of_build $lid: local_refiner_id$ >>

let refiner_let loc =
   let patt = <:patt< $lid: refiner_id$ >> in
      <:str_item< value $rec:false$ $list: [ patt, refiner_value loc ]$ >>

(*
 * Variable names.
 *)
let exn_id              = "_$exn"
let term_id             = "_$term"
let bvars_id            = "_$bvars"
let args_id             = "_$args"
let redex_id            = "_$redex"
let contractum_id       = "_$contractum"
let params_id           = "_$params"
let subgoals_id         = "_$subgoals"
let extract_id          = "_$extract"
let namer_id            = "_$namer"
let term_id             = "_$term"
let x_id                = "_$x"
let cvars_id            = "_$cvars"
let bvars_id            = "_$bvars"
let tvars_id            = "_$tvars"
let avars_id            = "_$avars"
let bnames_id           = "_$bnames"
let info_id             = "_$info"
let rewrite_id          = "_$rewrite"
let extract_id          = "_$extract"
let stack_id            = "_$stack"
let names_id            = "_$names"
let goal_id             = "_$goal"
let seq_id              = "_$seq"
let assums_id           = "_$assums"
let rule_id             = "_$rule"
let addrs_id            = "_$addrs"
let msequent_goal_id    = "_$mseq_goal"
let msequent_hyps_id    = "_$mseq_hyps"
let printer_id          = "_$printer"
let dprinter_id         = "_$dprinter"
let labels_id           = "_$labels"
let rule_name_id        = "_$rule_name"

(*
 * Convert between expressions and terms.
 *)
let expr_of_term loc t =
   let s = Ml_term.string_of_term t in
   <:expr< Ml_term.term_of_string $str: String.escaped s$ >>

let expr_of_mterm loc t =
   let s = Ml_term.string_of_mterm t in
   <:expr< Ml_term.mterm_of_string $str: String.escaped s$ >>

let expr_of_label loc = function
   [] ->
      <:expr< None >>
 | h :: _ ->
      <:expr< Some $str: h$ >>

let expr_of_contractum loc index =
   <:expr< $make_contractum_expr loc$ $lid:sprintf "%s%d" contractum_id index$ $lid:stack_id$ >>

(*
 * Curried function.
 *)
let rec curry loc vars expr =
   match vars with
      v::t ->
         let v = <:patt< $lid:v$ >> in
            <:expr< fun [ $list: [ v, None, curry loc t expr ]$ ] >>
    | [] ->
       expr

(*
 * Print a message on loading, and catch errors.
 *    Mp_debug.show_loading "Loading name%t";
 *    try e with
 *       exn ->
 *          Refine_exn.print_exn name exn
 *)
let wrap_exn loc name e =
   let unit_patt = <:patt< () >> in
   let unit_expr = <:expr< () >> in

   (* Wrap the body to catch exceptions *)
   let exn_patt = <:patt< $lid: exn_id$ >> in
   let exn_expr = <:expr< $lid: exn_id$ >> in
   let stderr = <:expr< Pervasives.stderr >> in
   let dform = <:expr< $lid: local_dformer_id$ . val >> in
   let printer = <:expr< Refine_exn.print_exn $dform$ $stderr$ $str: name$ $exn_expr$ >> in
   let wrapped = <:expr< try $e$ with [ $list: [exn_patt, None, printer]$ ] >> in

   (* Print a message before the execution *)
   let show_loading = <:expr< Mp_debug.show_loading >> in
   let msg = <:expr< $str: "Loading " ^ name ^ "%t"$ >> in
   let loading_msg = <:expr< $show_loading$ $msg$ >> in
      <:expr< do { $list: [ loading_msg; wrapped ] $ } >>

(*
 * Param expression.
 *)
let param_expr loc = function
   ContextParam s ->
      <:expr< Filter_summary.ContextParam $str:s$ >>
 | VarParam v ->
      <:expr< Filter_summary.VarParam $str:v$ >>
 | TermParam t ->
      let t' = expr_of_term loc t in
         <:expr< Filter_summary.TermParam $t'$ >>

(*
 * Create function type.
 *)
let params_ctyp loc ctyp params =
   let rec convert = function
      [] ->
         ctyp
    | h::t ->
         let ctyp' = convert t in
         let arg_type =
            match h with
               ContextParam _ ->
                  <:ctyp< Refiner.Refiner.TermAddr.address >>
             | VarParam _ ->
                  <:ctyp< string >>
             | TermParam _ ->
                  <:ctyp< Refiner.Refiner.Term.term >>
         in
            <:ctyp< $arg_type$ -> $ctyp'$ >>
   in
      convert params

(*
 * Convert display form options to expressions.
 *)
let dform_option_expr loc = function
   DFormParens ->
      dform_parens_expr loc
 | DFormPrec p ->
      dform_prec_expr loc p
 | DFormInheritPrec ->
      dform_inherit_prec_expr loc
 | DFormInternal ->
      dform_internal_expr loc

(*
 * Convert a module path to an expression.
 *)
let rec parent_path_expr loc = function
   [h] ->
      <:expr< $uid:String.capitalize h$ >>
 | h::t ->
      <:expr< $uid:String.capitalize h$ . $parent_path_expr loc t$ >>
 | [] ->
      raise (Invalid_argument "parent_path")

let rec parent_path_ctyp loc = function
   [h] ->
      <:ctyp< $uid:String.capitalize h$ >>
 | h::t ->
      <:ctyp< $uid:String.capitalize h$ . $parent_path_ctyp loc t$ >>
 | [] ->
      raise (Invalid_argument "parent_path")

(*
 * Interactive exception.
 *)
let interactive_exn loc name =
   let patt = <:patt< _ >> in
(*
   let ename = <:expr< Refiner.Refiner.RefineError.RefineError >> in
   let err = <:expr< Refiner.Refiner.RefineError.StringError "proof is incomplete" >> in
   let body = <:expr< raise ($ename$ ($str: name$, $err$)) >> in
 *)
   let body = <:expr< raise (Failure "interactive proof") >> in
      <:expr< fun [ $list: [ patt, None, body ]$ ] >>

let raise_toploop_exn loc =
   Stdpp.raise_with_loc loc (RefineError ("topval", StringError
                                          "The types allowed in toploop expressions are limited.\n\
Your type is not understood. See the module Mptop for allowed types."))

(*
 * This function checks that the type is acceptable for the toploop
 * and creates a toploop expression
 *)
let toploop_item_expr loc name ctyp =
   let rec collect index expr = function
      <:ctyp< unit >> ->
         mptop "UnitExpr" expr
    | <:ctyp< bool >> ->
         mptop "BoolExpr" expr
    | <:ctyp< int >> ->
         mptop "IntExpr" expr
    | <:ctyp< string >> ->
         mptop "StringExpr" expr
    | <:ctyp< term >> ->
         mptop "TermExpr" expr
    | <:ctyp< tactic >> ->
         mptop "TacticExpr" expr
    | <:ctyp< conv >> ->
         mptop "ConvExpr" expr
    | <:ctyp< $t1$ -> $t2$ >> ->
         collect_fun index expr t1 t2
    | _ ->
         raise_toploop_exn loc
   and collect_fun index expr t1 t2 =
      match t1 with
         <:ctyp< unit >> ->
            mpfun index "UnitFunExpr" expr t2
       | <:ctyp< bool >> ->
            mpfun index "BoolFunExpr" expr t2
       | <:ctyp< int >> ->
            mpfun index "IntFunExpr" expr t2
       | <:ctyp< string >> ->
            mpfun index "StringFunExpr" expr t2
       | <:ctyp< term >> ->
            mpfun index "TermFunExpr" expr t2
       | <:ctyp< tactic >> ->
            mpfun index "TacticFunExpr" expr t2
       | <:ctyp< conv >> ->
            mpfun index "ConvFunExpr" expr t2
       | <:ctyp< address >>
       | <:ctyp< list $lid: "int"$ >> ->
            mpfun index "AddrFunExpr" expr t2
       | <:ctyp< list $lid: "string"$ >> ->
            mpfun index "StringListFunExpr" expr t2
       | <:ctyp< list $lid: "term"$ >> ->
            mpfun index "TermListFunExpr" expr t2
       | <:ctyp< list $lid: "tactic"$ >> ->
            mpfun index "TacticListFunExpr" expr t2
       | <:ctyp< list $lid: "conv"$ >> ->
            mpfun index "ConvListFunExpr" expr t2
       | <:ctyp< int -> $lid: "tactic"$ >> ->
            mpfun index "IntTacticFunExpr" expr t2
       | _ ->
            raise_toploop_exn loc
   and mptop name expr =
      <:expr< Mptop. $uid: name$ $expr$ >>
   and mpfun index name expr t2 =
      let v = sprintf "v%d" index in
      let patt = <:patt< $lid: v$ >> in
      let expr = collect (succ index) <:expr< $expr$ $lid: v$ >> t2 in
         <:expr< Mptop. $uid: name$ (fun [ $list: [ patt, None, expr ]$ ]) >>
   in
      collect 0 <:expr< $lid: name$ >> ctyp

(************************************************************************
 * SIGNATURES                                                           *
 ************************************************************************)

(*
 * Rewrites.
 *)
let declare_rewrite loc { rw_name = name } =
   let ctyp = rewrite_ctyp loc in
      [<:sig_item< value $name$ : $ctyp$ >>]
      (* <:sig_item< value $refiner_name name$ : $refiner_ctyp loc$ >> *)

let declare_input_form loc { rw_name = name } =
   let ctyp = rewrite_ctyp loc in
      [<:sig_item< value $name$ : $ctyp$ >>]
      (* <:sig_item< value $refiner_name name$ : $refiner_ctyp loc$ >> *)

let declare_cond_rewrite loc { crw_name = name; crw_params = params } =
   let ctyp = params_ctyp loc (cond_rewrite_ctyp loc) params in
      [<:sig_item< value $name$ : $ctyp$ >>]
      (* <:sig_item< value $refiner_name name$ : $refiner_ctyp loc$ >>] *)

let declare_ml_rewrite loc { mlterm_name = name; mlterm_params = params } =
   let ctyp = params_ctyp loc (cond_rewrite_ctyp loc) params in
      [<:sig_item< value $name$ : $ctyp$ >>]
      (* <:sig_item< value $refiner_name name$ : $refiner_ctyp loc$ >>] *)

(*
 * Rules.
 *)
let declare_rule loc { rule_name = name; rule_params = params } =
   let ctyp = params_ctyp loc (tactic_ctyp loc) params in
      [<:sig_item< value $name$ : $ctyp$ >>]
      (* <:sig_item< value $refiner_name name$ : $refiner_ctyp loc$ >>] *)

let declare_ml_axiom loc { mlterm_name = name; mlterm_params = params } =
   let ctyp = params_ctyp loc (tactic_ctyp loc) params in
      [<:sig_item< value $name$ : $ctyp$ >>]
      (* <:sig_item< value $refiner_name name$ : $refiner_ctyp loc$ >>] *)

(*
 * Precedence.
 *)
let declare_prec loc name =
   [<:sig_item< value $name$ : $precedence_ctyp loc$ >>]

(*
 * Resource.
 *)
let declare_resource loc name {
   resource_input = input;
   resource_output = output
} =
   let get_resource_type = <:ctyp< Mp_resource.global_resource -> $output$ >> in
      [<:sig_item< type $input_type name$ = $input$ >>;
       <:sig_item< value $get_resource_name name$ : $get_resource_type$ >>]

(*
 * When a parent is declared, we need to open all the ancestors.
 *)
let declare_parent loc _ =
   []

(*
 * Standard summary item.
 *)
let declare_summary_item loc item =
   [item]

let declare_toploop_item loc item =
   begin match item with
         <:sig_item< value $s$ : $t$ >> ->
            (* Check that the type is understood *)
            ignore(toploop_item_expr (MLast.loc_of_ctyp t) s t)
       | _ ->
            Stdpp.raise_with_loc loc (RefineError ("declare_toploop_item", StringError "illegal topval"))
   end;
   declare_summary_item loc item

(*
 * Magic block is a block of items.
 *)
let declare_magic_block loc { magic_code = items } =
   items

(*
 * Trailer declares a new refiner.
 *)
let interf_postlog info loc =
   let refiner_decl = (<:sig_item< value $refiner_id$ : $refiner_ctyp loc$ >>) in
   let dformer_decl = (<:sig_item< value $dformer_id$ : $dformer_ctyp loc$ >>) in
      [refiner_decl; dformer_decl]

(*
 * Extract a signature item.
 *)
let extract_sig_item (item, loc) =
   match item with
      Rewrite ({ rw_name = name } as rw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: rewrite: %s%t" name eflush;
         declare_rewrite loc rw
    | InputForm ({ rw_name = name } as rw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: input form: %s%t" name eflush;
         declare_input_form loc rw
    | CondRewrite ({ crw_name = name } as crw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: cond rewrite: %s%t" name eflush;
         declare_cond_rewrite loc crw
    | Rule ({ rule_name = name } as rule) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: rule: %s%t" name eflush;
         declare_rule loc rule
    | Prec name ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: prec: %s%t" name eflush;
         declare_prec loc name
    | Resource (name, rsrc) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: resource: %s%t" name eflush;
         declare_resource loc name rsrc
    | Improve _ ->
         raise(Invalid_argument "Filter_prog.extract_sig_item")
    | Parent ({ parent_name = name } as parent) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: parent: %s%t" (string_of_path name) eflush;
         declare_parent loc parent
    | SummaryItem item ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: summary_item%t" eflush;
         declare_summary_item loc item
    | ToploopItem item ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: toploop_item%t" eflush;
         declare_toploop_item loc item
    | MagicBlock block ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: magic block%t" eflush;
         declare_magic_block loc block
    | Opname _ ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: opname%t" eflush;
         []
    | MLRewrite item ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: mlrewrite%t" eflush;
         declare_ml_rewrite loc item
    | MLAxiom item ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: mlaxiom%t" eflush;
         declare_ml_axiom loc item
    | DForm _ ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: dform%t" eflush;
         []
    | PrecRel _ ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: prec rel%t" eflush;
         []
    | Id id ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: id: 0x%08x%t" id eflush;
         []
    | Comment e ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: comment%t" eflush;
         []
    | Infix name ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: infix: %s%t" name eflush;
         []
    | Module (name, _) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: module: %s%t" name eflush;
         raise (Failure "Filter_sig.extract_sig_item: nested modules are not implemented")

(*
 * Extract a signature.
 *)
let extract_sig _ info resources path =
   let _ =
      if !debug_filter_prog then
         eprintf "Filter_prog.extract_sig: begin%t" eflush
   in
   let items = List_util.flat_map extract_sig_item (info_items info) in
   let postlog = interf_postlog resources (0, 0) in
      List.map (fun item -> item, (0, 0)) (items @ postlog)

(************************************************************************
 * IMPLEMENTATIONS                                                      *
 ************************************************************************)

(*
 * For implementations, we maintain a state, which contains
 *    1. a list of the resources that have been defined
 *
 * The implementation is wrapped in a functor.
 *)
module MakeExtract (Convert : ConvertProofSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Get the sig extractor.
    *)
   let extract_sig = extract_sig

   (*
    * Implementation state.
    *)
   type t = {
      imp_sig_info : (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info;
      imp_toploop : (string * (MLast.ctyp * loc)) list;
      imp_arg : Convert.t;
      imp_name : string;
      mutable imp_resources : (string * MLast.ctyp) list;
      imp_all_resources : (module_path * string * MLast.ctyp resource_sig) list
   }

   (*
    * Proof is from convertor.
    *)
   type proof = Convert.cooked
   type arg = Convert.t

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   (*
    * Opname with _ separators.
    *)
   let string_of_opname opname =
      let rec cat = function
         [h] ->
            h
       | h :: t ->
            h ^ "_" ^ cat t
       | [] ->
            ""
      in
         cat (Opname.dest_opname opname)

    (*
     * Eta-reduction
     *)
    let beta_reduce_var var f =
        let loc = MLast.loc_of_expr f in
        match f with
          <:expr< fun [ $list:pwel$ ] >> ->
             begin
                match pwel with
                   [<:patt< $lid:v$ >>, None, e] ->
                      v, e
                 | _ ->
                   var, <:expr< $f$ $lid:var$ >>
             end
        | f ->
           var, <:expr< $f$ $lid:var$ >>

    let checkpoint_resources want_checkpoint loc rule_name rest =
      if want_checkpoint then
         <:str_item< (Mp_resource.bookmark $str:rule_name$) >> :: rest
      else rest

   let res_type proc loc name =
      try
         List.assoc name proc.imp_resources
      with
         Not_found ->
            Stdpp.raise_with_loc loc (Failure ("Attempted to use undeclared resource " ^ name))

   (* Mp_resource.improve name (Obj.repr (data : name.input_type)) *)
   let impr_resource proc loc name expr =
      let input = res_type proc loc name in
      let expr = <:expr< Obj.repr ( $expr$ : $input$ ) >> in
         <:expr< Mp_resource.improve $str:name$ $expr$ >>

   (* Mp_resource.improve_list name (Obj.obj (Obj.repr (data : (name.input_type list)))) *)
   let impr_resource_list proc loc name expr =
      let input = res_type proc loc name in
      let expr = <:expr< Obj.obj (Obj.repr ( $expr$ : (list $input$) )) >> in
         <:expr< Mp_resource.improve_list $str:name$ $expr$ >>

   (************************************************************************
    * TOP LOOP                                                             *
    ************************************************************************)

   let impr_toploop proc loc name expr =
      let expr = <:expr< ($str:proc.imp_name$, $str: name$, $expr$) >> in
         <:str_item< ($impr_resource proc loc "toploop" expr$) >>

   (*
    * This is a little bogus, but we add rewrites automatically to the
    * toploop resource.
    *)
   let rec loop_params loc i body base_expr = function
      h :: t ->
         let v = "v" ^ string_of_int i in
         let body = <:expr< $body$ $lid: v$ >> in
         let v = <:patt< $lid:v$ >> in
         let expr = <:expr< fun [ $list: [v, None, loop_params loc (succ i) body base_expr t]$ ] >> in
         let expr =
            match h with
               ContextParam _ ->
                  <:expr< Mptop.AddressFunExpr $expr$ >>
             | VarParam _ ->
                  <:expr< Mptop.StringFunExpr $expr$ >>
             | TermParam _ ->
                  <:expr< Mptop.TermFunExpr $expr$ >>
         in
            expr
    | [] ->
         base_expr body

   let toploop_rewrite proc loc name params =
      let base body = <:expr< Mptop.ConvExpr $body$ >> in
         impr_toploop proc loc name (loop_params loc 0 <:expr< $lid: name$ >> base params)

   let toploop_rule proc loc name params =
      let base body = <:expr< Mptop.TacticExpr $body$ >> in
         impr_toploop proc loc name (loop_params loc 0 <:expr< $lid: name$ >> base params)

   (*
    * These are the argument types that can be used as annotations.
    *)
   type wrap_arg =
      BoolArg
    | IntArg
    | StringArg
    | TermArg
    | TermListArg

   (*
    * Build the wrap code.
    *)
   let wrap_tactic_expr loc =
      <:expr< Tactic_type.Tacticals.wrapT >>

   let wrap_optimized loc name arglist_name vars expr =
      let name = <:expr< $str:name$ >> in
         if vars = [] then
            <:expr< $wrap_tactic_expr loc$ (Tactic_type.TacticType . $uid:arglist_name$ $name$ ) $expr$ >>
         else
            <:expr< $wrap_tactic_expr loc$ (Tactic_type.TacticType . $uid:arglist_name$ ( $list:name :: vars$ )) $expr$ >>

   let wrap_arg loc arg v =
      let s =
         match arg with
            BoolArg ->
               "BoolArg"
          | IntArg ->
               "IntArg"
          | StringArg ->
               "StringArg"
          | TermArg ->
               "TermArg"
          | TermListArg ->
               "TermListArg"
      in
         <:expr< Tactic_type.TacticType. $uid:s$ $v$ >>

   let wrap_general loc name wrap vars expr =
         <:expr< $wrap_tactic_expr loc$ (Tactic_type.TacticType.GeneralArgList
                                        [| $list:List.map2 (wrap_arg loc) wrap vars$ |])
                 $expr$ >>

   let wrap_expr loc name wrap expr =
      let len = List.length wrap in
      let names =
         let rec collect i =
            if i = len then
               []
            else
               sprintf "v%d" i :: collect (succ i)
         in
            collect 0
      in
      let expr =
         let rec collect expr = function
            v :: tl ->
               collect (<:expr< $expr$ $lid:v$ >>) tl
          | [] ->
               expr
         in
            collect expr names
      in
      let vars = List.map (fun v -> <:expr< $lid:v$ >>) names in
      let expr =
         match wrap with
            [] ->
               wrap_optimized loc name "NoneArgList" vars expr
          | [IntArg] ->
               wrap_optimized loc name "IntArgList" vars expr
          | [BoolArg] ->
               wrap_optimized loc name "BoolArgList" vars expr
          | [StringArg] ->
               wrap_optimized loc name "StringArgList" vars expr
          | [TermArg] ->
               wrap_optimized loc name "TermArgList" vars expr
          | [IntArg; IntArg] ->
               wrap_optimized loc name "IntIntArgList" vars expr
          | [IntArg; BoolArg] ->
               wrap_optimized loc name "IntBoolArgList" vars expr
          | [IntArg; StringArg] ->
               wrap_optimized loc name "IntStringArgList" vars expr
          | [IntArg; TermArg] ->
               wrap_optimized loc name "IntTermArgList" vars expr
          | [BoolArg; IntArg] ->
               wrap_optimized loc name "BoolIntArgList" vars expr
          | [BoolArg; BoolArg] ->
               wrap_optimized loc name "BoolBoolArgList" vars expr
          | [BoolArg; StringArg] ->
               wrap_optimized loc name "BoolStringArgList" vars expr
          | [BoolArg; TermArg] ->
               wrap_optimized loc name "BoolTermArgList" vars expr
          | [StringArg; IntArg] ->
               wrap_optimized loc name "StringIntArgList" vars expr
          | [StringArg; BoolArg] ->
               wrap_optimized loc name "StringBoolArgList" vars expr
          | [StringArg; StringArg] ->
               wrap_optimized loc name "StringStringArgList" vars expr
          | [StringArg; TermArg] ->
               wrap_optimized loc name "StringTermArgList" vars expr
          | [TermArg; IntArg] ->
               wrap_optimized loc name "TermIntArgList" vars expr
          | [TermArg; BoolArg] ->
               wrap_optimized loc name "TermBoolArgList" vars expr
          | [TermArg; StringArg] ->
               wrap_optimized loc name "TermStringArgList" vars expr
          | [TermArg; TermArg] ->
               wrap_optimized loc name "TermTermArgList" vars expr
          | wrap ->
               wrap_general loc name wrap vars expr
      in
         curry loc names expr

   (*
    * Wrap a toploop expression.
    *)
   let wrap_toploop_item loc name ctyp expr =
      let rec collect wrap = function
         <:ctyp< tactic >> ->
            wrap_expr loc name (List.rev wrap) expr
       | <:ctyp< $t1$ -> $t2$ >> ->
            collect_fun wrap t1 t2
       | _ ->
            expr
      and collect_fun wrap t1 t2 =
         match t1 with
            <:ctyp< bool >> ->
               collect (BoolArg :: wrap) t2
          | <:ctyp< int >> ->
               collect (IntArg :: wrap) t2
          | <:ctyp< string >> ->
               collect (StringArg :: wrap) t2
          | <:ctyp< term >> ->
               collect (TermArg :: wrap) t2
          | <:ctyp< list $lid: "term"$ >> ->
               collect (TermListArg :: wrap) t2
          | _ ->
               expr
      in
         collect [] ctyp

   (*
    * This function creates str_items for the toploop.
    *)
   let add_toploop_item proc loc name ctyp =
      let expr = toploop_item_expr loc name ctyp in
         [impr_toploop proc loc name expr]

   (************************************************************************
    * ML RULE                                                              *
    ************************************************************************)

   (*
    * An mlterm is a side condition that is checked in ML.
    * The term expands to the code production.
    *
    * This is the code we create:
    *    let term = $expr_of_term redex$ in
    *    let bvars = [| $bvars$ |] in
    *    let args = [| redex :: List.map expr_of_term args$ |] in
    *    let redex, namer = compile_redices bvars args in
    *    let contractum_0_id = compile_contractum redex (expr_of_term contracta_0) in
    *    ...
    *    let contractum_n_id = compile_contractum redex (expr_of_term contracta_n) in
    *       code
    *)
   let define_ml_program proc loc strict_expr bvars args tname redex contracta code =
      (* Identifier names *)
      let term_patt = <:patt< $lid:term_id$ >> in
      let bvars_patt = <:patt< $lid:bvars_id$ >> in
      let args_patt  = <:patt< $lid:args_id$ >> in
      let redex_namer_patt= <:patt< ( $lid:redex_id$ , $lid:namer_id$ ) >> in
      let redex_expr = expr_of_term loc redex in
      let string_expr s = <:expr< $str:s$ >> in

      (* Build a contractum *)
      let rec contracta_bind index = function
         t::tl ->
            let term_expr = expr_of_term loc t in
            let name = sprintf "_$contractum%d" index in
            let let_patt = <:patt< $lid:name$ >> in
            let let_value =
               <:expr< $compile_contractum_expr loc$ $strict_expr loc$ $lid:redex_id$ $term_expr$ >>
            in
               (let_patt, let_value) :: (contracta_bind (succ index) tl)
       | [] ->
            []
      in

      (* Build the program *)
      let contracta_binding = contracta_bind 0 contracta in
      let contracta_expr =
         if contracta_binding = [] then
            code
         else
            <:expr< let $rec:false$ $list:contracta_binding$ in $code$ >>
      in
      let redex_namer_expr =
         <:expr< $compile_redices_expr loc$ $strict_expr loc$ $lid:bvars_id$ $lid:args_id$ >>
      in
      let redex_namer_let =
         <:expr< let $rec:false$ $list:[redex_namer_patt,
                                        redex_namer_expr]$
                 in $contracta_expr$ >>
      in
      let args_expr =
         <:expr< [ $lid:term_id$ :: $list_expr loc (expr_of_term loc) args$ ] >>
      in
      let args_let =
         <:expr< let $rec:false$ $list:[args_patt, args_expr]$ in $redex_namer_let$ >>
      in
      let bvars_expr =
         <:expr< [| $list: List.map string_expr bvars$ |] >>
      in
      let bvars_let =
         <:expr< let $rec:false$ $list:[bvars_patt, bvars_expr]$ in $args_let$ >>
      in
      let term_let =
         <:expr< let $rec:false$ $list:[term_patt, redex_expr]$ in $bvars_let$ >>
      in
         term_let

   let () = ()

   (************************************************************************
    * REWRITES                                                             *
    ************************************************************************)

   (*
    * An input form is a rewrite, but we don't add it to the
    * refiner (input forms have no formal justification).
    *
    * let name_rewrite =
    *    let redex_id = redex in
    *    let contractum_id = contractum in
    *       create_rewrite refiner name redex contractum
    * let name x = rewrite_of_rewrite name_rewrite x
    *)
   let define_input_form want_checkpoint proc loc
       { rw_name = name;
         rw_redex = redex;
         rw_contractum = contractum
       } =
      (* Names *)
      let rw_id      = "_$" ^ name ^ "_rewrite" in
      let rw_expr    = <:expr< $lid:rw_id$ >> in
      let rw_patt    = <:patt< $lid:rw_id$ >> in
      let x_patt     = <:patt< $lid:x_id$ >> in
      let name_patt  = <:patt< $lid:name$ >> in
      let wild_patt  = <:patt< _ >> in
      let redex_expr = <:expr< $lid:redex_id$ >> in
      let con_expr   = <:expr< $lid:contractum_id$ >> in
      let redex_patt = <:patt< $lid:redex_id$ >> in
      let con_patt   = <:patt< $lid:contractum_id$ >> in

      (* Expressions *)
      let redex_term = expr_of_term loc redex in
      let con_term = expr_of_term loc contractum in
      let create_expr =
         <:expr< $create_input_form_expr loc$ $lid:local_refiner_id$ $str:name$ $lid:redex_id$ $lid:contractum_id$ >>
      in
      let rw_body_expr = <:expr< $rewrite_of_rewrite_expr loc$ $lid:rw_id$ >> in

      (* Let expressions *)
      let body =
         <:expr< let $rec:false$ $list:[ redex_patt, redex_term;
                                         con_patt, con_term ]$
                 in
                 let $rec:false$ $list:[ name_patt, create_expr]$
                 in
                    $lid:name$ >>
       in
       let name_rewrite_let =
          <:str_item< value $rec:false$ $list:[ rw_patt, wrap_exn loc rw_id body ]$ >>
       in
       let name_let = <:str_item< value $rec:false$ $list:[ name_patt, rw_body_expr ]$ >> in
          checkpoint_resources want_checkpoint loc name (**)
             [name_rewrite_let; name_let; toploop_rewrite proc loc name []]

   let ()  = ()

   (*
    * A primitive rewrite is assumed true by fiat.
    *
    * let name_rewrite =
    *    let redex_id = redex in
    *    let contractum_id = contractum in
    *    let rw = create_rewrite refiner name redex contractum in
    *    let _ = prim_rewrite refiner_id name redex contractum in
    *       rw
    * let name x = rewrite_of_rewrite name_rewrite x
    *)
   let define_rewrite want_checkpoint code proc loc
       { rw_name = name;
         rw_redex = redex;
         rw_contractum = contractum
       } expr =
      (* Names *)
      let rw_id      = "_$" ^ name ^ "_rewrite" in
      let rw_expr    = <:expr< $lid:rw_id$ >> in
      let rw_patt    = <:patt< $lid:rw_id$ >> in
      let x_patt     = <:patt< $lid:x_id$ >> in
      let name_patt  = <:patt< $lid:name$ >> in
      let wild_patt  = <:patt< _ >> in
      let redex_expr = <:expr< $lid:redex_id$ >> in
      let con_expr   = <:expr< $lid:contractum_id$ >> in
      let redex_patt = <:patt< $lid:redex_id$ >> in
      let con_patt   = <:patt< $lid:contractum_id$ >> in

      (* Expressions *)
      let redex_term = expr_of_term loc redex in
      let con_term = expr_of_term loc contractum in
      let create_expr =
         <:expr< $create_rewrite_expr loc$ $lid:local_refiner_id$ $str:name$ $lid:redex_id$ $lid:contractum_id$ >>
      in
      let prim_expr =
         match expr with
            Some expr ->
               <:expr< $code$ $lid:local_refiner_id$ $str:name$ $lid:redex_id$ $lid:contractum_id$ $expr$ >>
          | None ->
               <:expr< $code$ $lid:local_refiner_id$ $str:name$ $lid:redex_id$ $lid:contractum_id$ >>
      in
      let rw_body_expr = <:expr< $rewrite_of_rewrite_expr loc$ $lid:rw_id$ >> in

      (* Let expressions *)
      let body =
         <:expr< let $rec:false$ $list:[ redex_patt, redex_term;
                                         con_patt, con_term ]$
                 in
                 let $rec:false$ $list:[ name_patt, create_expr;
                                         wild_patt, prim_expr ]$
                 in
                    $lid:name$ >>
       in
       let name_rewrite_let =
          <:str_item< value $rec:false$ $list:[ rw_patt, wrap_exn loc rw_id body ]$ >>
       in
       let name_let = <:str_item< value $rec:false$ $list:[ name_patt, rw_body_expr ]$ >> in
          checkpoint_resources want_checkpoint loc name (**)
             [name_rewrite_let; name_let; refiner_let loc; toploop_rewrite proc loc name []]

   let ()  = ()

   (*
    * Conditional rewrite is a little more complicated.
    * let name_rewrite =
    *    let vars_id = [| vars |] in
    *    let params_id = [ params ] in
    *    let subgoals_id = [ subgoals ] in
    *    let redex_id = redex in
    *    let contractum_id = contractum in
    *    let rw = create_cond_rewrite refiner name vars_id params_id subgoals_id redex contractum in
    *    let _ = prim_cond_rewrite refiner_id name vars_id params_id subgoals_id redex contractum in
    *       rw
    * let name x = rewrite_of_cond_rewrite name_rewrite x
    *)
   let define_cond_rewrite want_checkpoint code proc loc
       { crw_name       = name;
         crw_params     = params;
         crw_args       = args;
         crw_redex      = redex;
         crw_contractum = contractum
       } expr =
      (* Names *)
      let rw_id         = "_$" ^ name ^ "_rewrite" in
      let rw_expr       = <:expr< $lid:rw_id$ >> in
      let rw_patt       = <:patt< $lid:rw_id$ >> in
      let name_patt     = <:patt< $lid:name$ >> in
      let wild_patt     = <:patt< _ >> in
      let params_expr   = <:expr< $lid:params_id$ >> in
      let subgoals_expr = <:expr< $lid:subgoals_id$ >> in
      let redex_expr    = <:expr< $lid:redex_id$ >> in
      let con_expr      = <:expr< $lid:contractum_id$ >> in
      let cvars_patt    = <:patt< $lid:cvars_id$ >> in
      let bvars_patt    = <:patt< $lid:bvars_id$ >> in
      let params_patt   = <:patt< $lid:params_id$ >> in
      let subgoals_patt = <:patt< $lid:subgoals_id$ >> in
      let redex_patt    = <:patt< $lid:redex_id$ >> in
      let con_patt      = <:patt< $lid:contractum_id$ >> in

      let string_expr s = <:expr< $str:s$ >> in
      let lid_patt s = <:patt< $lid:s$ >> in
      let lid_expr s = <:expr< $lid:s$ >> in

      (* Expressions *)
      let cvars, bvars, tparams = split_params params in
      let all_ids, cvar_ids, bvar_ids, tparam_ids = name_params params in
      let cvars_expr = List.map string_expr cvars in
      let cvars_expr' = <:expr< [| $list:cvars_expr$ |] >> in
      let bvars_expr = List.map string_expr bvars in
      let bvars_expr' = <:expr< [| $list:bvars_expr$ |] >> in
      let params_expr = list_expr loc (expr_of_term loc) tparams in
      let subgoals_expr = list_expr loc (expr_of_term loc) args in
      let redex_expr = expr_of_term loc redex in
      let con_expr = expr_of_term loc contractum in
      let create_expr =
         <:expr< $create_cond_rewrite_expr loc$ $lid:local_refiner_id$ $str:name$ (**)
            $lid:bvars_id$ $lid:params_id$ $lid:subgoals_id$ $lid:redex_id$ $lid:contractum_id$ >>
      in
      let prim_expr =
         match expr with
            Some expr ->
               <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
                  $lid:bvars_id$ $lid:params_id$ $lid:subgoals_id$ $lid:redex_id$ $lid:contractum_id$ $expr$ >>
          | None ->
               <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
                  $lid:bvars_id$ $lid:params_id$ $lid:subgoals_id$ $lid:redex_id$ $lid:contractum_id$ >>
      in
      let rw_fun_expr =
         let addr_expr id = <:expr< $lid:id$ >> in
         let cvars_id_expr = <:expr< [| $list:List.map addr_expr cvar_ids$ |] >> in
         let bvars_id_expr = <:expr< [| $list:List.map lid_expr bvar_ids$ |] >> in
         let tparams_ids_expr = list_expr loc lid_expr tparam_ids in
         let body = <:expr< $rewrite_of_cond_rewrite_expr loc$ $lid:rw_id$
                                ( $list:[ bvars_id_expr; tparams_ids_expr ]$ ) >>
         in
            fun_expr loc all_ids body
      in

      (* Let construction *)
      let body =
         <:expr< let $rec:false$ $list:[ cvars_patt, cvars_expr';
                                         bvars_patt, bvars_expr';
                                         params_patt, params_expr;
                                         subgoals_patt, subgoals_expr;
                                         redex_patt, redex_expr;
                                         con_patt, con_expr ]$
                 in
                 let $rec:false$ $list:[ name_patt, create_expr;
                                         wild_patt, prim_expr ]$
                 in
                    $lid:name$ >>
       in
       let name_rewrite_let = <:str_item< value $rec:false$ $list:[ rw_patt, wrap_exn loc name body ]$ >> in
       let name_let = <:str_item< value $rec:false$ $list:[ name_patt, rw_fun_expr ]$ >> in
          checkpoint_resources want_checkpoint loc name  (**)
             [name_rewrite_let; name_let; refiner_let loc; toploop_rewrite proc loc name params]

   let () = ()

   let prim_rewrite proc loc rw =
      define_rewrite false (prim_rewrite_expr loc) proc loc rw None

   let prim_cond_rewrite proc loc crw =
      define_cond_rewrite false (prim_cond_rewrite_expr loc) proc loc crw None

   (*
    * Justify a rewrite with a tactic.
    *)
   let derived_rewrite proc loc rw expr =
      define_rewrite true (derived_rewrite_expr loc) proc loc rw (Some expr)

   let derived_cond_rewrite proc loc crw expr =
      define_cond_rewrite true (derived_cond_rewrite_expr loc) proc loc crw (Some expr)

   (*
    * Interactive forms.
    *)
   let interactive_rewrite proc loc rw expr =
      define_rewrite true (delayed_rewrite_expr loc) proc loc rw (Some (Convert.to_expr proc.imp_arg rw.rw_name expr))

   let interactive_cond_rewrite proc loc crw expr =
      define_cond_rewrite true (delayed_cond_rewrite_expr loc) proc loc crw (Some (Convert.to_expr proc.imp_arg crw.crw_name expr))

   (*
    * Incomplete forms.
    *)
   let incomplete_rewrite proc loc rw =
      define_rewrite true (delayed_rewrite_expr loc) proc loc rw (Some (interactive_exn loc "rewrite"))

   let incomplete_cond_rewrite proc loc crw =
      define_cond_rewrite true (delayed_cond_rewrite_expr loc) proc loc crw (Some (interactive_exn loc "rewrite"))

   (*
    * An ML rewrite performs the same action as a conditional rewrite,
    * but the ML code computes the rewrite.
    *
    * Constructed code:
    * let name_rewrite =
    *     ... header produced by define_ml_program ...
    *     let rewrite names bnames params seq goal =
    *        let stack = apply_redex redex [||] goal params in
    *        let goal, subgoals, extract = rewrite_code in
    *           goal, subgoals, namer stack names, extract
    *     in
    *        create_ml_cond_rewrite refiner name info rewrite
    * let name x = rewrite_of_cond_rewrite name_rewrite x
    *)
   let define_ml_rewrite want_checkpoint proc loc
       { mlterm_name       = name;
         mlterm_params     = params;
         mlterm_term       = redex;
         mlterm_contracta  = contracta
       } rewrite_expr =
      let info_patt = <:patt< $lid:info_id$ >> in
      let rw_id = "_$" ^ name ^ "_rewrite" in
      let rw_patt = <:patt< $lid:rw_id$ >> in
      let name_patt = <:patt< $lid:name$ >> in

      let rewrite_val_expr = <:expr< $lid:rewrite_id$ >> in
      let rewrite_val_patt = <:patt< $lid:rewrite_id$ >> in

      let string_expr s = <:expr< $str:s$ >> in
      let lid_patt s = <:patt< $lid:s$ >> in
      let lid_expr s = <:expr< $lid:s$ >> in

      let cvars, bvars, tparams = split_params params in
      let all_ids, cvar_ids, bvar_ids, tparam_ids = name_params params in
      let cvars_expr = List.map string_expr cvars in
      let cvars_expr' = <:expr< [| $list:cvars_expr$ |] >> in
      let bvars_expr = List.map string_expr bvars in
      let bvars_expr' = <:expr< [| $list:bvars_expr$ |] >> in
      let params_expr = list_expr loc (expr_of_term loc) tparams in
      let redex_expr = expr_of_term loc redex in
      let simple_flag = params = [] && bvars = [] in

      let create_ml_rewrite_expr =
         if simple_flag then
            create_ml_rewrite_expr loc
         else
            create_ml_cond_rewrite_expr loc
      in
      let create_expr =
         (<:expr< $create_ml_rewrite_expr$ $lid:local_refiner_id$ $str:name$ $lid:info_id$ >>)
      in
      let info_let =
         <:expr< let $rec:false$ $list:[info_patt, rewrite_val_expr]$ in $create_expr$ >>
      in
      let rewrite_body = <:expr< $lid:namer_id$ $lid:stack_id$ $lid:names_id$ >> in
      let rewrite_body = <:expr< ( $lid:goal_id$ , $lid:subgoals_id$ , $rewrite_body$ , $lid:extract_id$ ) >> in
      let rewrite_patt = <:patt< ( $lid:goal_id$ , $lid:subgoals_id$ , $lid:extract_id$ ) >> in
      let rewrite_body' = <:expr< $rewrite_expr$ $lid:goal_id$ >> in
      let rewrite_body =
         if simple_flag then
            rewrite_body'
         else
            <:expr< let $rec:false$ $list:[ rewrite_patt, rewrite_body' ]$ in $rewrite_body$ >>
      in
      let params_expr =
         if simple_flag then
            <:expr< [] >>
         else
            <:expr< $lid:params_id$ >>
      in
      let rewrite_patt = <:patt< $lid:stack_id$ >> in
      let rewrite_body' = <:expr< $apply_redex_expr loc$ $lid:redex_id$ [||] $lid:goal_id$ $params_expr$ >> in
      let rewrite_body = <:expr< let $rec:false$ $list:[ rewrite_patt, rewrite_body' ]$ in $rewrite_body$ >> in
      let rewrite_patt = <:patt< $lid:rewrite_id$ >> in
      let args_id =
         if simple_flag then
            [goal_id]
         else
            [names_id; bnames_id; params_id; seq_id; goal_id]
      in
      let rewrite_let  = <:expr< let $rec:false$ $list:[ rewrite_patt, curry loc args_id rewrite_body ]$ in $info_let$ >> in
      let body = define_ml_program proc loc strict_expr bvars tparams name redex contracta rewrite_let in

      let rw_fun_expr =
         let addr_expr id = <:expr< $lid:id$ >> in
         let cvars_id_expr = <:expr< [| $list:List.map addr_expr cvar_ids$ |] >> in
         let bvars_id_expr = <:expr< [| $list:List.map lid_expr bvar_ids$ |] >> in
         let tparams_ids_expr = list_expr loc lid_expr tparam_ids in
         let body =
            if simple_flag then
               <:expr< $rewrite_of_rewrite_expr loc$ $lid:rw_id$ >>
            else
               <:expr< $rewrite_of_cond_rewrite_expr loc$ $lid:rw_id$
                                ( $list:[ bvars_id_expr; tparams_ids_expr ]$ ) >>
         in
            fun_expr loc all_ids body
      in

      let name_rewrite_let = <:str_item< value $rec:false$ $list:[ rw_patt, wrap_exn loc name body ]$ >> in
      let name_let = <:str_item< value $rec:false$ $list:[ name_patt, rw_fun_expr ]$ >> in
         checkpoint_resources want_checkpoint loc name [name_rewrite_let; name_let; refiner_let loc]

   (************************************************************************
    * RULES                                                                *
    ************************************************************************)

   (*
    * Define the resources for a rule.
    * The Tactic_type.pre_tactic is passed as an argument,
    * along with the params, so that we can figure out its type.
    *)
   let define_rule_resources proc loc name cvars_id tvars_id avars_id params_id assums_id resources name_rule_expr =
      let define_resource (loc, name', args) =
         let input = res_type proc loc name' in
         let arg_expr =
            match args with
               [] ->
                  name_rule_expr
             | _ ->
                  <:expr< ( $list:name_rule_expr :: args$ ) >>
         in
         let process_name = "process_" ^ name' ^ "_resource_annotation" in
         let anno_name = "_$" ^ name' ^ "_resource_annotation" in
         let process_type = <:ctyp< Mp_resource.annotation_processor '$anno_name$ $input$ >> in
         let process_expr = <:expr< ($lid:process_name$ : $process_type$) >> in
         let expr =
            <:expr< $process_expr$ $str:name$ $lid:cvars_id$ $lid:tvars_id$ $lid:avars_id$ $lid:params_id$ $lid:assums_id$ $arg_expr$ >>
         in
            impr_resource proc loc name' expr
      in
         List.map define_resource resources

   (*
    * Split var params from regular params.
    * let name_rule =
    *    let cvars = cvars_expr
    *    and tvars = tvars_expr
    *    and avars = avars_expr
    *    and params = tparams_expr
    *    and assums = assums_expr
    *    and extract = extract_expr
    *    in
    *    let rule = create_rule refiner "name" cvars tvars params assums
    *    and _ = prim_rule refiner "name" tvars params avars extract
    *    in
    *       compile_rule !refiner rule
    * let name params x = tactic_of_rule name_rule ([| cvars |], [| vars |]) [non_vars] x
    *)
   let define_rule want_checkpoint code proc loc
       { rule_name = name;
         rule_params = params;
         rule_stmt = stmt;
         rule_resources = resources
       }
       extract =
      (* Check the specifications *)
      let string s   = <:expr< $str:s$ >> in
      let lid_patt s = <:patt< $lid:s$ >> in
      let lid_expr s = <:expr< $lid:s$ >> in

      (* Expressions *)
      let name_rule_id = "_$" ^ name ^ "_rule" in
      let cvars, tvars, tparams = split_params params in
      let all_ids, cvar_ids, tvar_ids, tparam_ids = name_params params in
      let cvars_expr = List.map string cvars in
      let cvars_expr' = <:expr< [| $list:cvars_expr$ |] >> in
      let tvars_expr = List.map string tvars in
      let tvars_expr' = <:expr< [| $list:tvars_expr$ |] >> in
      let labels, avars, mterm = split_mfunction stmt in
      let labels_expr = list_expr loc (expr_of_label loc) labels in
      let avars_expr = list_expr loc (expr_of_term loc) avars in
      let tparams_expr = list_expr loc (expr_of_term loc) tparams in
      let assums_expr = expr_of_mterm loc mterm in
      let axiom_value =
         <:expr< $create_rule_expr loc$ $lid:local_refiner_id$ $str:name$ (**)
            $lid:cvars_id$ $lid:tvars_id$ $lid:params_id$ $lid:assums_id$ >>
      in
      let thm_value =
         <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
            $lid:tvars_id$ $lid:params_id$ $lid:avars_id$ $lid:extract_id$ >>
      in
      let cvars_patt, tvars_patt, avars_patt, params_patt, assums_patt,
          extract_patt, rule_patt, name_patt, name_rule_patt,
          name_rule_expr, labels_patt, wild_patt =
         lid_patt cvars_id, lid_patt tvars_id, lid_patt avars_id,
         lid_patt params_id, lid_patt assums_id, lid_patt extract_id,
         lid_patt rule_id, lid_patt name, lid_patt name_rule_id,
         lid_expr name_rule_id, lid_patt labels_id, <:patt< _ >>
      in
      let name_value =
         let addr_expr id = <:expr< $lid:id$ >> in
         let cvars_id_expr = <:expr< [| $list:List.map addr_expr cvar_ids$ |] >> in
         let tvars_id_expr = <:expr< [| $list:List.map lid_expr tvar_ids$ |] >> in
         let tparams_ids_expr = list_expr loc lid_expr tparam_ids in
         let body = <:expr< $tactic_of_rule_expr loc$ $name_rule_expr$
                            ( $list:[ cvars_id_expr; tvars_id_expr ]$ )
                            $tparams_ids_expr$ $lid:x_id$ >>
         in
            fun_expr loc (all_ids @ [x_id]) body
      in
      let rule_expr = <:expr< $compile_rule_expr loc$ $lid:local_refiner_id$ $lid:labels_id$ $lid:rule_id$ >> in
      let resource_exprs =
         define_rule_resources proc loc name (**)
            cvars_id tvars_id params_id avars_id assums_id
            resources name_rule_expr
      in
      let rule_expr =
         (<:expr< let $rec:false$ $list:[ cvars_patt, cvars_expr';
                                          tvars_patt, tvars_expr';
                                          avars_patt, avars_expr;
                                          params_patt, tparams_expr;
                                          assums_patt, assums_expr;
                                          labels_patt, labels_expr;
                                          extract_patt, extract ]$
                  in
                  let $rec:false$ $list:[ rule_patt, axiom_value;
                                          wild_patt, thm_value ]$
                  in
                  let $rec:false$ $list:[ name_rule_patt, rule_expr ]$ in
                     ( do { $list:(resource_exprs @[name_rule_expr])$ } )
          >>)
      in
      let rule_def = <:str_item< value $rec:false$ $list:[ name_rule_patt, wrap_exn loc name rule_expr ]$ >> in
      let tac_def = <:str_item< value $rec:false$ $list:[ name_patt, name_value ]$ >> in
         checkpoint_resources want_checkpoint loc name (**)
            [rule_def; tac_def; refiner_let loc; toploop_rule proc loc name params]

   let prim_rule proc loc ax extract =
      let code = prim_rule_expr loc in
      let extract_expr = expr_of_term loc extract in
         define_rule false code proc loc ax extract_expr

   let derived_rule proc loc ax tac =
      let code = derived_rule_expr loc in
         define_rule true code proc loc ax tac

   let interactive_rule proc loc ax expr =
      let code = delayed_rule_expr loc in
         define_rule true code proc loc ax (Convert.to_expr proc.imp_arg ax.rule_name expr)

   let incomplete_rule proc loc ax =
      let code = delayed_rule_expr loc in
         define_rule true code proc loc ax (interactive_exn loc "rule")

   let () = ()

   (*
    * An ML rewrite performs the same action as a conditional rewrite,
    * but the ML code computes the rewrite.
    *
    * Constructed code:
    * let name_rewrite =
    *     ... header produced by define_ml_program ...
    *     let rewrite names bnames params seq goal =
    *        let stack = apply_redex redex [||] goal params in
    *        let goal, subgoals = rewrite_code in
    *           goal, subgoals, namer stack names
    *     in
    *     let extract names params subgoals =
    *        extract_code
    *     in
    *     let info =
    *        { ml_rewrite_rewrite = rewrite;
    *          ml_rewrite_extract = extract
    *        }
    *     in
    *        create_ml_rewrite refiner name info
    * let name x = rewrite_of_cond_rewrite name_rewrite x
    *)
   let define_ml_rule want_checkpoint proc loc
       { mlterm_name       = name;
         mlterm_params     = params;
         mlterm_term       = redex;
         mlterm_contracta  = contracta
       } rule_expr =
      (* Names *)
      let info_patt = <:patt< $lid:info_id$ >> in
      let name_rule_id = "_$" ^ name ^ "_rule" in
      let name_patt = <:patt< $lid:name$ >> in

      let rule_val_expr = <:expr< $lid:rule_id$ >> in
      let extract_val_expr = <:expr< $lid:extract_id$ >> in
      let rule_val_patt = <:patt< $lid:rule_id$ >> in
      let extract_val_patt = <:patt< $lid:extract_id$ >> in

      let string_expr s = <:expr< $str:s$ >> in
      let lid_patt s = <:patt< $lid:s$ >> in
      let lid_patt_ s = <:patt< $lid: "_" ^ s$ >> in
      let lid_expr s = <:expr< $lid:s$ >> in

      let goal_id, rule_expr = beta_reduce_var goal_id rule_expr in

      let cvars, bvars, tparams = split_params params in
      let all_ids, cvar_ids, bvar_ids, tparam_ids = name_params params in
      let cvars_expr = List.map string_expr cvars in
      let cvars_expr' = <:expr< [| $list:cvars_expr$ |] >> in
      let bvars_expr = List.map string_expr bvars in
      let bvars_expr' = <:expr< [| $list:bvars_expr$ |] >> in
      let params_expr = list_expr loc (expr_of_term loc) tparams in
      let redex_expr = expr_of_term loc redex in

      let create_expr =
         <:expr< $compile_labeled_rule_expr loc$ $lid:local_refiner_id$ ($create_ml_rule_expr loc$ $lid:local_refiner_id$ $str:name$ $lid:info_id$) >>
      in
      let info_let =
         <:expr< let $rec:false$ $list:[info_patt, rule_val_expr]$ in $create_expr$ >>
      in
      let rule_patt =
         <:patt< ( [| $list:List.map lid_patt_ cvars$ |], [| $list:List.map lid_patt_ bvars$ |] ) >>
      in
      let wild_patt = <:patt< _ >> in
      let wild_expr = <:expr< failwith "bad match" >> in
      let rule_expr =
         <:expr< match ( $lid:addrs_id$ , $lid:names_id$ ) with
                 [ $list: [rule_patt, None, rule_expr; wild_patt, None, wild_expr]$ ] >>
      in
      let rule_body = <:expr< $lid:namer_id$ $lid:stack_id$ $lid:names_id$ >> in
      let rule_body = <:expr< ( $lid:subgoals_id$ , $rule_body$ , $lid:extract_id$ ) >> in
      let rule_patt = <:patt< ( $lid:subgoals_id$ , $lid:extract_id$ ) >> in
      let rule_body = <:expr< let $rec:false$ $list:[ rule_patt, rule_expr ]$ in $rule_body$ >> in
      let rule_patt = <:patt< $lid:stack_id$ >> in
      let rule_body' = <:expr< $apply_redex_expr loc$ $lid:redex_id$ $lid:addrs_id$ $lid:msequent_goal_id$ $lid:params_id$ >> in
      let rule_body = <:expr< let $rec:false$ $list:[ rule_patt, rule_body' ]$ in $rule_body$ >> in
      let rule_patt = <:patt< ( $lid:msequent_goal_id$ , $lid:msequent_hyps_id$ ) >> in
      let rule_expr = <:expr< $dest_msequent_expr loc$ $lid:goal_id$ >> in
      let rule_body = <:expr< let $rec:false$ $list:[ rule_patt, rule_expr ]$ in $rule_body$ >> in
      let rule_patt = <:patt< $lid:rule_id$ >> in
      let rule_let  = <:expr< let $rec:false$ $list:[ rule_patt, curry loc [addrs_id; names_id; goal_id; params_id] rule_body ]$ in $info_let$ >> in
      let body = define_ml_program proc loc strict_expr cvars tparams name redex contracta rule_let in

      let rule_fun_expr =
         let addr_expr id = <:expr< $lid:id$ >> in
         let cvars_id_expr = <:expr< [| $list:List.map addr_expr cvar_ids$ |] >> in
         let tvars_id_expr = <:expr< [| $list:List.map lid_expr bvar_ids$ |] >> in
         let tparams_ids_expr = list_expr loc lid_expr tparam_ids in
         let body = <:expr< $tactic_of_rule_expr loc$ $lid:name_rule_id$
                            ( $list:[ cvars_id_expr; tvars_id_expr ]$ )
                            $tparams_ids_expr$ $lid:x_id$ >>
         in
            fun_expr loc (all_ids @ [x_id]) body
      in

      let rule_patt = <:patt< $lid:name_rule_id$ >> in
      let name_rule_let =
         <:str_item< value $rec:false$ $list:[ rule_patt, wrap_exn loc (name ^ "_rule") body ]$ >>
      in
      let name_let =
         <:str_item< value $rec:false$ $list:[ name_patt, rule_fun_expr ]$ >>
      in
         checkpoint_resources want_checkpoint loc name [name_rule_let; name_let; refiner_let loc]

   let create_dform_expr loc modes =
      let string_expr s = <:expr< $str:s$ >> in
      match modes with
         Modes modes -> <:expr< $create_dform_modes_expr loc$ $list_expr loc string_expr modes$ >>
       | ExceptModes modes -> <:expr< $create_dform_except_modes_expr loc$ $list_expr loc string_expr modes$ >>
       | AllModes -> create_dform_all_expr loc

   (*
    * Define a display form expansion.
    *
    * create_dform{_modes,_except_modes,_all} [modes] dformer
    *    { dform_pattern = t;
    *      dform_options = [options];
    *      dform_print = DFormExpansion expansion
    *    }
    *)
   let define_dform proc loc
       { dform_name = name;
         dform_modes = modes;
         dform_options = options;
         dform_redex = t
       }
       expansion =
      let name_expr = <:expr< $str: name$ >> in
      let options_expr = list_expr loc (dform_option_expr loc) options in
      let expansion_expr = <:expr< $dform_expansion_expr loc$ $expr_of_term loc expansion$ >> in
      let t_expr = expr_of_term loc t in
      let rec_value =
         <:expr< { $list:[ dform_name_patt loc, name_expr;
                           dform_pattern_patt loc, t_expr;
                           dform_options_patt loc, options_expr;
                           dform_print_patt loc, expansion_expr ]$ } >>
      in
      let expr = <:expr< $create_dform_expr loc modes$ $lid:local_dformer_id$ $rec_value$ >> in
         [<:str_item< $exp: wrap_exn loc name expr$ >>]

   (*
    * Precedence definition relation.
    *)
   let define_prec proc loc s =
      let prec_patt = <:patt< $lid:s$ >> in
      let new_prec = <:expr< $new_prec_expr loc$ () >> in
         [<:str_item< value $rec:false$ $list:[ prec_patt, new_prec ]$ >>]

   let define_prec_rel proc loc
       { prec_left = s;
         prec_right = s';
         prec_rel = rel
       } =
      let expr =
         match rel with
            NoRelation ->
               <:expr< () >>
          | LTRelation ->
               <:expr< $add_lt_expr loc$ $lid:s$ $lid:s'$ >>
          | EQRelation ->
               <:expr< $add_eq_expr loc$ $lid:s$ $lid:s'$ >>
          | GTRelation ->
               <:expr< $add_lt_expr loc$ $lid:s'$ $lid:s$ >>
      in
      let name = sprintf "%s..%s" s s' in
         [<:str_item< $exp:wrap_exn loc name expr$ >>]

   (*
    * Pattern to match rewrite destruction.
    *)
   let rewrite_type_patt loc = function
      RewriteTermType name ->
         <:patt< $rewriter_patt loc$ . RewriteTerm $lid:name$ >>
    | RewriteFunType name ->
         <:patt< $rewriter_patt loc$ . RewriteFun $lid:name$ >>
    | RewriteContextType name ->
         <:patt< $rewriter_patt loc$ . RewriteContext $lid:name$ >>
    | RewriteStringType name ->
         <:patt< $rewriter_patt loc$ . RewriteString $lid:name$ >>
    | RewriteNumType name ->
         <:patt< $rewriter_patt loc$ . RewriteNum $lid:name$ >>
    | RewriteLevelType name ->
         <:patt< $rewriter_patt loc$ . RewriteLevel $lid:name$ >>

   (*
    * An ml dterm is a display form that is computed in ML.
    *
    * Within the body, terms may expand to contracta.
    *
    * This is the code we create:
    * let _ =
    *    ... define_ml_program proc loc t ...
    *    let printer { dform_term = term
    *                  dform_items = items;
    *                  dform_printer = printer;
    *                  dform_buffer = buffer
    *                } =
    *       code
    *    in
    *       create_dform{_modes,_except_modes,_all} [modes] name
    *          { dform_pattern = t;
    *            dform_options = [options];
    *            dform_print = DFormPrinter printer
    *          }
    *)
   let define_ml_dform proc loc
       { dform_name = name;
         dform_modes = modes;
         dform_options = options;
         dform_redex = t
       }
       { dform_ml_printer = printer;
         dform_ml_buffer = buffer;
         dform_ml_contracta = cons;
         dform_ml_code = code
       } =
      (* Dform info *)
      let string_expr s = <:expr< $str:s$ >> in
      let name_expr = <:expr< $str: name$ >> in
      let options_expr = list_expr loc (dform_option_expr loc) options in

      (* Identifier names *)
      let term_patt = <:patt< $lid:term_id$ >> in
      let redex_patt = <:patt< $lid:redex_id$ >> in
      let dprinter_patt = <:patt< $lid:dprinter_id$ >> in
      let printer_patt = <:patt< $lid:printer$ >> in
      let buffer_patt = <:patt< $lid:buffer$ >> in
      let stack_patt = <:patt< $lid:stack_id$ >> in

      let term_expr = <:expr< $lid:term_id$ >> in
      let redex_expr = <:expr< $lid:redex_id$ >> in
      let dprinter_expr = <:expr< $lid:dprinter_id$ >> in
      let printer_expr = <:expr< $lid:printer$ >> in
      let buffer_expr = <:expr< $lid:buffer$ >> in

      (* Items *)
      let redex, _ = compile_redex Relaxed [||] t in
      let items = extract_redex_types redex in
      let items_patt = list_patt loc (rewrite_type_patt loc) items in

      (* Build the program *)
      let dprinter = <:expr< $dform_printer_expr loc$ $dprinter_expr$ >> in
      let rec_value =
         <:expr< { $list:[ dform_name_patt loc, name_expr;
                           dform_pattern_patt loc, term_expr;
                           dform_options_patt loc, options_expr;
                           dform_print_patt loc, dprinter ]$ } >>
      in
      let body_expr =
         <:expr< $create_dform_expr loc modes$ $lid:local_dformer_id$ $rec_value$ >>
      in
      let dprinter_rec_patt =
         <:patt< { $list:[ dform_term_patt loc, term_patt;
                           dform_items_patt loc, items_patt;
                           dform_printer_patt loc, printer_patt;
                           dform_buffer_patt loc, buffer_patt ]$ } >>
      in
      let wild_patt = <:patt< _ >> in
      let wild_code = <:expr< failwith "bad match" >> in
      let code_expr = <:expr< $code$ $lid:term_id$ >> in
      let dprinter_fun_expr =
         <:expr< fun [ $list:[dprinter_rec_patt, None, code_expr; wild_patt, None, wild_code]$ ] >>
      in
      let dprinter_let_expr =
         <:expr< let $rec:false$ $list:[ dprinter_patt, dprinter_fun_expr ]$ in $body_expr$ >>
      in
      let expr = define_ml_program proc loc relaxed_expr [] [] name t cons dprinter_let_expr in
         [<:str_item< $exp:expr$ >>]

   let _ = ()

   (*
    * Record a resource.
    *
    * type resource_name
    *)
   let define_resource proc loc name expr =
      let { resource_input = input;
            resource_output = output
          } =
         try
            List.assoc name (get_resources proc.imp_sig_info)
         with
            Not_found ->
               Stdpp.raise_with_loc loc (Failure("Tries to implement undeclared resource " ^ name))
      in
      let get_patt = <:patt< $lid:get_resource_name name$ >> in
      let intermediate = "_$" ^ name ^ "_resource_intermediate" in
      let create_type = <:ctyp< Mp_resource.resource_info $input$ '$intermediate$ $output$ >> in
      let create_expr = <:expr< Mp_resource.create_resource $str:name$ ( $expr$ : $create_type$ ) >> in
      let input_name = input_type name in
         proc.imp_resources
            <- (name, <:ctyp< $lid:input_name$ >>) :: proc.imp_resources;
         [<:str_item< type $input_name$ = $input$ >>;
          <:str_item< value $rec:false$ $list:[get_patt, create_expr]$ >>]

   let rec is_list_expr = function
      MLast.ExUid(_,"[]") -> true
    | MLast.ExApp(_,MLast.ExApp(_,MLast.ExUid(_,"::"),_),tail) ->
         is_list_expr tail
    | _ -> false

   let improve_resource proc loc { improve_name = name; improve_expr = expr } =
      let improve_expr =
         if is_list_expr expr then impr_resource_list proc loc name expr
         else impr_resource proc loc name expr
      in
         [<:str_item< ($improve_expr$) >> ]

   (*
    * When a parent is included, we need to open all the ancestors,
    * and we need to patch in all the resources.
    *)
   let define_parent proc loc
       { parent_name = path;
         parent_resources = nresources
       } =
      let parent_path = parent_path_expr loc path in
      let joins =
         let parent_refiner = (<:expr< $parent_path$ . $lid: refiner_id$ >>) in
         let parent_dformer = (<:expr< $parent_path$ . $lid: dformer_id$ >>) in
         let refiner_expr = (<:expr< $join_refiner_expr loc$ $lid: local_refiner_id$ $parent_refiner$ >>) in
         let dformer_expr = (<:expr< $join_mode_base_expr loc$ $lid: local_dformer_id$ $parent_dformer$ >>) in
         let refiner_item = (<:str_item< $exp: refiner_expr$ >>) in
         let refiner_let  = refiner_let loc in
         let dformer_item = (<:str_item< $exp: dformer_expr$ >>) in
            [refiner_item; refiner_let; dformer_item]
      in let rec find_resource name = function
         [] ->
            Stdpp.raise_with_loc loc (Invalid_argument("Resource " ^ name ^ " not known by cached info"))
       | (path, name', _) :: _ when name = name' ->
            path
       | _ :: t ->
            find_resource name t
      in let make_ctyp (name, _) =
         let path = find_resource name proc.imp_all_resources in
         (name, <:ctyp< $parent_path_ctyp loc path$ . $lid:input_type name$ >>)
      in
         proc.imp_resources <- proc.imp_resources @ (List.map make_ctyp nresources);
         match path with
            [name] ->
               <:str_item< Mp_resource.extends_theory $str:name$ >> :: joins
          | _ ->
               Stdpp.raise_with_loc loc (Invalid_argument "Including sub-theories not implemented")


   (*
    * Collect the toploop values in this module.
    *)
   let implem_toploop info =
      let rec collect = function
         (ToploopItem <:sig_item< value $s$ : $t$ >>, loc) :: tl ->
            (s, (t, loc)) :: collect tl
       | _ :: tl ->
            collect tl
       | [] ->
            []
      in
         collect (info_items info)

   (*
    * An regular item.
    *)
   let wrap_toploop_item proc loc ((patt, expr) as item) =
      match patt with
         <:patt< $lid:name$ >> ->
            begin
               try
                  let ctyp, _ = List.assoc name proc.imp_toploop in
                  let expr1 = wrap_toploop_item loc name ctyp expr in
                     (patt, expr1), add_toploop_item proc loc name ctyp
               with
                  Not_found ->
                     item, []
            end
        | _ ->
            item, []

   let wrap_toploop_items proc loc pel =
      let pel, resources = List.split (List.map (wrap_toploop_item proc loc) pel) in
         pel, List.flatten resources

   let define_summary_item proc loc item =
      match item with
         <:str_item< value $rec:rec_flag$ $list:pel$ >> ->
            let pel, toploop = wrap_toploop_items proc loc pel in
            let item1 = <:str_item< value $rec:rec_flag$ $list:pel$ >> in
               item1 :: toploop
       | _ ->
         [item]

   (*
    * A magic block computes a hash value from the definitions
    * in the block.
    *)
   let define_magic_block proc loc { magic_name = name; magic_code = stmts } =
      let index = List.fold_left Filter_hash.hash_str_item 0 stmts in
      let name_patt = <:patt< $lid:name$ >> in
      let index_val = <:expr< $int:string_of_int index$ >> in
      let hash_def = <:str_item< value $rec:false$ $list:[ name_patt, index_val ]$ >> in
         hash_def :: stmts

   (*
    * Prolog declares the refiner and dformer.
    *
    * let refiner_name = ref Refine.Refiner.null_refiner
    * let dformer_name = ref Dform_print.null_mode_base
    *)
   let implem_prolog proc loc name =
      let refiner_val = <:expr< $null_refiner_expr loc name$ >> in
      let dformer_val = <:expr< ref $null_mode_base_expr loc$ >> in
      let refiner_patt = <:patt< $lid:local_refiner_id$ >> in
      let dformer_patt = <:patt< $lid:local_dformer_id$ >> in
         [<:str_item< value $rec:false$ $list:[ refiner_patt, refiner_val; dformer_patt, dformer_val ]$ >>]

   (*
    * Trailing declarations.
    *
    * let _ = Mp_resource.close_theory "module_name"
    * let _ = Refiner.label_refiner refiner_name "module_name"
    * let refiner = !refiner_name
    * let dformer = !dformer_name
    * let theory_name =
    *    { thy_name = "module_name";
    *      thy_refiner = refiner;
    *      thy_dformer = dformer
    *    }
    * let _ = record_theory theory_name
    *)
   let implem_postlog { imp_name = name } loc =
      let thy_elems =
         [(<:patt< Theory.thy_name >>, <:expr< $str:name$ >>);
          (<:patt< Theory.thy_refiner >>, <:expr< $lid:refiner_id$ >>);
          (<:patt< Theory.thy_dformer >>, <:expr< $lid:dformer_id$ >>)]
      in
      let thy_rec = <:expr< { $list:thy_elems$ } >> in
      let thy = <:expr< Theory.record_theory $thy_rec$ >> in
      let refiner_patt = <:patt< $lid:refiner_id$ >> in
      let dformer_patt = <:patt< $lid:dformer_id$ >> in
      let dformer_val = <:expr< $lid:local_dformer_id$ . val >> in
      let label_expr = <:expr< $label_refiner_expr loc$ $lid:local_refiner_id$ $str:name$ >> in
          [<:str_item< Mp_resource.close_theory $str:name$ >>;
          (<:str_item< value $rec:false$
                              $list:[refiner_patt, label_expr;
                                     dformer_patt, dformer_val]$ >>);
          (<:str_item< $exp:thy$ >>)]

   let _ = ()

   (*
    * Now extract the program.
    *)
   let extract_str_item proc (item, loc) =
      match item with
         Rewrite ({ rw_name = name; rw_proof = Primitive _ } as rw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: primrw: %s%t" name eflush;
            prim_rewrite proc loc rw
       | Rewrite ({ rw_name = name; rw_proof = Derived tac } as rw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: rwthm: %s%t" name eflush;
            derived_rewrite proc loc rw tac
       | Rewrite ({ rw_name = name; rw_proof = Incomplete } as rw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: rwincomplete: %s%t" name eflush;
            incomplete_rewrite proc loc rw
       | Rewrite ({ rw_name = name; rw_proof = Interactive pf } as rw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: rwinteractive: %s%t" name eflush;
            interactive_rewrite proc loc rw pf
       | InputForm ({ rw_name = name } as rw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: primrw: %s%t" name eflush;
            define_input_form false proc loc rw
       | CondRewrite ({ crw_name = name; crw_proof = Primitive _ } as crw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: prim condrw: %s%t" name eflush;
            prim_cond_rewrite proc loc crw
       | CondRewrite ({ crw_name = name; crw_proof = Derived tac } as crw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: thm condrw: %s%t" name eflush;
            derived_cond_rewrite proc loc crw tac
       | CondRewrite ({ crw_name = name; crw_proof = Incomplete } as crw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: interactive condrw: %s%t" name eflush;
            incomplete_cond_rewrite proc loc crw
       | CondRewrite ({ crw_name = name; crw_proof = Interactive pf } as crw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: interactive condrw: %s%t" name eflush;
            interactive_cond_rewrite proc loc crw pf
       | MLRewrite ({ mlterm_name = name; mlterm_def = Some rewrite_expr } as mlrw) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: ML rewrite: %s%t" name eflush;
            define_ml_rewrite false proc loc mlrw rewrite_expr
       | MLRewrite ({ mlterm_name = name; mlterm_def = None }) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: ML rewrite (unimplemented): %s%t" name eflush;
            raise (Failure "Filter_prog.extract_str_item: ML rewrite is not defined")
       | Rule ({ rule_name = name; rule_proof = Primitive t } as rule) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: prim rule: %s%t" name eflush;
            prim_rule proc loc rule t
       | Rule ({ rule_name = name; rule_proof = Derived tac } as rule) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: thm rule: %s%t" name eflush;
            derived_rule proc loc rule tac
       | Rule ({ rule_name = name; rule_proof = Incomplete } as rule) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: incomplete rule: %s%t" name eflush;
            incomplete_rule proc loc rule
       | Rule ({ rule_name = name; rule_proof = Interactive pf } as rule) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: interactive rule: %s%t" name eflush;
            interactive_rule proc loc rule pf
       | MLAxiom ({ mlterm_name = name; mlterm_def = Some rule_expr } as mlrule) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: ML axiom: %s%t" name eflush;
            define_ml_rule false proc loc mlrule rule_expr
       | MLAxiom ({ mlterm_name = name; mlterm_def = None }) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: ML axiom unimplemented: %s%t" name eflush;
            raise (Failure "Filter_prog.extract_str_item: ML axiom is not defined")
       | DForm ({ dform_def = TermDForm expansion} as df) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: dform%t" eflush;
            define_dform proc loc df expansion
       | DForm ({ dform_def = MLDForm code} as df) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: dform%t" eflush;
            define_ml_dform proc loc df code
       | DForm { dform_def = NoDForm } ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: dform%t" eflush;
            raise (Failure "Filter_proof.extract_str_item: dform is not defined")
       | Prec name ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: prec: %s%t" name eflush;
            define_prec proc loc name
       | PrecRel rel ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: prec_rel%t" eflush;
            define_prec_rel proc loc rel
       | Resource (name, expr) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: resource: %s%t" name eflush;
            define_resource proc loc name expr
       | Improve ({ improve_name = name } as impr) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: improve %s with ... %t" name eflush;
            improve_resource proc loc impr
       | Parent ({ parent_name = name } as parent) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: parent: %s%t" (string_of_path name) eflush;
            define_parent proc loc parent
       | SummaryItem item ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: summary item%t" eflush;
            define_summary_item proc loc item
       | ToploopItem item ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: toploop item%t" eflush;
            define_summary_item proc loc item
       | MagicBlock block ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: magic block%t" eflush;
            define_magic_block proc loc block
       | Opname _ ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: opname%t" eflush;
            []
       | Id _ ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: id%t" eflush;
            []
       | Comment _ ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: comment%t" eflush;
            []
       | Infix _ ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: infix%t" eflush;
            []
       | Module _ ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: infix%t" eflush;
            raise (Failure "Filter_prog.extract_str_item: nested modules are not implemented")

   (*
    * Extract a signature.
    *)
   let extract_str arg sig_info info resources name =
      let proc = { imp_sig_info = sig_info;
                   imp_resources = [];
                   imp_toploop = implem_toploop sig_info;
                   imp_arg = arg;
                   imp_name = name;
                   imp_all_resources = resources
                 }
      in
      let prolog = implem_prolog proc (0, 0) name in
      let items = List_util.flat_map (extract_str_item proc) (info_items info) in
      let postlog = implem_postlog proc (0, 0) in
         List.map (fun item -> item, (0, 0)) (prolog @ items @ postlog)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
