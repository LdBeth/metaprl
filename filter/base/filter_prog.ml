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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
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
      (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (module_path * MLast.ctyp resource_info) list ->
      string -> (MLast.sig_item * (int * int)) list

   val extract_str :
      arg ->
      (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (term, meta_term, proof proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info ->
      (module_path * MLast.ctyp resource_info) list ->
      string -> (MLast.str_item * (int * int)) list

   (*
    * Defining implementations.
    *)
   type t

   val prim_axiom : t -> loc -> (term, 'proof, MLast.expr) axiom_info -> term -> MLast.str_item list
   val derived_axiom : t -> loc -> (term, 'proof, MLast.expr) axiom_info -> MLast.expr -> MLast.str_item list

   val prim_rule : t -> loc -> (term, meta_term, 'proof, MLast.expr) rule_info -> term -> MLast.str_item list
   val derived_rule : t -> loc -> (term, meta_term, 'proof, MLast.expr) rule_info -> MLast.expr -> MLast.str_item list

   val prim_rewrite : t -> loc -> (term, 'proof, MLast.expr) rewrite_info -> MLast.str_item list
   val derived_rewrite : t -> loc -> (term, 'proof, MLast.expr) rewrite_info -> MLast.expr -> MLast.str_item list

   val prim_cond_rewrite : t -> loc -> (term, 'proof, MLast.expr) cond_rewrite_info -> MLast.str_item list
   val derived_cond_rewrite : t -> loc -> (term, 'proof, MLast.expr) cond_rewrite_info -> MLast.expr -> MLast.str_item list

   val define_dform : t -> loc -> (term, MLast.expr) dform_info -> term -> MLast.str_item list
   val define_prec : t -> loc -> string -> MLast.str_item list
   val define_prec_rel : t -> loc -> prec_rel_info -> MLast.str_item list
   val define_resource : t -> loc -> MLast.ctyp resource_info -> MLast.str_item list
   val define_parent : t -> loc -> MLast.ctyp parent_info -> MLast.str_item list
   val define_magic_block : t -> loc -> MLast.str_item magic_info -> MLast.str_item list

   val implem_prolog : t -> loc -> string -> MLast.str_item list
   val implem_postlog : t -> loc -> string -> MLast.str_item list
end

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

(*
 * Axiom.
 *)
let refiner_expr loc =
   <:expr< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"Refine"$ >>

let refiner_patt loc =
   <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"Refine"$ >>

let refiner_ctyp loc =
   <:ctyp< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"Refine"$ >>

let rewriter_expr loc =
   <:expr< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"Rewrite"$ >>

let rewriter_patt loc =
   <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"Rewrite"$ >>

let tactic_type_expr loc =
   <:expr< $uid:"Tactic_type"$ . $uid: "Tactic"$ >>

let tactic_type_ctyp loc =
   <:ctyp< $uid:"Tactic_type"$ . $uid: "Tactic"$ >>

let rewrite_type_expr loc =
   <:expr< $uid:"Tactic_type"$ . $uid: "Rewrite"$ >>

let rewrite_type_ctyp loc =
   <:ctyp< $uid:"Tactic_type"$ . $uid: "Rewrite"$ >>

let dest_msequent_expr loc =
   <:expr< $refiner_expr loc$ . $lid: "dest_msequent"$ >>

let create_axiom_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"create_axiom"$ >>

let prim_axiom_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"prim_axiom"$ >>

let derived_axiom_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"derived_axiom"$ >>

let delayed_axiom_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"delayed_axiom"$ >>

(*
 * Rule.
 *)
let create_rule_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"create_rule"$ >>

let prim_rule_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"prim_rule"$ >>

let derived_rule_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"derived_rule"$ >>

let delayed_rule_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"delayed_rule"$ >>

let create_ml_rule_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"create_ml_rule"$ >>

let compile_rule_expr loc =
   <:expr< $tactic_type_expr loc$ . $lid:"compile_rule"$ >>

let compile_labeled_rule_expr loc =
   <:expr< $tactic_type_expr loc$ . $lid:"compile_labeled_rule"$ >>

let tactic_of_rule_expr loc =
   <:expr< $tactic_type_expr loc$ . $lid:"tactic_of_rule"$ >>

let tactic_ctyp loc =
   <:ctyp< $tactic_type_ctyp loc$ . $lid:"tactic"$ >>

(*
 * Rewrite.
 *)
let rewrite_ctyp loc =
   <:ctyp< $rewrite_type_ctyp loc$ . $lid:"conv"$ >>

let create_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"create_rewrite"$ >>

let prim_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"prim_rewrite"$ >>

let derived_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"derived_rewrite"$ >>

let delayed_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"delayed_rewrite"$ >>

let create_ml_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"create_ml_rewrite"$ >>

let create_ml_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"create_ml_cond_rewrite"$ >>

let rewrite_of_rewrite_expr loc =
   <:expr< $rewrite_type_expr loc$ . $lid:"rewrite_of_rewrite"$ >>

(*
 * Conditional rewrite.
 *)
let cond_rewrite_ctyp loc =
   <:ctyp< $rewrite_type_ctyp loc$ . $lid:"conv"$ >>

(*
   let sarray = <:ctyp< $lid:"array"$ $lid:"string"$ >> in
   let term = <:ctyp< $lid:"list"$ ($uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"Term"$ . $lid:"term"$) >> in
   let arg = <:ctyp< ($sarray$ * $term$) >> in
      <:ctyp< $arg$ -> $rewrite_ctyp loc$ >>
 *)

let create_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"create_cond_rewrite"$ >>

let prim_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"prim_cond_rewrite"$ >>

let derived_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"derived_cond_rewrite"$ >>

let delayed_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"delayed_cond_rewrite"$ >>

let rewrite_of_cond_rewrite_expr loc =
   <:expr< $rewrite_type_expr loc$ . $lid:"rewrite_of_cond_rewrite"$ >>

let apply_redex_expr loc =
   <:expr< $rewriter_expr loc$ . $lid:"apply_redex"$ >>

let construct_redex_expr loc =
   <:expr< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermMan"$ . $lid:"construct_redex"$ >>

let compile_redex_expr loc =
   <:expr< $rewriter_expr loc$ . $lid:"compile_redex"$ >>

let compile_redices_expr loc =
   <:expr< $rewriter_expr loc$ . $lid:"compile_redices"$ >>

let compile_contractum_expr loc =
   <:expr< $rewriter_expr loc$ . $lid:"compile_contractum"$ >>

let make_contractum_expr loc =
   <:expr< $rewriter_expr loc$ . $lid:"make_contractum"$ >>

let strict_expr loc =
   <:expr< $rewriter_expr loc$ . $uid:"Strict"$ >>

(*
 * Other expressions.
 *)
let thy_name_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"thy_name"$ >>

let thy_refiner_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"thy_refiner"$ >>

let thy_dformer_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"thy_dformer"$ >>

let record_theory_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"record_theory"$ >>

let label_refiner_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"label_refiner"$ >>

let refiner_ctyp loc =
   <:ctyp< $refiner_ctyp loc$ . $lid:"refiner"$ >>

let join_refiner_expr loc =
   <:expr< $refiner_expr loc$ . $lid:"join_refiner"$ >>

let join_mode_base_expr loc =
   <:expr< $uid:"Dform_print"$ . $lid:"join_mode_base"$ >>

let dformer_ctyp loc =
   <:ctyp< $uid:"Dform_print"$ . $lid:"dform_mode_base"$ >>

let resource_rsrc_ctyp loc =
   <:ctyp< $uid:"Mp_resource"$ . $lid:"t"$ >>

let resource_join_expr loc =
   <:expr< $uid:"Mp_resource"$ . $lid:"join"$ >>

let resource_improve_arg_expr loc =
   <:expr< $uid:"Mp_resource"$ . $lid:"improve_arg"$ >>

let ext_resource_name name =
   "ext_" ^ name

let dform_name_expr loc =
   <:expr< $uid:"Dform"$ . $lid:"dform_name"$ >>

let dform_pattern_expr loc =
   <:expr< $uid:"Dform"$ . $lid:"dform_pattern"$ >>

let dform_options_expr loc =
   <:expr< $uid:"Dform"$ . $lid:"dform_options"$ >>

let dform_print_expr loc =
   <:expr< $uid:"Dform"$ . $lid:"dform_print"$ >>

let dform_term_patt loc =
   <:patt< $uid:"Dform"$ . $lid:"dform_term"$ >>

let dform_printer_patt loc =
   <:patt< $uid:"Dform"$ . $lid:"dform_printer"$ >>

let dform_items_patt loc =
   <:patt< $uid:"Dform"$ . $lid:"dform_items"$ >>

let dform_buffer_patt loc =
   <:patt< $uid:"Dform"$ . $lid:"dform_buffer"$ >>

let dform_expansion_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormExpansion"$ >>

let dform_printer_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormPrinter"$ >>

let dform_parens_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormParens"$ >>

let dform_internal_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormInternal"$ >>

let dform_prec_expr loc s =
   <:expr< $uid:"Dform"$ . $uid:"DFormPrec"$ $lid:s$ >>

let dform_inherit_prec_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormInheritPrec"$ >>

let dform_inherit_prec_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormInheritPrec"$ >>

let create_dform_expr loc =
   <:expr< $uid:"Dform_print"$ . $lid:"create_dform"$ >>

let refiner_id = "refiner"
let dformer_id = "dformer"

let local_refiner_id = "_$global_refiner"
let local_dformer_id = "_$global_dformer"
let stack_id = "_$rewrite_stack"

let null_refiner_expr loc name =
   <:expr< $refiner_expr loc$ . $lid:"null_refiner"$ $str: name$ >>

let null_mode_base_expr loc =
   <:expr< $uid:"Dform_print"$ . $lid:"null_mode_base"$ >>

let nil_array loc =
   <:expr< [| $list:[]$ |] >>

let nil_list loc =
   <:expr< [] >>

let precedence_ctyp loc =
   <:ctyp< $uid:"Precedence"$ . $lid:"precedence"$ >>

let new_prec_expr loc =
   <:expr< $uid:"Precedence"$ . $lid:"new_prec"$ >>

let add_lt_expr loc =
   <:expr< $uid:"Precedence"$ . $lid:"add_lt"$ >>

let add_eq_expr loc =
   <:expr< $uid:"Precedence"$ . $lid:"add_eq"$ >>

(*
 * Each axiom gets a refiner associated with it, with the following name.
 *)
let refiner_value loc =
   <:expr< $uid: "Refiner"$ . $uid: "Refiner"$ . $uid: "Refine"$ . $lid: "refiner_of_build"$ $lid: local_refiner_id$ >>

let refiner_let loc =
   let patt = <:patt< $lid: refiner_id$ >> in
      <:str_item< value $rec:false$ $list: [ patt, refiner_value loc ]$ >>

(*
let refiner_name name =
   name ^ "_refiner"

let refiner_patt loc name =
   <:patt< $lid: refiner_name name$ >>

let refiner_let_name loc name =
   let patt = <:patt< $lid: refiner_name name$ >> in
   let expr = <:expr< $lid: refiner_id$ >> in
      <:str_item< value $rec:false$ $list: [ patt, expr ]$ >>
*)

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
   <:expr< $uid: "Ml_term"$ . $lid: "term_of_string"$ $str: s$ >>

let expr_of_mterm loc t =
   let s = Ml_term.string_of_mterm t in
   <:expr< $uid: "Ml_term"$ . $lid: "mterm_of_string"$ $str: s$ >>

let expr_of_label loc = function
   [] ->
      <:expr< $uid:"None"$ >>
 | h :: _ ->
      <:expr< $uid:"Some"$ $str: h$ >>

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
   let stderr = <:expr< $uid: "Pervasives"$ . $lid: "stderr"$ >> in
   let dform = <:expr< $lid: local_dformer_id$ . $lid: "val"$ >> in
   let printer = <:expr< $uid: "Refine_exn"$ . $lid: "print_exn"$ $dform$ $stderr$ $str: name$ $exn_expr$ >> in
   let wrapped = <:expr< try $e$ with [ $list: [exn_patt, None, printer]$ ] >> in

   (* Print a message before the execution *)
   let show_loading = <:expr< $uid: "Mp_debug"$ . $lid: "show_loading"$ >> in
   let msg = <:expr< $str: "Loading " ^ name ^ "%t"$ >> in
   let loading_msg = <:expr< $show_loading$ $msg$ >> in
      <:expr< do $list: [ loading_msg ]$ return $wrapped$ >>

(*
 * Param expression.
 *)
let param_expr loc = function
   ContextParam s ->
      <:expr< $uid:"Filter_summary"$ . $uid:"ContextParam"$ $str:s$ >>
 | VarParam v ->
      <:expr< $uid:"Filter_summary"$ . $uid:"VarParam"$ $str:v$ >>
 | TermParam t ->
      let t' = expr_of_term loc t in
         <:expr< $uid:"Filter_summary"$ . $uid:"TermParam"$ $t'$ >>

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
                  <:ctyp< $lid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermAddr"$ . $lid:"address"$ >>
             | VarParam _ ->
                  <:ctyp< $lid:"string"$ >>
             | TermParam _ ->
                  <:ctyp< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"Term"$ . $lid:"term"$ >>
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
   let ename = <:expr< $uid:"Refiner"$ . $uid: "Refiner"$ . $uid: "RefineError"$ . $uid: "RefineError"$ >> in
   let err = <:expr< $uid:"Refiner"$ . $uid: "Refiner"$ . $uid: "RefineError"$ . $uid: "StringError"$ $str: "proof is incomplete"$ >> in
   let body = <:expr< raise ($ename$ ($str: name$, $err$)) >> in
 *)
   let body = <:expr< raise ($uid:"Failure"$ $str: "interactive proof"$) >> in
      <:expr< fun [ $list: [ patt, None, body ]$ ] >>

(*
 * This is a little bogus, but we add rewrites automatically to the
 * toploop resource.
 *)
let toploop_rewrite loc name =
   let patt = <:patt< $lid: "toploop_resource"$ >> in
   let expr = <:expr< $uid: "Mp_resource"$ . $lid: "improve"$
                      $lid: "toploop_resource"$
                      ($str: name$, $uid: "Mptop"$ . $uid: "ConvExpr"$ $lid: name$) >>
   in
      <:str_item< value $rec: false$ $list: [ patt, expr ]$ >>

let toploop_rule loc name params =
   let rec loop i body = function
      h :: t ->
         let v = "v" ^ string_of_int i in
         let body = <:expr< $body$ $lid: v$ >> in
         let v = <:patt< $lid:v$ >> in
         let expr = <:expr< fun [ $list: [v, None, loop (succ i) body t]$ ] >> in
         let expr =
            match h with
               ContextParam _ ->
                  <:expr< $uid:"Mptop"$ . $uid:"AddressFunExpr"$ $expr$ >>
             | VarParam _ ->
                  <:expr< $uid:"Mptop"$ . $uid:"StringFunExpr"$ $expr$ >>
             | TermParam _ ->
                  <:expr< $uid:"Mptop"$ . $uid:"TermFunExpr"$ $expr$ >>
         in
            expr
    | [] ->
        <:expr< $uid:"Mptop"$ . $uid:"TacticExpr"$ $body$ >>
   in
   let patt = <:patt< $lid: "toploop_resource"$ >> in
   let expr = loop 0 <:expr< $lid: name$ >> params in
   let expr = <:expr< $uid: "Mp_resource"$ . $lid: "improve"$
                      $lid: "toploop_resource"$
                      ($str: name$, $expr$) >>
   in
      <:str_item< value $rec: false$ $list: [ patt, expr ]$ >>

(*
 * Add a toploop item.
 *)
let raise_toploop_exn loc =
   Stdpp.raise_with_loc loc (RefineError ("topval", StringError
                                          "The types allowed in toploop expressions are limited.\n\
Your type is not understood.  See the module Mptop for allowed types."))

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
   <:expr< $uid: "Tactic_type"$ . $uid: "Tacticals"$ . $lid: "wrapT"$ >>

let wrap_optimized loc name arglist_name vars expr =
   let name = <:expr< $str:name$ >> in
      if vars = [] then
         <:expr< $wrap_tactic_expr loc$ ($uid: "Tactic_type"$ . $uid: "TacticType"$ . $uid: arglist_name$ $name$ ) $expr$ >>
      else
         <:expr< $wrap_tactic_expr loc$ ($uid: "Tactic_type"$ . $uid: "TacticType"$ . $uid: arglist_name$ ( $list:name :: vars$ )) $expr$ >>

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
      <:expr< $uid:"Tactic_type"$ . $uid:"TacticType"$ . $uid:s$ $v$ >>

let wrap_general loc name wrap vars expr =
      <:expr< $wrap_tactic_expr loc$ ($uid: "Tactic_type"$ . $uid: "TacticType"$ . $uid: "GeneralArgList"$
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
      <:ctyp< $lid: "tactic"$ >> ->
         wrap_expr loc name (List.rev wrap) expr
    | <:ctyp< $t1$ -> $t2$ >> ->
         collect_fun wrap t1 t2
    | _ ->
         expr
   and collect_fun wrap t1 t2 =
      match t1 with
         <:ctyp< $lid: "bool"$ >> ->
            collect (BoolArg :: wrap) t2
       | <:ctyp< $lid: "int"$ >> ->
            collect (IntArg :: wrap) t2
       | <:ctyp< $lid: "string"$ >> ->
            collect (StringArg :: wrap) t2
       | <:ctyp< $lid: "term"$ >> ->
            collect (TermArg :: wrap) t2
       | <:ctyp< $lid: "list"$ $lid: "term"$ >> ->
            collect (TermListArg :: wrap) t2
       | _ ->
            expr
   in
      collect [] ctyp

(*
 * This function creates str_items for the toploop.
 *)
let add_toploop_item loc name ctyp =
   let rec collect index expr = function
      <:ctyp< $lid: "unit"$ >> ->
         mptop "UnitExpr" expr
    | <:ctyp< $lid: "bool"$ >> ->
         mptop "BoolExpr" expr
    | <:ctyp< $lid: "int"$ >> ->
         mptop "IntExpr" expr
    | <:ctyp< $lid: "string"$ >> ->
         mptop "StringExpr" expr
    | <:ctyp< $lid: "term"$ >> ->
         mptop "TermExpr" expr
    | <:ctyp< $lid: "tactic"$ >> ->
         mptop "TacticExpr" expr
    | <:ctyp< $lid: "conv"$ >> ->
         mptop "ConvExpr" expr
    | <:ctyp< $t1$ -> $t2$ >> ->
         collect_fun index expr t1 t2
    | _ ->
         raise_toploop_exn loc
   and collect_fun index expr t1 t2 =
      match t1 with
         <:ctyp< $lid: "unit"$ >> ->
            mpfun index "UnitFunExpr" expr t1
       | <:ctyp< $lid: "bool"$ >> ->
            mpfun index "BoolFunExpr" expr t2
       | <:ctyp< $lid: "int"$ >> ->
            mpfun index "IntFunExpr" expr t2
       | <:ctyp< $lid: "string"$ >> ->
            mpfun index "StringFunExpr" expr t2
       | <:ctyp< $lid: "term"$ >> ->
            mpfun index "TermFunExpr" expr t2
       | <:ctyp< $lid: "tactic"$ >> ->
            mpfun index "TacticFunExpr" expr t2
       | <:ctyp< $lid: "conv"$ >> ->
            mpfun index "ConvFunExpr" expr t2
       | <:ctyp< $lid: "address"$ >>
       | <:ctyp< $lid: "list"$ $lid: "int"$ >> ->
            mpfun index "AddrFunExpr" expr t2
       | <:ctyp< $lid: "list"$ $lid: "string"$ >> ->
            mpfun index "StringListFunExpr" expr t2
       | <:ctyp< $lid: "list"$ $lid: "term"$ >> ->
            mpfun index "TermListFunExpr" expr t2
       | <:ctyp< $lid: "list"$ $lid: "tactic"$ >> ->
            mpfun index "TacticListFunExpr" expr t2
       | <:ctyp< $lid: "list"$ $lid: "conv"$ >> ->
            mpfun index "ConvListFunExpr" expr t2
       | <:ctyp< $lid: "int"$ -> $lid: "tactic"$ >> ->
            mpfun index "IntTacticFunExpr" expr t2
       | _ ->
            raise_toploop_exn loc
   and mptop name expr =
      <:expr< $uid: "Mptop"$ . $uid: name$ $expr$ >>
   and mpfun index name expr t2 =
      let v = sprintf "v%d" index in
      let patt = <:patt< $lid: v$ >> in
      let expr = collect (index + 1) <:expr< $expr$ $lid: v$ >> t2 in
         <:expr< $uid: "Mptop"$ . $uid: name$ (fun [ $list: [ patt, None, expr ]$ ]) >>
   in
   let expr = collect 0 <:expr< $lid: name$ >> ctyp in
   let patt = <:patt< $lid:"toploop_resource"$ >> in
   let expr = <:expr< $uid: "Mp_resource"$ . $lid: "improve"$
                      $lid: "toploop_resource"$ ($str: name$, $expr$)
              >>
   in
      patt, expr

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
let declare_axiom loc { axiom_name = name } =
   let ctyp = params_ctyp loc (tactic_ctyp loc) [] in
      [<:sig_item< value $name$ : $ctyp$ >>]
      (* <:sig_item< value $refiner_name name$ : $refiner_ctyp loc$ >>] *)

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
let declare_resource loc { resource_name = name;
                           resource_extract_type = extract_type;
                           resource_improve_type = improve_type;
                           resource_data_type = data_type;
                           resource_arg_type = arg_type
    } =
   let rsrc_type = <:ctyp< $resource_rsrc_ctyp loc$ $improve_type$ $extract_type$ $data_type$ $arg_type$ >> in
      [<:sig_item< type $list:[name, [], rsrc_type, []]$ >>]

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
   let _ =
      match item with
         <:sig_item< value $s$ : $t$ >> ->
            (* Check that the type is understood *)
            add_toploop_item (MLast.loc_of_ctyp t) s t
       | _ ->
            Stdpp.raise_with_loc loc (RefineError ("declare_toploop_item", StringError "illegal topval"))
   in
      declare_summary_item loc item

(*
 * Magic block is a block of items.
 *)
let declare_magic_block loc { magic_code = items } =
   items

(*
 * Collect the inherited resources.
 *)
let interf_resources resources loc =
   let rec loop names = function
      (mname, { resource_name = name;
                resource_extract_type = extract_type;
                resource_improve_type = improve_type;
                resource_data_type = data_type;
                resource_arg_type = arg_type
       } as rsrc)::t ->
         if !debug_resource then
            if mname = [] then
               eprintf "Mp_resource: %s%t" name eflush
            else
               eprintf "Mp_resource: %s/%s%t" (string_of_path mname) name eflush;
         if not (List.mem name names) then
            let ctyp =
               if mname = [] then
                  (<:ctyp< $lid: name$ >>)
               else
                  let ctyp = parent_path_ctyp loc mname in
                     (<:ctyp< $ctyp$ . $lid: name$ >>)
            in
               (<:sig_item< value $ext_resource_name name$ : $ctyp$ >>) :: (loop (name :: names) t)
         else
            loop names t

    | [] ->
         []
   in
      loop [] resources

(*
 * Trailer declares a new refiner.
 *)
let interf_postlog info loc =
   let refiner_decl = (<:sig_item< value $refiner_id$ : $refiner_ctyp loc$ >>) in
   let dformer_decl = (<:sig_item< value $dformer_id$ : $dformer_ctyp loc$ >>) in
   let resources = interf_resources info loc in
      refiner_decl :: dformer_decl :: resources

(*
 * Extract a signature item.
 *)
let extract_sig_item (item, loc) =
   match item with
      Rewrite ({ rw_name = name } as rw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: rewrite: %s%t" name eflush;
         declare_rewrite loc rw
    | CondRewrite ({ crw_name = name } as crw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: cond rewrite: %s%t" name eflush;
         declare_cond_rewrite loc crw
    | Axiom ({ axiom_name = name } as ax) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: axiom: %s%t" name eflush;
         declare_axiom loc ax
    | Rule ({ rule_name = name } as rule) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: rule: %s%t" name eflush;
         declare_rule loc rule
    | Prec name ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: prec: %s%t" name eflush;
         declare_prec loc name
    | Resource ({ resource_name = name } as rsrc) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: resource: %s%t" name eflush;
         declare_resource loc rsrc
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
   type t =
      { mutable imp_resources : MLast.ctyp resource_info list;
        imp_sig_info : (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info;
        imp_toploop : (string * (MLast.ctyp * loc)) list;
        imp_arg : Convert.t
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

    let checkpoint_resources proc loc rule_name =
      let label_resource resource =
         let { resource_name = name } = resource in
         let name_expr = (<:expr< $lid:name$ >>) in
         let name_patt = (<:patt< $lid:name$ >>) in
         let expr = <:expr< $uid:"Mp_resource"$ . $lid:"label"$ $name_expr$ $lid:rule_name_id$>> in
            name_patt, expr
      in
      let { imp_resources = resources } = proc in
         if resources = [] then
            []
         else
            let rule_name_id_patt = <:patt< $lid:rule_name_id$ >> in
            let rule_name_expr = <:expr< $str:rule_name$ >> in
               [<:str_item< value $rec:false$ $list: [rule_name_id_patt, rule_name_expr]$ >>;
                <:str_item< value $rec:false$ $list: List.map label_resource resources$ >>]

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
   let define_ml_program proc loc bvars args tname redex contracta code =
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
               <:expr< $compile_contractum_expr loc$ $lid:redex_id$ $term_expr$ >>
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
   let define_rewrite code proc loc
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
       let name_let =
          <:str_item< value $rec:false$ $list:[ name_patt, rw_body_expr ]$ >>
       in
          (checkpoint_resources proc loc name) @ [name_rewrite_let; name_let; refiner_let loc; toploop_rewrite loc name]

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
   let define_cond_rewrite code proc loc
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
       let name_rewrite_let =
          <:str_item< value $rec:false$ $list:[ rw_patt, wrap_exn loc name body ]$ >>
       in
       let name_let =
          <:str_item< value $rec:false$ $list:[ name_patt, rw_fun_expr ]$ >>
       in
          (checkpoint_resources proc loc name) @ [name_rewrite_let; name_let; refiner_let loc (* ; refiner_let_name loc name *)]

   let () = ()

   let prim_rewrite proc loc rw =
      define_rewrite (prim_rewrite_expr loc) proc loc rw None

   let prim_cond_rewrite proc loc crw =
      define_cond_rewrite (prim_cond_rewrite_expr loc) proc loc crw None

   (*
    * Justify a rewrite with a tactic.
    *)
   let derived_rewrite proc loc rw expr =
      define_rewrite (derived_rewrite_expr loc) proc loc rw (Some expr)

   let derived_cond_rewrite proc loc crw expr =
      define_cond_rewrite (derived_cond_rewrite_expr loc) proc loc crw (Some expr)

   (*
    * Interactive forms.
    *)
   let interactive_rewrite proc loc rw expr =
      define_rewrite (delayed_rewrite_expr loc) proc loc rw (Some (Convert.to_expr proc.imp_arg rw.rw_name expr))

   let interactive_cond_rewrite proc loc crw expr =
      define_cond_rewrite (delayed_cond_rewrite_expr loc) proc loc crw (Some (Convert.to_expr proc.imp_arg crw.crw_name expr))

   (*
    * Incomplete forms.
    *)
   let incomplete_rewrite proc loc rw =
      define_rewrite (delayed_rewrite_expr loc) proc loc rw (Some (interactive_exn loc "rewrite"))

   let incomplete_cond_rewrite proc loc crw =
      define_cond_rewrite (delayed_cond_rewrite_expr loc) proc loc crw (Some (interactive_exn loc "rewrite"))

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
   let define_ml_rewrite proc loc
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
      let body = define_ml_program proc loc bvars tparams name redex contracta rewrite_let in

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

      let name_rewrite_let =
         <:str_item< value $rec:false$ $list:[ rw_patt, wrap_exn loc name body ]$ >>
      in
      let name_let =
         <:str_item< value $rec:false$ $list:[ name_patt, rw_fun_expr ]$ >>
      in
         (checkpoint_resources proc loc name) @ [name_rewrite_let; name_let; refiner_let loc (* ; refiner_let_name loc name *)]

   (************************************************************************
    * RULES                                                                *
    ************************************************************************)

   (*
    * A primitive rule specifies the extract.
    *)
   let define_axiom code proc loc { axiom_name = name; axiom_stmt = stmt } extract =
      let goal_expr = expr_of_term loc stmt in
      let goals = list_expr loc (function x -> x) [goal_expr] in
      let axiom_value =
         <:expr< $create_axiom_expr loc$ $lid:local_refiner_id$ $str:name$ $goal_expr$ >>
      in
      let axiom_patt = <:patt< $lid:name$ >> in
      let thm =
         <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
                 (* $nil_array loc$ $nil_list loc$ $goals$ *) $extract$
         >>
      in
      let axiom_item = (<:str_item< value $rec:false$ $list:[axiom_patt, wrap_exn loc name axiom_value]$ >>) in
      let thm_item = <:str_item< $exp: wrap_exn loc name thm$ >> in
         (checkpoint_resources proc loc name) @ [axiom_item; thm_item; refiner_let loc (* ; refiner_let_name loc name *)]

   let prim_axiom proc loc ax extract =
      let code = prim_axiom_expr loc in
      let extract_expr = expr_of_term loc extract in
         define_axiom code proc loc ax extract_expr

   let derived_axiom proc loc ax tac =
      let code = derived_axiom_expr loc in
         define_axiom code proc loc ax tac

   let interactive_axiom proc loc ax expr =
      let code = delayed_axiom_expr loc in
         define_axiom code proc loc ax (Convert.to_expr proc.imp_arg ax.axiom_name expr)

   let incomplete_axiom proc loc ax =
      let code = delayed_axiom_expr loc in
         define_axiom code proc loc ax (interactive_exn loc "axiom")

   let () = ()

   (*
    * Define the resources for a rule.
    * The Tactic_type.pre_tactic is passed as an argument,
    * along with the params, so that we can figure out its type.
    *)
   let define_rule_resources proc loc name params cvars_id tvars_id avars_id params_id assums_id resources name_rule_id =
      let define_resource (loc, name', args) =
         let rule_expr = <:expr< $lid:name_rule_id$ >> in
         let name_patt = <:patt< $lid:name'$ >> in
         let arg_expr =
            match args with
               [] ->
                  rule_expr
             | _ ->
                  <:expr< ( $list:rule_expr :: args$ ) >>
         in
         let name_expr =
            <:expr< $resource_improve_arg_expr loc$ $lid:name'$ $str:name$ $lid:cvars_id$ $lid:tvars_id$ $lid:avars_id$ $lid:params_id$ $lid:assums_id$ $arg_expr$ >>
         in
            name_patt, name_expr
      in
      let resources_pe = List.map define_resource resources in
      let names = name_rule_id :: List.map (fun (_, name, _) -> name) resources in
      let name_patt = List.map (fun s -> <:patt< $lid:s$ >>) names in
      let name_expr = List.map (fun s -> <:expr< $lid:s$ >>) names in
         <:patt< ( $list: name_patt$ ) >>, <:expr< let $rec:false$ $list: resources_pe$ in ( $list: name_expr$ ) >>

   (*
    * A rule is an axiom with parameters.
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
   let define_rule code proc loc
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
      let name_value =
         let addr_expr id = <:expr< $lid:id$ >> in
         let cvars_id_expr = <:expr< [| $list:List.map addr_expr cvar_ids$ |] >> in
         let tvars_id_expr = <:expr< [| $list:List.map lid_expr tvar_ids$ |] >> in
         let tparams_ids_expr = list_expr loc lid_expr tparam_ids in
         let body = <:expr< $tactic_of_rule_expr loc$ $lid:name_rule_id$
                            ( $list:[ cvars_id_expr; tvars_id_expr ]$ )
                            $tparams_ids_expr$ $lid:x_id$ >>
         in
            fun_expr loc (all_ids @ [x_id]) body
      in
      let cvars_patt, tvars_patt, avars_patt,
          params_patt, assums_patt, extract_patt,
          rule_patt, name_patt, name_rule_patt, labels_patt, wild_patt =
         lid_patt cvars_id, lid_patt tvars_id, lid_patt avars_id,
         lid_patt params_id, lid_patt assums_id, lid_patt extract_id,
         lid_patt rule_id, lid_patt name, lid_patt name_rule_id, lid_patt labels_id,
         <:patt< _ >>
      in
      let rule_expr = <:expr< $compile_rule_expr loc$ $lid:local_refiner_id$ $lid:labels_id$ $lid:rule_id$ >> in
      let resource_patt, resource_expr =
         define_rule_resources proc loc name params (**)
            cvars_id tvars_id params_id avars_id assums_id
            resources name_rule_id
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
                     $resource_expr$
          >>)
      in
      let rule_def = <:str_item< value $rec:false$ $list:[ resource_patt, wrap_exn loc name rule_expr ]$ >> in
      let tac_def = <:str_item< value $rec:false$ $list:[ name_patt, name_value ]$ >> in
         (checkpoint_resources proc loc name) @ [rule_def; tac_def; refiner_let loc; toploop_rule loc name params]

   let prim_rule proc loc ax extract =
      let code = prim_rule_expr loc in
      let extract_expr = expr_of_term loc extract in
         define_rule code proc loc ax extract_expr

   let derived_rule proc loc ax tac =
      let code = derived_rule_expr loc in
         define_rule code proc loc ax tac

   let interactive_rule proc loc ax expr =
      let code = delayed_rule_expr loc in
         define_rule code proc loc ax (Convert.to_expr proc.imp_arg ax.rule_name expr)

   let incomplete_rule proc loc ax =
      let code = delayed_rule_expr loc in
         define_rule code proc loc ax (interactive_exn loc "rule")

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
   let define_ml_rule proc loc
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
      let wild_expr = <:expr< $lid:"failwith"$ $str:"bad match"$ >> in
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
      let body = define_ml_program proc loc cvars tparams name redex contracta rule_let in

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
         (checkpoint_resources proc loc name) @ [name_rule_let; name_let; refiner_let loc (* ; refiner_let_name loc name *)]

   (*
    * Define a display form expansion.
    *
    * create_dform dformer [modes]
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
      let string_expr s = <:expr< $str:s$ >> in
      let name_expr = <:expr< $str: name$ >> in
      let modes_expr = list_expr loc string_expr modes in
      let options_expr = list_expr loc (dform_option_expr loc) options in
      let expansion_expr = <:expr< $dform_expansion_expr loc$ $expr_of_term loc expansion$ >> in
      let t_expr = expr_of_term loc t in
      let rec_value =
         <:expr< { $list:[ dform_name_expr loc, name_expr;
                           dform_pattern_expr loc, t_expr;
                           dform_options_expr loc, options_expr;
                           dform_print_expr loc, expansion_expr ]$ } >>
      in
      let expr = <:expr< $create_dform_expr loc$ $lid:local_dformer_id$ $modes_expr$ $rec_value$ >> in
      let name = string_of_opname (opname_of_term t) in
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
         <:patt< $rewriter_patt loc$ . $uid:"RewriteTerm"$ $lid:name$ >>
    | RewriteFunType name ->
         <:patt< $rewriter_patt loc$ . $uid:"RewriteFun"$ $lid:name$ >>
    | RewriteContextType name ->
         <:patt< $rewriter_patt loc$ . $uid:"RewriteContext"$ $lid:name$ >>
    | RewriteStringType name ->
         <:patt< $rewriter_patt loc$ . $uid:"RewriteString"$ $lid:name$ >>
    | RewriteIntType name ->
         <:patt< $rewriter_patt loc$ . $uid:"RewriteInt"$ $lid:name$ >>
    | RewriteLevelType name ->
         <:patt< $rewriter_patt loc$ . $uid:"RewriteLevel"$ $lid:name$ >>

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
    *       create_dform name [modes] { dform_pattern = t;
    *                                   dform_options = [options];
    *                                   dform_print = DFormPrinter printer
    *                                 }
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
      let modes_expr = list_expr loc string_expr modes in
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
         <:expr< { $list:[ dform_name_expr loc, name_expr;
                           dform_pattern_expr loc, term_expr;
                           dform_options_expr loc, options_expr;
                           dform_print_expr loc, dprinter ]$ } >>
      in
      let body_expr =
         <:expr< $create_dform_expr loc$ $lid:local_dformer_id$ $modes_expr$ $rec_value$ >>
      in
      let dprinter_rec_patt =
         <:patt< { $list:[ dform_term_patt loc, term_patt;
                           dform_items_patt loc, items_patt;
                           dform_printer_patt loc, printer_patt;
                           dform_buffer_patt loc, buffer_patt ]$ } >>
      in
      let wild_patt = <:patt< _ >> in
      let wild_code = <:expr< $lid:"failwith"$ $str:"bad match"$ >> in
      let code_expr = <:expr< $code$ $lid:term_id$ >> in
      let dprinter_fun_expr =
         <:expr< fun [ $list:[dprinter_rec_patt, None, code_expr; wild_patt, None, wild_code]$ ] >>
      in
      let dprinter_let_expr =
         <:expr< let $rec:false$ $list:[ dprinter_patt, dprinter_fun_expr ]$ in $body_expr$ >>
      in
      let expr = define_ml_program proc loc [] [] name t cons dprinter_let_expr in
         [<:str_item< $exp:expr$ >>]

   let _ = ()

   (*
    * Record a resource.
    *
    * type resource_name
    *)
   let define_resource proc loc r =
      let { resource_name = name;
            resource_extract_type = extract_type;
            resource_improve_type = improve_type;
            resource_data_type = data_type;
            resource_arg_type = arg_type
          } = r
      in
      let rsrc_type = <:ctyp< $resource_rsrc_ctyp loc$ $improve_type$ $extract_type$ $data_type$ $arg_type$ >> in
         proc.imp_resources <- r :: proc.imp_resources;
         [<:str_item< type $list:[name, [], rsrc_type, []]$ >>]

   (*
    * When a parent is included, we need to open all the ancestors,
    * and we need to patch in all the resources.
    *)
   let define_parent proc loc
       { parent_name = path;
         parent_opens = opens;
         parent_resources = nresources
       } =
      let _ =
         if !debug_resource then
            let print_resources out resources =
               let print { resource_name = name } =
                  fprintf out " %s" name
               in
                  List.iter print resources
            in
               eprintf "Filter_prof.define_parent: %s: %a%t" (string_of_path path) print_resources nresources eflush
      in
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
      in
      let print_resource resources resource =
         let { resource_name = name } = resource in
         let name_expr = (<:expr< $lid:name$ >>) in
         let ext_name_expr = (<:expr< $lid:ext_resource_name name$ >>) in
         let name_patt = (<:patt< $lid:name$ >>) in
         let parent_value = (<:expr< $parent_path$ . $ext_name_expr$ >>) in
         if mem_resource resource resources then
            (*
             * let name = name.resource_join name Parent.name
             *)
            let _ =
               if !debug_resource then
                  eprintf "Filter_prog.define_parent: join resource %s.%s%t" (string_of_path path) name eflush
            in
            let rsrc_val = <:expr< $resource_join_expr loc$ $name_expr$ $parent_value$ >> in
               (resources, <:str_item< value $rec:false$ $list:[ name_patt, rsrc_val ]$ >>)
         else
            (*
             * let name = Parent.name
             *)
            let _ =
               if !debug_resource then
                  eprintf "Filter_prog.define_parent: new resource %s.%s%t" (string_of_path path) name eflush
            in
               (resource :: resources, <:str_item< value $rec:false$ $list:[ name_patt, parent_value ]$ >>)
      in
      let { imp_resources = resources } = proc in
      let resources, items = List_util.fold_left print_resource resources nresources in
         proc.imp_resources <- resources;
         joins @ items

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
                  let patt2, expr2 = add_toploop_item loc name ctyp in
                     (patt, expr1), [patt2, expr2]
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
            let pel, resources = wrap_toploop_items proc loc pel in
            let item1 = <:str_item< value $rec:rec_flag$ $list:pel$ >> in
               if resources = [] then
                  [item1]
               else
                  item1 :: List.map (fun (patt, expr) -> <:str_item< value $rec:false$ $list:[patt, expr]$ >>) resources

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
      let dformer_val = <:expr< $lid:"ref"$ $null_mode_base_expr loc$ >> in
      let refiner_patt = <:patt< $lid:local_refiner_id$ >> in
      let dformer_patt = <:patt< $lid:local_dformer_id$ >> in
         [<:str_item< value $rec:false$ $list:[ refiner_patt, refiner_val; dformer_patt, dformer_val ]$ >>]

   (*
    * Collect the resources in this module.
    *)
   let implem_resources resources name =
      let loc = 0, 0 in
      let bind_of_resource { resource_name = name' } =
         let patt = <:patt< $lid: ext_resource_name name'$ >> in
         let expr = <:expr< $uid: "Mp_resource"$ . $lid: "close"$ $lid: name'$ $str:name$ >> in
            patt, expr
      in
      let values = List.map bind_of_resource resources in
         <:str_item< value $rec:false$ $list: values$ >>

   (*
    * Trailing declarations.
    *
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
   let implem_postlog proc loc name =
      let thy_elems =
         [(<:expr< $thy_name_expr loc$ >>, <:expr< $str:name$ >>);
          (<:expr< $thy_refiner_expr loc$ >>, <:expr< $lid:refiner_id$ >>);
          (<:expr< $thy_dformer_expr loc$ >>, <:expr< $lid:dformer_id$ >>)]
      in
      let thy_rec = <:expr< { $list:thy_elems$ } >> in
      let thy = <:expr< $record_theory_expr loc$ $thy_rec$ >> in
      let refiner_patt = <:patt< $lid:refiner_id$ >> in
      let dformer_patt = <:patt< $lid:dformer_id$ >> in
      let dformer_val = <:expr< $lid:local_dformer_id$ . $lid:"val"$ >> in
      let label_expr = <:expr< $label_refiner_expr loc$ $lid:local_refiner_id$ $str:name$ >> in
          [implem_resources proc.imp_resources name;
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
            define_ml_rewrite proc loc mlrw rewrite_expr
       | MLRewrite ({ mlterm_name = name; mlterm_def = None }) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: ML rewrite (unimplemented): %s%t" name eflush;
            raise (Failure "Filter_prog.extract_str_item: ML rewrite is not defined")
       | Axiom ({ axiom_name = name; axiom_proof = Primitive t } as ax) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: prim axiom: %s%t" name eflush;
            prim_axiom proc loc ax t
       | Axiom ({ axiom_name = name; axiom_proof = Derived tac } as ax) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: thm axiom: %s%t" name eflush;
            derived_axiom proc loc ax tac
       | Axiom ({ axiom_name = name; axiom_proof = Incomplete } as ax) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: incomplete axiom: %s%t" name eflush;
            incomplete_axiom proc loc ax
       | Axiom ({ axiom_name = name; axiom_proof = Interactive pf } as ax) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: interactive axiom: %s%t" name eflush;
            interactive_axiom proc loc ax pf
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
            define_ml_rule proc loc mlrule rule_expr
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
       | Resource ({ resource_name = name } as rsrc) ->
            if !debug_filter_prog then
               eprintf "Filter_prog.extract_str_item: resource: %s%t" name eflush;
            define_resource proc loc rsrc
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
      let proc = { imp_resources = [];
                   imp_sig_info = sig_info;
                   imp_toploop = implem_toploop sig_info;
                   imp_arg = arg
                 }
      in
      let prolog = implem_prolog proc (0, 0) name in
      let items = List_util.flat_map (extract_str_item proc) (info_items info) in
      let postlog = implem_postlog proc (0, 0) name in
         List.map (fun item -> item, (0, 0)) (prolog @ items @ postlog)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
