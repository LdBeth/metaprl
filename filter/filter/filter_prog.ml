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
open Refiner.Refiner.TermMan
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
open Filter_summary_type
open Filter_summary_util
open Filter_summary
open Proof_convert

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
 * For implementations, we maintain a state, which contains
 *    1. a list of the resources that have been defined
 *)
type t = {
   imp_sig_info : (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info;
   mutable imp_toploop : (string * MLast.ctyp) list;
   imp_arg : Convert.t;
   imp_name : string;
   mutable imp_resources : (string * MLast.ctyp) list;
   imp_all_resources : (module_path * string * MLast.ctyp resource_sig) list;
   mutable imp_terms : term list;
   mutable imp_num_terms : int;
   mutable imp_meta_terms : meta_term list;
   mutable imp_num_meta_terms : int;
}

(*
 * These are the argument types that can be used as annotations.
 *)
type wrap_arg =
   BoolArg
 | IntArg
 | StringArg
 | TermArg
 | TermListArg

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

(*
 * Convert between expressions and terms.
 *)
let global_term_var = "_$globterms"
let global_meta_term_var = "_$globmterms"

let expr_of_term proc loc t =
   proc.imp_terms <- t::proc.imp_terms;
   let num = proc.imp_num_terms in
      proc.imp_num_terms <- num + 1;
      <:expr< $lid:global_term_var$.($int:(string_of_int num)$) >>

let expr_of_meta_term proc loc t =
   proc.imp_meta_terms <- t::proc.imp_meta_terms;
   let num = proc.imp_num_meta_terms in
      proc.imp_num_meta_terms <- num + 1;
      <:expr< $lid:global_meta_term_var$.($int:(string_of_int num)$) >>

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

(*
 * Axiom.
 *)
let refiner_expr loc =
   <:expr< Refiner.Refiner.Refine >>

let refiner_ctyp loc =
   <:ctyp< Refiner.Refiner.Refine >>

let rewriter_expr loc =
   <:expr< Refiner.Refiner.Rewrite >>

let rewriter_patt loc =
   <:patt< Refiner.Refiner.Rewrite >>

let tactic_type_expr loc =
   <:expr< Tactic_type.Tactic >>

let rewrite_type_expr loc =
   <:expr< Tactic_type.Rewrite >>

let rewrite_type_ctyp loc =
   <:ctyp< Tactic_type.Rewrite >>

let dest_msequent_expr loc =
   <:expr< $refiner_expr loc$ . dest_msequent >>

(*
 * Rule.
 *)
let prim_rule_expr loc =
   <:expr< $refiner_expr loc$ . prim_rule >>

let derived_rule_expr loc =
   <:expr< $refiner_expr loc$ . derived_rule >>

let delayed_rule_expr loc =
   <:expr< $refiner_expr loc$ . delayed_rule >>

let create_ml_rule_expr loc =
   <:expr< $refiner_expr loc$ . create_ml_rule >>

let tactic_of_rule_expr loc =
   <:expr< $tactic_type_expr loc$ . tactic_of_rule >>

let tactic_ctyp loc =
   <:ctyp< Tactic_type.Tactic.tactic >>

(*
 * Rewrite.
 *)
let rewrite_ctyp loc =
   <:ctyp< $rewrite_type_ctyp loc$ . conv >>

let prim_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . prim_rewrite >>

let def_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . definitional_rewrite >>

let derived_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . derived_rewrite >>

let delayed_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . delayed_rewrite >>

let rewrite_of_pre_rewrite_expr loc =
   <:expr< $rewrite_type_expr loc$ . rewrite_of_pre_rewrite >>

(*
 * Conditional rewrite.
 *)
let cond_rewrite_ctyp loc =
   <:ctyp< $rewrite_type_ctyp loc$ . conv >>

let prim_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . prim_cond_rewrite >>

let derived_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . derived_cond_rewrite >>

let delayed_cond_rewrite_expr loc =
   <:expr< $refiner_expr loc$ . delayed_cond_rewrite >>

let apply_redex_expr loc =
   <:expr< $rewriter_expr loc$ . apply_redex >>

let strict_expr loc =
   <:expr< $rewriter_expr loc$ . Strict >>

let relaxed_expr loc =
   <:expr< $rewriter_expr loc$ . Relaxed >>

(*
 * Other expressions.
 *)
let refiner_ctyp loc =
   <:ctyp< $refiner_ctyp loc$ . refiner >>

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

let refiner_id = "refiner"
let dformer_id = "dformer"

let local_refiner_id = "_$global_refiner"
let local_dformer_id = "_$global_dformer"
let stack_id = "_$rewrite_stack"

let add_lt_expr loc =
   <:expr< Precedence.add_lt >>

let add_eq_expr loc =
   <:expr< Precedence.add_eq >>

(*
 * Each rule gets a refiner associated with it, with the following name.
 *)
let refiner_let loc =
   <:str_item< value $lid: refiner_id$ = $refiner_expr loc$ .refiner_of_build $lid: local_refiner_id$ >>

(*
 * Variable names.
 *)
let exn_id              = "_$exn"
let term_id             = "_$term"
let args_id             = "_$args"
let redex_id            = "_$redex"
let contractum_id       = "_$contractum"
let params_id           = "_$params"
let subgoals_id         = "_$subgoals"
let term_id             = "_$term"
let cvars_id            = "_$cvars"
let avars_id            = "_$avars"
let bnames_id           = "_$bnames"
let info_id             = "_$info"
let rewrite_id          = "_$rewrite"
let extract_id          = "_$extract"
let stack_id            = "_$stack"
let names_id            = "_$names"
let goal_id             = "_$goal"
let assums_id           = "_$assums"
let rule_id             = "_$rule"
let addrs_id            = "_$addrs"
let msequent_goal_id    = "_$mseq_goal"
let msequent_hyps_id    = "_$mseq_hyps"
let dprinter_id         = "_$dprinter"
let rule_name_id        = "_$rule_name"

let expr_of_label loc = function
   [] ->
      <:expr< None >>
 | [h] ->
      <:expr< Some $str: h$ >>
 | _ ->
      Stdpp.raise_with_loc loc (Invalid_argument "Filter_prog.expr_of_label: multi-component labels not supported")

(*
 * Print a message on loading, and catch errors.
 *    Mp_debug.show_loading "Loading name%t";
 *    try e with
 *       exn ->
 *          Refine_exn.print_exn name exn
 *)
let wrap_exn proc loc name e =
   let name = (String.capitalize proc.imp_name) ^ "." ^ name in
   <:expr< 
      do { 
         (* Print a message before the execution *)
         Mp_debug.show_loading $str: "Loading " ^ name ^ "%t"$;
         (* Wrap the body to catch exceptions *)
         try $e$ with
         $lid: exn_id$ ->
            Refine_exn.print_exn $lid: local_dformer_id$.val Pervasives.stderr $str: name$ $lid: exn_id$
      }
   >>

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
                  <:ctyp< int >>
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
   DFormParens -> <:expr< Dform.DFormParens >>
 | DFormPrec p -> <:expr< Dform.DFormPrec $lid:p$ >>
 | DFormInheritPrec -> <:expr< Dform.DFormInheritPrec >>
 | DFormInternal -> <:expr< Dform.DFormInternal >>

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

let raise_toploop_exn loc =
   Stdpp.raise_with_loc loc (RefineError ("topval", StringError
                                          "The types allowed in toploop expressions are limited.\n\
Your type is not understood. See the support/shell/shell_sig.mlz file\n\
for the list of allowed types."))

(*
 * This function checks that the type is acceptable for the toploop
 * and creates a toploop expression
 *)
let toploop_item_expr loc name ctyp =
   let str_lid s =
      match s with
         "unit" | "bool" |  "int" | "string" | "term" | "tactic" | "conv" | "address" ->
            String.capitalize s
       | _ ->
            raise_toploop_exn loc
   in let rec collect index expr = function
      <:ctyp< $lid: typ$ >> ->
         let name = str_lid typ in
            <:expr< Shell_sig. $uid: name ^ "Expr"$ $expr$ >>, <:expr< Shell_sig. $uid: name ^ "Type"$ >>
    | <:ctyp< $t1$ -> $t2$ >> ->
         let v = sprintf "v%d" index in
         let patt = <:patt< $lid: v$ >> in
         let expr,texpr = collect (succ index) <:expr< $expr$ $lid: v$ >> t2 in
         let expr = <:expr< fun [ $list: [patt, None, expr]$ ]>> in
            begin match t1 with
               <:ctyp< $lid: typ$ >> ->
                  let name = str_lid typ in
                     <:expr< Shell_sig. $uid: name ^ "FunExpr"$ $expr$ >>,
                     <:expr< Shell_sig.FunType Shell_sig.$uid: name ^ "Type"$ $texpr$ >>
             | <:ctyp< list $lid: typ$ >> ->
                  let name = str_lid typ in
                     <:expr< Shell_sig. $uid: name ^ "ListFunExpr"$ $expr$ >>,
                     <:expr< Shell_sig.FunType (Shell_sig.ListType Shell_sig.$uid: name ^ "Type"$) $texpr$ >>
             | <:ctyp< int -> tactic >> ->
                  <:expr< Shell_sig.IntTacticFunExpr $expr$ >>,
                  <:expr< Shell_sig.FunType (Shell_sig.FunType Shell_sig.IntType Shell_sig.TacticType) $texpr$ >>
             | _ ->
                  raise_toploop_exn loc
            end
    | _ ->
         raise_toploop_exn loc
   in
      collect 0 <:expr< $lid: name$ >> ctyp

(************************************************************************
 * SIGNATURES                                                           *
 ************************************************************************)

(*
 * Rewrites.
 *)
let declare_rewrite loc rw =
   [<:sig_item< value $rw.rw_name$ : $rewrite_ctyp loc$ >>]

let declare_input_form = declare_rewrite

let declare_definition loc def =
   [<:sig_item< value $def.opdef_name$ : $rewrite_ctyp loc$ >>]

let declare_cond_rewrite loc { crw_name = name; crw_params = params } =
   [<:sig_item< value $name$ : $params_ctyp loc (cond_rewrite_ctyp loc) params$ >>]

let declare_ml_rewrite loc { mlterm_name = name; mlterm_params = params } =
   [<:sig_item< value $name$ : $params_ctyp loc (cond_rewrite_ctyp loc) params$ >>]

(*
 * Rules.
 *)
let declare_rule loc { rule_name = name; rule_params = params } =
   [<:sig_item< value $name$ : $params_ctyp loc (tactic_ctyp loc) params$ >>]

let declare_ml_axiom loc { mlterm_name = name; mlterm_params = params } =
   [<:sig_item< value $name$ : $params_ctyp loc (tactic_ctyp loc) params$ >>]

(*
 * Precedence.
 *)
let declare_prec loc name =
   [<:sig_item< value $name$ : Precedence.precedence >>]

(*
 * Resource.
 *)
let declare_resource loc name {
   resource_input = input;
   resource_output = output
} =
   [<:sig_item< type $input_type name$ = $input$ >>;
    <:sig_item< value $get_resource_name name$ : Mp_resource.global_resource -> $output$ >>]

(*
 * When a parent is declared, we need to open all the ancestors.
 *)
let declare_parent loc _ =
   []

(*
 * Standard summary item.
 *)
let declare_summary_item loc item =
   if item.item_bindings <> [] then
      Stdpp.raise_with_loc loc (Invalid_argument "Signature items can not have bindings in them!");
   [item.item_item]

let declare_toploop_item loc item =
   begin match item with
         <:sig_item< value $s$ : $t$ >> ->
            (* Check that the type is understood *)
            ignore(toploop_item_expr (MLast.loc_of_ctyp t) s t)
       | _ ->
            Stdpp.raise_with_loc loc (RefineError ("declare_toploop_item", StringError "illegal topval"))
   end;
   [item]

(*
 * Magic block is a block of items.
 *)
let declare_magic_block loc { magic_code = items } =
   items

(*
 * Trailer declares a new refiner.
 *)
let interf_postlog info loc =
   [ <:sig_item< value $refiner_id$ : $refiner_ctyp loc$ >>;
     <:sig_item< value $dformer_id$ : $dformer_ctyp loc$ >> ]

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
    | Opname _ | DForm _ | PrecRel _ | Id _ | Comment _ | Infix _ ->
         []
    | MLRewrite item ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: mlrewrite%t" eflush;
         declare_ml_rewrite loc item
    | MLAxiom item ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: mlaxiom%t" eflush;
         declare_ml_axiom loc item
    | Module (name, _) ->
         Stdpp.raise_with_loc loc (Failure "Filter_sig.extract_sig_item: nested modules are not implemented")
    | Definition def ->
         declare_definition loc def
         
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
 * UTILITIES                                                            *
 ************************************************************************)

 (*
  * Eta-reduction
  *)
 let beta_reduce_var var f =
    let loc = MLast.loc_of_expr f in
    match f with
       <:expr< fun $lid:v$ -> $e$ >> -> 
          v, e
     | _ ->
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

let impr_resource proc loc name expr =
   <:expr< Mp_resource.improve $str:name$ (Obj.repr ( $expr$ : $res_type proc loc name$ )) >>

let impr_resource_list proc loc name expr =
   <:expr< Mp_resource.improve_list $str:name$ (Obj.magic ( $expr$ : (list $res_type proc loc name$) )) >>

let rec mk_string_list_expr loc = function
   [] ->
      <:expr< [] >>
 | hd::tl ->
      <:expr< $lid:"::"$ $str:hd$ $mk_string_list_expr loc tl$ >>

let binding_let proc loc (v, bnd) =
   <:patt< $lid:v$ >>,
   match bnd with
      BindTerm t ->
         expr_of_term proc loc t
    | BindOpname op ->
         <:expr< Opname.make_opname $mk_string_list_expr loc (Opname.dest_opname op)$ >>

let bindings_let proc loc bnd_expr expr =
   if bnd_expr.item_bindings = [] then expr
   else <:expr< let $list:List.map (binding_let proc loc) (List.rev bnd_expr.item_bindings)$ in $expr$ >>

let expr_of_bnd_expr proc loc expr =
   bindings_let proc loc expr expr.item_item

(************************************************************************
 * TOP LOOP                                                             *
 ************************************************************************)

let impr_toploop proc loc name (expr,texpr) =
   let expr = <:expr< ($str:proc.imp_name$, $str: name$, $expr$, $texpr$) >> in
      <:str_item< ($impr_resource proc loc "toploop" expr$) >>

(*
 * This is a little bogus, but we add rewrites automatically to the
 * toploop resource.
 *)
let rec loop_params loc i body base_expr = function
   h :: t ->
      let v = "v" ^ string_of_int i in
      let expr, texpr = loop_params loc (succ i) <:expr< $body$ $lid: v$ >> base_expr t in
      let expr = <:expr< fun $lid:v$ -> $expr$ >> in
      let expr =
         match h with
            ContextParam _ ->
               <:expr< Shell_sig.IntFunExpr $expr$ >>, <:expr< Shell_sig.FunType Shell_sig.IntType $texpr$ >>
          | TermParam _ ->
               <:expr< Shell_sig.TermFunExpr $expr$ >>, <:expr< Shell_sig.FunType Shell_sig.TermType $texpr$ >>
      in
         expr
 | [] ->
      base_expr body

let toploop_rewrite proc loc name params =
   let base body = <:expr< Shell_sig.ConvExpr $body$ >>, <:expr< Shell_sig.ConvType >> in
      impr_toploop proc loc name (loop_params loc 0 <:expr< $lid: name$ >> base params)

let toploop_rule proc loc name params =
   let base body = <:expr< Shell_sig.TacticExpr $body$ >>, <:expr< Shell_sig.TacticType >> in
      impr_toploop proc loc name (loop_params loc 0 <:expr< $lid: name$ >> base params)

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
      fun_expr loc names expr

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

(************************************************************************
 * ML RULE                                                              *
 ************************************************************************)

(*
 * An mlterm is a side condition that is checked in ML.
 * The term expands to the code production.
 *
 *)
let define_ml_program proc loc strict_expr args tname redex bnd_expr code =
   <:expr<
      let $lid:term_id$ = $expr_of_term proc loc redex$ in
      let $lid:args_id$ = [ $lid:term_id$ :: $list_expr loc (expr_of_term proc loc) args$ ] in
      (* XXX BUG? We should actually pass the context args array, not just [||], but that 
         is only needed for ml_rules and that code is dead anyway... *)
      let $lid:redex_id$ = Refiner.Refiner.Rewrite.compile_redices $strict_expr loc$ [||] $lid:args_id$ in
         $bindings_let proc loc bnd_expr code$
   >>

(************************************************************************
 * REWRITES                                                             *
 ************************************************************************)

let extract_expr loc modname name =
   <:expr< Shell.extract [ $str:modname$; $str:name$ ] >>

(*
 * Define the resources for a rewrite.
 * The Tactic_type.pre_tactic is passed as an argument,
 * along with the params, so that we can figure out its type.
 *)
let define_rewrite_resources proc loc name redex contractum assums params resources name_id_expr =
   if resources.item_item = [] then <:expr< () >> else
   let define_resource (loc, name', args) =
      let input = res_type proc loc name' in
      let arg_expr =
         match args with
            [] ->
               name_id_expr
          | _ ->
               <:expr< ( $list:name_id_expr :: args$ ) >>
      in
      let process_name = "process_" ^ name' ^ "_resource_rw_annotation" in
      let anno_name = "_$" ^ name' ^ "_resource_annotation" in
         impr_resource proc loc name' <:expr< 
            ($lid:process_name$ : Mp_resource.rw_annotation_processor '$anno_name$ $input$)
               $str:name$ $redex$ $contractum$ $assums$ $params$ $arg_expr$
         >>
   in bindings_let proc loc resources <:expr< do { $list:List.map define_resource resources.item_item$ } >>

(*
 * An input form is a rewrite, but we don't add it to the
 * refiner (input forms have no formal justification).
 *)
let define_input_form want_checkpoint proc loc iform =
   if iform.rw_resources.item_item <> [] then
      Stdpp.raise_with_loc loc (Invalid_argument "Resource annotations on input forms are not supported yet");
   let name = iform.rw_name in
   let create_input_form = <:expr<
      let $lid:contractum_id$ = $expr_of_term proc loc iform.rw_contractum$ in
         $refiner_expr loc$.create_input_form $lid:local_refiner_id$ $str:name$ (**)
            ($expr_of_term proc loc iform.rw_redex$) $lid:contractum_id$
    >> in
       checkpoint_resources want_checkpoint loc name [
          <:str_item< value $lid:name$ = $rewrite_of_pre_rewrite_expr loc$ ($wrap_exn proc loc name create_input_form$) [] >>;
          toploop_rewrite proc loc name []
       ]

(*
 * A primitive rewrite is assumed true by fiat.
 *)
let define_rewrite want_checkpoint code proc loc rw expr =
   let name = rw.rw_name in
   let rw_id  = "_$" ^ name ^ "_rewrite" in
   let rw_id_expr = <:expr< $lid:rw_id$ >> in
   let redex = <:expr< $lid:redex_id$ >> in
   let contractum = <:expr< $lid:contractum_id$ >> in
   let nil = <:expr< [] >> in
   let prim_expr =
      match expr with
         Some expr ->
            <:expr< $code$ $lid:local_refiner_id$ $str:name$ $redex$ $contractum$ $expr$ >>
       | None ->
            <:expr< $code$ $lid:local_refiner_id$ $str:name$ $redex$ $contractum$ >>
   in
   let create_rw = <:expr<
      let $lid:redex_id$ = $expr_of_term proc loc rw.rw_redex$ in
      let $lid:contractum_id$ = $expr_of_term proc loc rw.rw_contractum$ in
      let $lid:rw_id$ = 
         $refiner_expr loc$.create_rewrite $lid:local_refiner_id$ $str:name$ $redex$ $contractum$ in
      let _ = do { 
         $prim_expr$; 
         $define_rewrite_resources proc loc name redex contractum nil nil rw.rw_resources rw_id_expr$
      } in
         $rw_id_expr$
    >> in
       checkpoint_resources want_checkpoint loc name [
          <:str_item< value $lid:name$ = $rewrite_of_pre_rewrite_expr loc$ ($wrap_exn proc loc name create_rw$) $nil$ >>;
          refiner_let loc;
          toploop_rewrite proc loc name []
       ]

(*
 * Conditional rewrite is a little more complicated.
 *)
let define_cond_rewrite want_checkpoint code proc loc crw expr =
   let name          = crw.crw_name in
   let rw_id         = "_$" ^ name ^ "_rewrite" in
   let rw_id_expr    = <:expr< $lid:rw_id$ >> in
   let rw_id_patt    = <:patt< $lid:rw_id$ >> in
   let lid_expr s    = <:expr< $lid:s$ >> in
   let params_expr   = <:expr< $lid:params_id$ >> in
   let subgoals_expr = <:expr< $lid:subgoals_id$ >> in
   let cvars, tparams = split_params crw.crw_params in
   if cvars <> [] then
      Stdpp.raise_with_loc loc (Invalid_argument ("rewrites can not have context variables"));
   let all_ids, cvar_ids, tparam_ids = name_params crw.crw_params in
   let redex = <:expr< $lid:redex_id$ >> in
   let contractum = <:expr< $lid:contractum_id$ >> in
   let prim_expr =
      match expr with
         Some expr ->
            <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
               $params_expr$ $subgoals_expr$ $redex$ $contractum$ $expr$ >>
       | None ->
            <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
               $params_expr$ $subgoals_expr$ $redex$ $contractum$ >>
   in
   let rw_fun_expr =
      fun_expr loc all_ids <:expr<
         $rewrite_of_pre_rewrite_expr loc$ $rw_id_expr$ $list_expr loc lid_expr tparam_ids$ >>
   in
   let create_expr = <:expr<
      let $lid:params_id$ = $list_expr loc (expr_of_term proc loc) tparams$ in
      let $lid:subgoals_id$ = $list_expr loc (expr_of_term proc loc) crw.crw_args$ in
      let $lid:redex_id$ = $expr_of_term proc loc crw.crw_redex$ in
      let $lid:contractum_id$ = $expr_of_term proc loc crw.crw_contractum$ in
      let $rw_id_patt$ =
         $refiner_expr loc$.create_cond_rewrite $lid:local_refiner_id$ $str:name$ (**)
            $params_expr$ $subgoals_expr$ $redex$ $contractum$
      in let _ = do {
         $prim_expr$;
         $define_rewrite_resources proc loc name redex contractum subgoals_expr params_expr crw.crw_resources rw_id_expr$
      } in
         $rw_id_expr$
    >> in
       checkpoint_resources want_checkpoint loc name [
          <:str_item< value $rw_id_patt$ = $wrap_exn proc loc name create_expr $ >>;
          <:str_item< value $lid:name$ = $rw_fun_expr$ >>;
          refiner_let loc;
          toploop_rewrite proc loc name crw.crw_params
       ]

let prim_rewrite proc loc rw =
   define_rewrite false (prim_rewrite_expr loc) proc loc rw None

let definition proc loc def =
   let rw = {
      rw_name = def.opdef_name;
      rw_redex = def.opdef_term;
      rw_contractum = def.opdef_definition;
      rw_proof = xnil_term;
      rw_resources = def.opdef_resources;
   } in
      define_rewrite false (def_rewrite_expr loc) proc loc rw None

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
let interactive_rewrite proc loc rw =
   define_rewrite true (delayed_rewrite_expr loc) proc loc rw (Some (extract_expr loc proc.imp_name rw.rw_name))

let interactive_cond_rewrite proc loc crw =
   define_cond_rewrite true (delayed_cond_rewrite_expr loc) proc loc crw (Some (extract_expr loc proc.imp_name crw.crw_name))

(*
 * An ML rewrite performs the same action as a conditional rewrite,
 * but the ML code computes the rewrite.
 *)
let define_ml_rewrite want_checkpoint proc loc mlrw rewrite_expr =
   if mlrw.mlterm_resources.item_item <> [] then
      Stdpp.raise_with_loc loc (Invalid_argument "Resource annotations on ML rewrites are not supported yet");
   let name = mlrw.mlterm_name in
   let rw_id = "_$" ^ name ^ "_rewrite" in
   let name_patt = <:patt< $lid:name$ >> in
   let lid_expr s = <:expr< $lid:s$ >> in

   let params = mlrw.mlterm_params in
   let cvars, tparams = split_params params in
   if cvars <> [] then
      Stdpp.raise_with_loc loc (Invalid_argument ("rewrites can not have context variables"));
   let all_ids, cvar_ids, tparam_ids = name_params params in
   let params_expr = list_expr loc (expr_of_term proc loc) tparams in
   let redex_expr = expr_of_term proc loc mlrw.mlterm_term in
   let simple_flag = params = [] in
   let create_ml_rewrite_expr =
      if simple_flag then
         <:expr< $refiner_expr loc$ . create_ml_rewrite >>
      else
         raise(Invalid_argument("Conditional ML rewrites are not currently supported - the code is there, but needs to be cleaned up"))
         <:expr< $refiner_expr loc$ . create_ml_cond_rewrite >>
   in
   let rewrite_body' = <:expr< $rewrite_expr.item_item$ $lid:goal_id$ >> in
   let rewrite_body =
      if simple_flag then
         rewrite_body'
      else <:expr<
         let ( $lid:goal_id$ , $lid:subgoals_id$ , $lid:extract_id$ ) = $rewrite_body'$ in
            ( $lid:goal_id$ , $lid:subgoals_id$ , $lid:stack_id$ , $lid:extract_id$ )
      >>
   in
   let params_expr =
      if simple_flag then
         <:expr< [] >>
      else
         <:expr< $lid:params_id$ >>
   in
   let rewrite_body = <:expr<
      let $lid:stack_id$ = $apply_redex_expr loc$ $lid:redex_id$ [||] $lid:goal_id$ $params_expr$ in $rewrite_body$
   >> in
   let args_id =
      if simple_flag then
         [goal_id]
      else
         [names_id; bnames_id; params_id; goal_id]
   in
   let rewrite_let = <:expr<
      let $lid:rewrite_id$ = $fun_expr loc args_id rewrite_body$ in
      let $lid:info_id$ = $lid:rewrite_id$ in
         $create_ml_rewrite_expr$ $lid:local_refiner_id$ $str:name$ $lid:info_id$
   >> in
   let body = define_ml_program proc loc strict_expr tparams name mlrw.mlterm_term rewrite_expr rewrite_let in
      checkpoint_resources want_checkpoint loc name [
         <:str_item< value $name_patt$ =
            $rewrite_of_pre_rewrite_expr loc$ ($wrap_exn proc loc name body $) $list_expr loc lid_expr tparam_ids$ >>; 
         refiner_let loc
      ]

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*
 * Define the resources for a rule.
 * The Tactic_type.pre_tactic is passed as an argument,
 * along with the params, so that we can figure out its type.
 *)
let define_rule_resources proc loc name cvars_id avars_id params_id assums_id resources name_rule_expr =
   if resources.item_item = [] then <:expr< () >> else
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
         impr_resource proc loc name' <:expr< 
            ($lid:process_name$ : Mp_resource.annotation_processor '$anno_name$ $input$)
               $str:name$ $lid:cvars_id$ $lid:avars_id$ $lid:params_id$ $lid:assums_id$ $arg_expr$
         >>
   in bindings_let proc loc resources <:expr< do { $list:List.map define_resource resources.item_item$ } >>

let define_rule want_checkpoint code proc loc
    { rule_name = name;
      rule_params = params;
      rule_stmt = stmt;
      rule_resources = resources
    }
    extract =
   (* Check the specifications *)
   let string_expr s = <:expr< $str:s$ >> in
   let lid_expr s = <:expr< $lid:s$ >> in

   (* Expressions *)
   let name_rule_id = "_$" ^ name ^ "_rule" in
   let cvars, tparams = split_params params in
   let all_ids, cvar_ids, tparam_ids = name_params params in
   let labels, avars, mterm = split_mfunction stmt in
   let name_rule_expr = lid_expr name_rule_id in
   let name_value =
      fun_expr loc all_ids <:expr<
         $tactic_of_rule_expr loc$ $name_rule_expr$ (**)
            [| $list:List.map lid_expr cvar_ids$ |] $list_expr loc lid_expr tparam_ids$
      >>
   in
   let rule_expr = <:expr<
      let $lid:cvars_id$ = [| $list: List.map string_expr cvars$ |] in
      let $lid:avars_id$ = $list_expr loc (expr_of_term proc loc) avars$ in
      let $lid:params_id$ = $list_expr loc (expr_of_term proc loc) tparams$ in
      let $lid:assums_id$ = $expr_of_meta_term proc loc mterm$ in
      let $lid:rule_id$ = 
         $refiner_expr loc$.create_rule $lid:local_refiner_id$ $str:name$ $lid:cvars_id$ $lid:params_id$ $lid:assums_id$
      in
      let $lid:name_rule_id$ = 
         $tactic_type_expr loc$.compile_rule $lid:local_refiner_id$ ($list_expr loc (expr_of_label loc) labels$) $lid:rule_id$
      in let _ = do {
         $code$ $lid:local_refiner_id$ $str:name$ $lid:cvars_id$ $lid:params_id$ $lid:avars_id$ $extract$;
         $define_rule_resources proc loc name cvars_id params_id avars_id assums_id resources name_rule_expr$
      }
         in $name_rule_expr$
   >> in
      checkpoint_resources want_checkpoint loc name [
         <:str_item< value $lid:name_rule_id$ = $wrap_exn proc loc name rule_expr$ >>; 
         <:str_item< value $lid:name$ = $name_value$ >>; 
         refiner_let loc; 
         toploop_rule proc loc name params
      ]

let prim_rule proc loc ax extract =
   let code = prim_rule_expr loc in
   let extract_expr = expr_of_term proc loc extract in
      define_rule false code proc loc ax extract_expr

let derived_rule proc loc ax tac =
   let code = derived_rule_expr loc in
      define_rule true code proc loc ax tac

let interactive_rule proc loc ax =
   let code = delayed_rule_expr loc in
      define_rule true code proc loc ax (extract_expr loc proc.imp_name ax.rule_name)

(*
 * An ML rule performs the same action as a normal one,
 * but the ML code computes the subgoals.
 *
 *)
let define_ml_rule want_checkpoint proc loc
    { mlterm_name       = name;
      mlterm_params     = params;
      mlterm_term       = redex;
    } code =
   (* Names *)
   raise(Invalid_argument("ML rules are not currently supported - the code is there, but needs to be cleaned up"));
   let name_rule_id = "_$" ^ name ^ "_rule" in
   let name_patt = <:patt< $lid:name$ >> in

   let string_expr s = <:expr< $str:s$ >> in
   let lid_patt s = <:patt< $lid:s$ >> in
   let lid_patt_ s = <:patt< $lid: "_" ^ s$ >> in
   let lid_expr s = <:expr< $lid:s$ >> in

   let goal_id, rule_expr = beta_reduce_var goal_id code.item_item in

   let cvars, tparams = split_params params in
   let all_ids, cvar_ids, tparam_ids = name_params params in
   let cvars_expr = <:expr< [| $list: List.map string_expr cvars$ |] >> in
   let params_expr = list_expr loc (expr_of_term proc loc) tparams in
   let redex_expr = expr_of_term proc loc redex in

   let create_expr =
      <:expr< $tactic_type_expr loc$.compile_labeled_rule $lid:local_refiner_id$ (**)
               ($create_ml_rule_expr loc$ $lid:local_refiner_id$ $str:name$ $lid:info_id$) >>
   in
   let info_let =
      <:expr< let $lid:info_id$ = $lid:rule_id$ in $create_expr$ >>
   in
   let rule_patt =
      <:patt< [| $list:List.map lid_patt_ cvars$ |] >>
   in
   let wild_patt = <:patt< _ >> in
   let wild_expr = <:expr< failwith "bad match" >> in
   let rule_expr =
      <:expr< match ( $lid:addrs_id$ , $lid:names_id$ ) with
              [ $list: [rule_patt, None, code.item_item; wild_patt, None, wild_expr]$ ] >>
   in
   let rule_body = <:expr< ( $lid:subgoals_id$ , $lid:stack_id$ , $lid:extract_id$ ) >> in
   let rule_patt = <:patt< ( $lid:subgoals_id$ , $lid:extract_id$ ) >> in
   let rule_body = <:expr< let $list:[ rule_patt, rule_expr ]$ in $rule_body$ >> in
   let rule_patt = <:patt< $lid:stack_id$ >> in
   let rule_body' = <:expr< $apply_redex_expr loc$ $lid:redex_id$ $lid:addrs_id$ $lid:msequent_goal_id$ $lid:params_id$ >> in
   let rule_body = <:expr< let $list:[ rule_patt, rule_body' ]$ in $rule_body$ >> in
   let rule_patt = <:patt< ( $lid:msequent_goal_id$ , $lid:msequent_hyps_id$ ) >> in
   let rule_expr = <:expr< $dest_msequent_expr loc$ $lid:goal_id$ >> in
   let rule_body = <:expr< let $list:[ rule_patt, rule_expr ]$ in $rule_body$ >> in
   let rule_patt = <:patt< $lid:rule_id$ >> in
   let rule_let  = <:expr< let $list:[ rule_patt, fun_expr loc [addrs_id; names_id; goal_id; params_id] rule_body ]$ in $info_let$ >> in
   let body = define_ml_program proc loc strict_expr tparams name redex code rule_let in

   let rule_fun_expr =
      let tparams_ids_expr = list_expr loc lid_expr tparam_ids in
      let body = <:expr< $tactic_of_rule_expr loc$ $lid:name_rule_id$
                         [| $list:List.map lid_expr cvar_ids$ |]
                         $tparams_ids_expr$>>
      in
         fun_expr loc all_ids body
   in

   let rule_patt = <:patt< $lid:name_rule_id$ >> in
   let name_rule_let =
      <:str_item< value $rule_patt$ = $wrap_exn proc loc (name ^ "_rule") body$ >>
   in
   let name_let = <:str_item< value $name_patt$ = $bindings_let proc loc code rule_fun_expr$ >> in
      checkpoint_resources want_checkpoint loc name [name_rule_let; name_let; refiner_let loc]

let create_dform_expr loc modes =
   let string_expr s = <:expr< $str:s$ >> in
   match modes with
      Modes modes -> <:expr< Dform_print.create_dform_modes $list_expr loc string_expr modes$ >>
    | ExceptModes modes -> <:expr< Dform_print.create_dform_except_modes $list_expr loc string_expr modes$ >>
    | AllModes -> <:expr< Dform_print.create_dform_all >>

(*
 * Define a display form expansion.
 *)
let define_dform proc loc df expansion =
   let expr = <:expr< 
      $create_dform_expr loc df.dform_modes$ $lid:local_dformer_id$ {
         $dform_name_patt loc$ = $str: df.dform_name$;
         $dform_pattern_patt loc$ = $expr_of_term proc loc df.dform_redex$;
         $dform_options_patt loc$ = $list_expr loc (dform_option_expr loc) df.dform_options$;
         $dform_print_patt loc$ = Dform.DFormExpansion $expr_of_term proc loc expansion$
      }
   >> in
      [<:str_item< $exp: wrap_exn proc loc df.dform_name expr$ >>]

(*
 * Precedence definition relation.
 *)
let define_prec proc loc s =
   [<:str_item< value $lid:s$ = Precedence.new_prec () >>]

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
      [<:str_item< $exp:wrap_exn proc loc name expr$ >>]

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
 *)
let define_ml_dform proc loc
    { dform_name = name;
      dform_modes = modes;
      dform_options = options;
      dform_redex = t
    }
    { dform_ml_printer = printer;
      dform_ml_buffer = buffer;
      dform_ml_code = code
    } =
   let items = extract_redex_types (compile_redex Relaxed [||] t) in
   let dprinter_let_expr = <:expr<
      let $lid:dprinter_id$ =
         fun [
            { Dform.dform_term = $lid:term_id$;
              Dform.dform_items = $list_patt loc (rewrite_type_patt loc) items$;
              Dform.dform_printer = $lid:printer$;
              Dform.dform_buffer = $lid:buffer$ } ->
               $code.item_item$ $lid:term_id$
          | _ ->
               failwith "bad match"
         ]
      in
         $create_dform_expr loc modes$ $lid:local_dformer_id$ {
            $dform_name_patt loc$ = $str: name$;
            $dform_pattern_patt loc$ = $lid:term_id$;
            $dform_options_patt loc$ = $list_expr loc (dform_option_expr loc) options$;
            $dform_print_patt loc$ = Dform.DFormPrinter $lid:dprinter_id$
         }
   >> in
      [<:str_item< $exp: define_ml_program proc loc relaxed_expr [] name t code dprinter_let_expr$ >>]

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
   let intermediate = "_$" ^ name ^ "_resource_intermediate" in
   let input_name = input_type name in
      proc.imp_resources
         <- (name, <:ctyp< $lid:input_name$ >>) :: proc.imp_resources;
      [<:str_item< type $input_name$ = $input$ >>;
       <:str_item< value $lid:get_resource_name name$ =
         Mp_resource.create_resource $str:name$ (**)
            ( $expr$ : Mp_resource.resource_info $lid:input_name$ '$intermediate$ $output$ ) >>]

let rec is_list_expr = function
   MLast.ExUid(_,"[]") -> true
 | MLast.ExApp(_,MLast.ExApp(_,MLast.ExUid(_,"::"),_),tail) ->
      is_list_expr tail
 | _ -> false

let improve_resource proc loc { improve_name = name; improve_expr = expr } =
   let expr' = expr_of_bnd_expr proc loc expr in
   let improve_expr =
      if is_list_expr expr.item_item then impr_resource_list proc loc name expr'
      else impr_resource proc loc name expr'
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
   let rec find_resource name = function
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
         [name] -> [
            <:str_item< Mp_resource.extends_theory $str:name$ >>;
            <:str_item< $exp:refiner_expr loc$.join_refiner $lid: local_refiner_id$ $parent_path$.$lid: refiner_id$ >>;
            refiner_let loc;
            <:str_item< Dform_print.join_mode_base $lid: local_dformer_id$ $parent_path$.$lid: dformer_id$ >>
         ]
       | _ ->
            Stdpp.raise_with_loc loc (Invalid_argument "Including sub-theories not implemented")

(*
 * Collect the toploop values in this module.
 *)
let implem_toploop info =
   let rec collect = function
      (ToploopItem <:sig_item< value $s$ : $t$ >>, _) :: tl ->
         (s, t) :: collect tl
    | _ :: tl ->
         collect tl
    | [] ->
         []
   in
      collect (info_items info)

let define_summary_item proc loc = function
   { item_bindings = []; item_item = item } ->
      [item]
 | { item_bindings = bnds; item_item = <:str_item< value $list:[patt,expr]$ >> } ->
      let expr = <:expr< let $list:List.map (binding_let proc loc) (List.rev bnds)$ in $expr$ >> in
         [<:str_item< value $list:[patt, expr]$ >>]
 | { item_bindings = bnds; item_item = item } ->
      [<:str_item< value $list:List.map (binding_let proc loc) (List.rev bnds)$ >>; item]

(*
 * This function creates str_items for the toploop.
 *)
let add_toploop_item proc loc name ctyp =
   let expr = toploop_item_expr loc name ctyp in
      impr_toploop proc loc name expr

let get_top_ctyp proc name =
   let ctyp = List.assoc name proc.imp_toploop in
      proc.imp_toploop <- List.remove_assoc name proc.imp_toploop;
      ctyp

(*
 * An regular item.
 *)
let wrap_toploop_item proc loc ((patt, expr) as item) =
   match patt with
      <:patt< $lid:name$ >> when List.mem_assoc name proc.imp_toploop ->
         let ctyp = get_top_ctyp proc name in
         let expr1 = wrap_toploop_item loc name ctyp expr in
         let expr = toploop_item_expr loc name ctyp in
            (patt, expr1), [add_toploop_item proc loc name ctyp]
     | _ ->
         item, []

let wrap_toploop_items proc loc pel =
   let pel, resources = List.split (List.map (wrap_toploop_item proc loc) pel) in
      pel, List.flatten resources

let rec wrap_summary_items proc = function
   [] -> []
 | item::items ->
      let items = wrap_summary_items proc items in
      begin match item with
         MLast.StVal (loc, rec_flag, pel) ->
            let pel, toploop = wrap_toploop_items proc loc pel in
               <:str_item< value $rec:rec_flag$ $list:pel$ >> :: (toploop @ items)
       | MLast.StExt (loc, name, ctyp, _ ) when List.mem_assoc name proc.imp_toploop ->
            item :: add_toploop_item proc loc name (get_top_ctyp proc name) :: items
       | _ ->
         item::items
      end

(*
 * A magic block computes a hash value from the definitions
 * in the block.
 *)
let define_magic_block proc loc { magic_name = name; magic_code = stmts } =
   let index = List.fold_left Filter_hash.hash_str_item 0 stmts in
      <:str_item< value  $lid:name$ = $int:string_of_int index$ >> :: stmts

(*
 * Prolog declares the refiner and dformer.
 *)
let implem_prolog proc loc name =
   let term_let =
      if proc.imp_terms = [] && proc.imp_meta_terms = [] then [] else 
      let marshalled_terms = Ml_term.string_of_term_lists (List.rev proc.imp_terms) (List.rev proc.imp_meta_terms) in
         [<:str_item< value ($lid:global_term_var$, $lid:global_meta_term_var$) =
                         Ml_term.term_arrays_of_string $str:String.escaped marshalled_terms$>>]
   in
   <:str_item< value $lid:local_refiner_id$ = $refiner_expr loc$ . null_refiner $str: name$
               and   $lid:local_dformer_id$ = ref Dform_print.null_mode_base >> :: term_let

(*
 * Trailing declarations.
 *)
let implem_postlog proc loc = 
   match proc.imp_toploop with
      (name, _) :: _ -> raise (Failure ("Topval "^name^" not implemented (or was duplicated in the interface)"))
    | [] ->
         let name = <:expr< $str:proc.imp_name$ >> in [
            <:str_item< Mp_resource.close_theory $name$ >>;
            <:str_item< value $lid:refiner_id$ = $refiner_expr loc$.label_refiner $lid:local_refiner_id$ $name$ and
                              $lid:dformer_id$ = $lid:local_dformer_id$.val >>;
            <:str_item<
               Theory.record_theory {
                  Theory.thy_name = $name$;
                  Theory.thy_refiner = $lid:refiner_id$;
                  Theory.thy_dformer = $lid:dformer_id$
               }>>]

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
    | Rewrite ({ rw_name = name; rw_proof = (Interactive _ | Incomplete) } as rw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: rwinteractive: %s%t" name eflush;
         interactive_rewrite proc loc rw
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
    | CondRewrite ({ crw_name = name; crw_proof = (Interactive _|Incomplete) } as crw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: interactive condrw: %s%t" name eflush;
         interactive_cond_rewrite proc loc crw
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
    | Rule ({ rule_name = name; rule_proof = (Interactive _ | Incomplete) } as rule) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: interactive rule: %s%t" name eflush;
         interactive_rule proc loc rule
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
         raise(Invalid_argument "Filter_prog.extract_str_item: we should not have ToploopItem in str")
    | MagicBlock block ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: magic block%t" eflush;
         define_magic_block proc loc block
    | Opname _ | Id _ | Comment _ | Infix _ ->
         []
    | Module _ ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: infix%t" eflush;
         raise (Failure "Filter_prog.extract_str_item: nested modules are not implemented")
    | Definition def ->
         definition proc loc def

(*
 * Extract a signature.
 *)
let extract_str arg sig_info info resources name =
   let proc = { imp_sig_info = sig_info;
                imp_resources = [];
                imp_toploop = implem_toploop sig_info;
                imp_arg = arg;
                imp_name = name;
                imp_all_resources = resources;
                imp_terms = [];
                imp_num_terms = 0;
                imp_meta_terms = [];
                imp_num_meta_terms = 0;
              }
   in
   let items = List_util.flat_map (extract_str_item proc) (info_items info) in
   let items = wrap_summary_items proc items in
   let prolog = implem_prolog proc (0, 0) name in
   let postlog = implem_postlog proc (0, 0) in
      List.map (fun item -> item, (0, 0)) (prolog @ items @ postlog)

module ProofCaches = Filter_cache.MakeCaches (Proof_convert.Convert)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
