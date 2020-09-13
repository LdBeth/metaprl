(*
 * Conversion from module_info to program text.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and California
 * Institute of Technology
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
open Rewrite_sig
open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermTy
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Precedence
open Mp_resource

open Filter_base_type
open Filter_type
open Filter_util
open Filter_shape
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
 * For implementations, we maintain a state that contains
 *    1. a list of the resources that have been defined
 *)
type t =
   { imp_sig_info : (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info;
     mutable imp_toploop : (string * MLast.ctyp) list;
     imp_arg : Convert.t;
     imp_name : string;
     imp_group : string;
     imp_groupdesc : string;
     mutable imp_resources : (string * (MLast.ctyp * string)) list; (* The second string is the FQN *)
     imp_all_resources : (module_path * string * MLast.ctyp resource_sig) list;
     mutable imp_terms : term list;
     mutable imp_num_terms : int;
     mutable imp_meta_terms : meta_term list;
     mutable imp_num_meta_terms : int;
     mutable imp_nums : Lm_num.num list;
     mutable imp_num_nums : int;
     mutable imp_opnames : opname list;
     mutable imp_num_opnames : int
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

let () = Stdpp.loc_name := "_loc"

(*
 * Convert between expressions and terms.
 *)
let global_term_var = "_$globterms"
let global_meta_term_var = "_$globmterms"
let global_num_var = "_$globnums"
let global_opname_var = "_$globopnames"

let expr_of_term proc _loc t =
   proc.imp_terms <- t::proc.imp_terms;
   let num = proc.imp_num_terms in
      proc.imp_num_terms <- num + 1;
      <:expr< $lid:global_term_var$.($int:(string_of_int num)$) >>

let expr_of_meta_term proc _loc t =
   proc.imp_meta_terms <- t::proc.imp_meta_terms;
   let num = proc.imp_num_meta_terms in
      proc.imp_num_meta_terms <- num + 1;
      <:expr< $lid:global_meta_term_var$.($int:(string_of_int num)$) >>

let expr_of_num proc _loc i =
   proc.imp_nums <- i :: proc.imp_nums;
   let num = proc.imp_num_nums in
      proc.imp_num_nums <- succ num;
      <:expr< $lid:global_num_var$.($int:(string_of_int num)$) >>

let expr_of_opname proc _loc opname =
   proc.imp_opnames <- opname :: proc.imp_opnames;
   let num = proc.imp_num_opnames in
      proc.imp_num_opnames <- succ num;
      <:expr< $lid:global_opname_var$.($int:(string_of_int num)$) >>

(************************************************************************
 * UTILITIES                                                            *
 ************************************************************************)

(*
 * Construct an expression list.
 *)
let list_expr _loc f l =
   let rec map = function
      h::t ->
         let hd = f h in
         let tl = map t in
            <:expr< [ $hd$ :: $tl$ ] >>
    | [] ->
         <:expr< [] >>
   in
      map l

(*
 * Construct an expression list.
 *)
(* unused
let apply_patt _loc f l =
   let rec map = function
      [h] ->
         f h
    | h::t ->
         let hd = f h in
         let tl = map t in
            <:patt< $hd$ $tl$ >>
    | [] ->
         raise (Invalid_argument "apply_patt")
   in
      map l
*)

(*
 * Construct an expression list.
 *)
let list_patt _loc f l =
   let rec map = function
      [] ->
         <:patt< [] >>
    | h::t ->
         let hd = f h in
         let tl = map t in
            <:patt< [ $hd$ :: $tl$ ] >>
   in
      map l

(*
 * A multiple argument function (curried)
 *)
let rec fun_expr _loc ids body =
   match ids with
      h::t ->
         <:expr< fun $lid:h$ -> $fun_expr _loc t body$ >>
    | [] ->
         body

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

(*
 * Axiom.
 *)
let refiner_expr _loc =
   <:expr< Refiner.Refiner.Refine >>

let refiner_ctyp _loc =
   <:ctyp< Refiner.Refiner.Refine >>

let rewriter_expr _loc =
   <:expr< Refiner.Refiner.Rewrite >>

(* unused
let rewriter_patt _loc =
   <:patt< Refiner.Refiner.Rewrite >>
*)

let tactic_type_expr _loc =
   <:expr< Tactic_type.Tactic >>

let rewrite_type_expr _loc =
   <:expr< Tactic_type.Conversionals >>

let rewrite_type_ctyp _loc =
   <:ctyp< Tactic_type.Tactic >>

let dest_msequent_expr _loc =
   <:expr< $refiner_expr _loc$ . dest_msequent >>

(*
 * Rule.
 *)
let create_ml_rule_expr _loc =
   <:expr< $refiner_expr _loc$ . create_ml_rule >>

let tactic_of_rule_expr _loc =
   <:expr< $tactic_type_expr _loc$ . tactic_of_rule >>

let tactic_ctyp _loc =
   <:ctyp< Tactic_type.Tactic.tactic >>

(*
 * Rewrite.
 *)
let rewrite_ctyp _loc =
   <:ctyp< $rewrite_type_ctyp _loc$ . conv >>

let prim_rewrite_expr _loc =
   <:expr< $refiner_expr _loc$ . prim_rewrite >>

let def_rewrite_expr _loc =
   <:expr< $refiner_expr _loc$ . definitional_rewrite >>

let derived_rewrite_expr _loc =
   <:expr< $refiner_expr _loc$ . derived_rewrite >>

let delayed_rewrite_expr _loc =
   <:expr< $refiner_expr _loc$ . delayed_rewrite >>

let rewrite_of_pre_rewrite_expr _loc =
   <:expr< $rewrite_type_expr _loc$ . rewrite_of_pre_rewrite >>

(*
 * Conditional rewrite.
 *)
let cond_rewrite_ctyp _loc =
   <:ctyp< $rewrite_type_ctyp _loc$ . conv >>

let prim_cond_rewrite_expr _loc =
   <:expr< $refiner_expr _loc$ . prim_cond_rewrite >>

let derived_cond_rewrite_expr _loc =
   <:expr< $refiner_expr _loc$ . derived_cond_rewrite >>

let delayed_cond_rewrite_expr _loc =
   <:expr< $refiner_expr _loc$ . delayed_cond_rewrite >>

let apply_redex_expr _loc =
   <:expr< $rewriter_expr _loc$ . apply_redex >>

(*
 * Other expressions.
 *)
let refiner_ctyp _loc =
   <:ctyp< $refiner_ctyp _loc$ . refiner >>

let get_resource_name name =
   "get_" ^ name ^ "_resource"

let input_type name =
   "_$" ^ name ^ "_$$resource_input"

let output_type name =
   "_$" ^ name ^ "_$$resource_output"

let res_fqn path name =
   (String.concat "!" (List.map String.capitalize path)) ^ "!" ^ name

let refiner_id = "refiner"

let local_refiner_id = "_$global_refiner"
(* unused
let stack_id = "_$rewrite_stack"
*)

let add_lt_expr _loc =
   <:expr< Precedence.add_lt >>

let add_eq_expr _loc =
   <:expr< Precedence.add_eq >>

(*
 * Build an expression from a position.
 *)
let expr_of_loc _loc =
   let filename = Ploc.file_name _loc in
   let first_line = Ploc.line_nb _loc in
   let first_bol = Ploc.bol_pos _loc in
   let first_pos = Ploc.first_pos _loc in
   let last_pos = Ploc.last_pos _loc in
    <:expr< Ploc.make_loc $str:filename$ $int:string_of_int first_line$ $int:string_of_int first_bol$
            ($int:string_of_int first_pos$, $int:string_of_int last_pos$) "" >>

(*
 * Each rule gets a refiner associated with it, with the following name.
 *)
let refiner_let _loc =
   <:str_item< value $lid: refiner_id$ = $refiner_expr _loc$ .refiner_of_build $lid: local_refiner_id$ >>

let refiner_ignore _loc =
   <:str_item< value _ = $lid: refiner_id$ >>

let empty_rw_args _loc =
   <:expr< Refiner.Refiner.Rewrite.empty_rw_args >>

let empty_args_spec _loc =
   <:expr< Refiner.Refiner.Rewrite.empty_args_spec >>

let args_val_expr _loc i_ids a_ids =
   if i_ids = [] && a_ids = [] then
      empty_rw_args _loc
   else
      let lid s = <:expr< $lid:s$ >> in
         <:expr< {
            Rewrite_sig.arg_ints = [| $list: List.map lid i_ids$ |];
            Rewrite_sig.arg_addrs = [| $list: List.map lid a_ids$ |]
         } >>

let args_spec_expr _loc ivars avars =
   if ivars = [] && avars = [] then
      empty_args_spec _loc
   else
      let var_expr v = <:expr< Lm_symbol.add $str: string_of_symbol v$ >> in
         <:expr< {
            Rewrite_sig.spec_ints = [| $list: List.map var_expr ivars$ |];
            Rewrite_sig.spec_addrs = [| $list: List.map var_expr avars$ |]
         } >>

let rec apply_annotation_processor _loc e args =
   match args with
      [] ->
         e
    | arg :: args ->
         (* Label the arg if it isn't already labeled *)
         let arg =
            match arg with
               MLast.ExLab _ ->
                  arg
             | _ ->
                  <:expr< ~{ $lid:"options"$ = $arg$ } >>
         in
            apply_annotation_processor _loc <:expr< $e$ $arg$ >> args

(*
 * Variable names.
 *)
let exn_id              = "_$exn"
let term_id             = "_$term"
let redex_id            = "_$redex"
let contractum_id       = "_$contractum"
let params_id           = "_$params"
let subgoals_id         = "_$subgoals"
let args_id             = "_$args"
let bnames_id           = "_$bnames"
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
(* unused
let rule_name_id        = "_$rule_name"
*)

let expr_of_label _loc = function
   [] ->
      <:expr< None >>
 | [h] ->
      <:expr< Some $str: h$ >>
 | _ ->
      Stdpp.raise_with_loc _loc (Invalid_argument "Filter_prog.expr_of_label: multi-component labels not supported")

(*
 * Print a message on loading, and catch errors.
 *    Lm_debug.show_loading "Loading name%t";
 *    try e with
 *       exn ->
 *          Refine_exn.stderr_exn name exn
 *)
let wrap_exn proc _loc name e =
   let name = (String.capitalize proc.imp_name) ^ "." ^ name in
   <:expr<
      do {
         (* Print a message before the execution *)
         Lm_debug.show_loading $str: "Loading " ^ name ^ "%t"$;
         (* Wrap the body to catch exceptions *)
         try $e$ with
         $lid: exn_id$ ->
            Filter_exn.stderr_exn $str: name$ $lid: exn_id$
      }
   >>

(*
 * Create function type.
 *)
let params_ctyp _loc ctyp params =
   let rec convert = function
      [] ->
         ctyp
    | h::t ->
         let ctyp' = convert t in
         let arg_type =
            match h with
               IntParam _ ->
                  <:ctyp< int >>
             | AddrParam _ ->
                  <:ctyp< Refiner.Refiner.TermAddr.address >>
             | TermParam _ ->
                  <:ctyp< Refiner.Refiner.TermType.term >>
         in
            <:ctyp< $arg_type$ -> $ctyp'$ >>
   in
      convert params

(*
 * Convert display form options to expressions.
 *)
let dform_option_expr _loc = function
   DFormParens -> <:expr< Dform.DFormParens >>
 | DFormPrec p -> <:expr< Dform.DFormPrec $lid:p$ >>
 | DFormInheritPrec -> <:expr< Dform.DFormInheritPrec >>

(*
 * Convert a module path to an expression.
 *)
let rec parent_path_expr _loc = function
   [h] ->
      <:expr< $uid:String.capitalize h$ >>
 | h::t ->
      <:expr< $uid:String.capitalize h$ . $parent_path_expr _loc t$ >>
 | [] ->
      raise (Invalid_argument "parent_path")

let rec parent_path_ctyp _loc = function
   [h] ->
      <:ctyp< $uid:String.capitalize h$ >>
 | h::t ->
      <:ctyp< $uid:String.capitalize h$ . $parent_path_ctyp _loc t$ >>
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
let toploop_item_expr _loc name ctyp =
   let str_lid s =
      match s with
         "unit" | "bool" |  "int" | "string" | "term" | "tactic" | "conv" | "address" | "addr_item" ->
            String.capitalize s
       | _ ->
            raise_toploop_exn _loc
   in let rec collect index expr = function
      <:ctyp< $lid: typ$ >> ->
         let name = str_lid typ in
            <:expr< Shell_sig. $uid: name ^ "Expr"$ $expr$ >>, <:expr< Shell_sig. $uid: name ^ "Type"$ >>
    | <:ctyp< $t1$ -> $t2$ >> ->
         let v = sprintf "v%d" index in
         let patt = <:patt< $lid: v$ >> in
         let expr,texpr = collect (succ index) <:expr< $expr$ $lid: v$ >> t2 in
         let expr = <:expr< fun [ $list: [patt, Ploc.VaVal None, expr]$ ]>> in
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
                  raise_toploop_exn _loc
            end
    | _ ->
         raise_toploop_exn _loc
   in
      collect 0 <:expr< $lid: name$ >> ctyp

(************************************************************************
 * SIGNATURES                                                           *
 ************************************************************************)

(*
 * Rewrites.
 *)
let declare_rewrite _loc rw =
   [<:sig_item< value $rw.rw_name$ : $rewrite_ctyp _loc$ >>]

let declare_define_term _loc _ def =
   [<:sig_item< value $def.term_def_name$ : $rewrite_ctyp _loc$ >>]

let declare_cond_rewrite _loc { crw_name = name; crw_params = params; _ } =
   [<:sig_item< value $name$ : $params_ctyp _loc (cond_rewrite_ctyp _loc) params$ >>]

let declare_ml_rewrite _loc { mlterm_name = name; mlterm_params = params; _ } =
   [<:sig_item< value $name$ : $params_ctyp _loc (cond_rewrite_ctyp _loc) params$ >>]

(*
 * Rules.
 *)
let declare_rule _loc { rule_name = name; rule_params = params; _ } =
   [<:sig_item< value $name$ : $params_ctyp _loc (tactic_ctyp _loc) params$ >>]

let declare_ml_axiom _loc { mlterm_name = name; mlterm_params = params; _ } =
   [<:sig_item< value $name$ : $params_ctyp _loc (tactic_ctyp _loc) params$ >>]

(*
 * Precedence.
 *)
let declare_prec _loc name =
   [<:sig_item< value $name$ : Precedence.precedence >>]

(*
 * Resource.
 *)
let declare_resource _loc name res =
   let outp_name = output_type name in
      [<:sig_item< type $input_type name$ = $res.resource_input$ >>;
       <:sig_item< type $outp_name$ = $res.resource_output$ >>;
       <:sig_item< value $get_resource_name name$ : Mp_resource.global_resource -> $lid:outp_name$ >>]

(*
 * When a parent is declared, we need to open all the ancestors.
 *)
let declare_parent _loc _ =
   []

(*
 * Standard summary item.
 *)
let declare_summary_item loc item =
   if item.item_bindings <> [] then
      Stdpp.raise_with_loc loc (Invalid_argument "Signature items can not have bindings in them!");
   [item.item_item]

let declare_toploop_item _loc item =
   begin match item with
         <:sig_item< value $s$ : $t$ >> ->
            (* Check that the type is understood *)
            ignore(toploop_item_expr (MLast.loc_of_ctyp t) s t)
       | _ ->
            Stdpp.raise_with_loc _loc (RefineError ("declare_toploop_item", StringError "illegal topval"))
   end;
   [item]

(*
 * Magic block is a block of items.
 *)
let declare_magic_block _loc { magic_code = items; _ } =
   items

(*
 * Trailer declares a new refiner.
 *)
let interf_postlog info _loc =
   [<:sig_item< value $refiner_id$ : $refiner_ctyp _loc$ >>]

(*
 * Extract a signature item.
 *)
let extract_sig_item (item, loc) =
   match item with
      Rewrite ({ rw_name = name; _ } as rw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: rewrite: %s%t" name eflush;
         declare_rewrite loc rw
    | InputForm { iform_name = name; _ } ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: input form: %s%t" name eflush;
         []
    | CondRewrite ({ crw_name = name; _ } as crw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: cond rewrite: %s%t" name eflush;
         declare_cond_rewrite loc crw
    | Rule ({ rule_name = name; _ } as item) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: rule: %s%t" name eflush;
         declare_rule loc item
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
    | Parent ({ parent_name = name; _ } as parent) ->
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
    | DefineTerm (shapeclass, class_term, term_def)
      when is_shape_normal shapeclass ->
         declare_define_term loc class_term term_def
    | DefineTerm _
    | DeclareTypeClass _
    | DeclareType _
    | DeclareTerm _
    | DeclareTypeRewrite _
    | DForm _
    | PrecRel _
    | Id _
    | Comment _
    | MLGramUpd _
    | PRLGrammar _ ->
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

(*
 * Extract a signature.
 *)
let extract_sig _ info resources _ _ _ =
   let _ =
      if !debug_filter_prog then
         eprintf "Filter_prog.extract_sig: begin%t" eflush
   in
   let items = Lm_list_util.flat_map extract_sig_item (info_items info) in
   let postlog = interf_postlog resources dummy_loc in
      List.map (fun item -> item, dummy_loc) (items @ postlog)

(************************************************************************
 * UTILITIES                                                            *
 ************************************************************************)

 (*
  * Eta-reduction
  *)
(* unused
let beta_reduce_var var f =
   let _loc = MLast.loc_of_expr f in
      match f with
         <:expr< fun $lid:v$ -> $e$ >> ->
            v, e
       | _ ->
            var, <:expr< $f$ $lid:var$ >>
*)

let checkpoint_resources want_checkpoint _loc rule_name rest =
   if want_checkpoint then
      <:str_item< (Mp_resource.bookmark $str:rule_name$) >> :: rest
   else
      rest

let find_res proc loc name =
   try
      List.assoc name proc.imp_resources
   with
      Not_found ->
         Stdpp.raise_with_loc loc (Failure ("Attempted to use undeclared resource " ^ name))

let flag_expr _loc = function
   Private -> <:expr< Mp_resource.Private >>
 | Public -> <:expr< Mp_resource.Public >>

let impr_resource proc _loc flag name expr =
   let ctyp, fqn = find_res proc _loc name in
      <:expr< Mp_resource.improve $flag_expr _loc flag$ $str:fqn$ (Obj.repr ( $expr$ : $ctyp$ )) >>

let impr_resource_list proc _loc flag name expr =
   let ctyp, fqn = find_res proc _loc name in
      <:expr< Mp_resource.improve_list $flag_expr _loc flag$ $str:fqn$ (Obj.magic ( $expr$ : list $ctyp$ )) >>

(* unused
let rec mk_string_list_expr _loc = function
   [] ->
      <:expr< [] >>
 | hd::tl ->
      <:expr< $lid:"::"$ $str:hd$ $mk_string_list_expr _loc tl$ >>
*)

let binding_let proc _loc (v, bnd) =
   <:patt< $lid:v$ >>,
   match bnd with
      BindTerm t ->
         expr_of_term proc _loc t
    | BindOpname op ->
         expr_of_opname proc _loc op
    | BindNum n ->
         expr_of_num proc _loc n

let bindings_let proc _loc bnd_expr expr =
   if bnd_expr.item_bindings = [] then
      expr
   else
      <:expr< let $list:List.map (binding_let proc _loc) (List.rev bnd_expr.item_bindings)$ in $expr$ >>

let expr_of_bnd_expr proc _loc expr =
   bindings_let proc _loc expr expr.item_item

(************************************************************************
 * TOP LOOP                                                             *
 ************************************************************************)

let impr_toploop proc _loc name pvt_flag (expr,texpr) =
   let expr = <:expr< ($str:proc.imp_name$, $str: name$, $expr$, $texpr$) >> in
      <:str_item< ($impr_resource proc _loc pvt_flag "toploop" expr$) >>

(*
 * This is a little bogus, but we add rewrites automatically to the
 * toploop resource.
 *)
let rec loop_params _loc i body base_expr = function
   h :: t ->
      let v = "v" ^ string_of_int i in
      let expr, texpr = loop_params _loc (succ i) <:expr< $body$ $lid: v$ >> base_expr t in
      let expr = <:expr< fun $lid:v$ -> $expr$ >> in
      let expr =
         match h with
            IntParam _ ->
               <:expr< Shell_sig.IntFunExpr $expr$ >>, <:expr< Shell_sig.FunType Shell_sig.IntType $texpr$ >>
          | AddrParam _ ->
               <:expr< Shell_sig.AddressFunExpr $expr$ >>, <:expr< Shell_sig.FunType Shell_sig.AddressType $texpr$ >>
          | TermParam _ ->
               <:expr< Shell_sig.TermFunExpr $expr$ >>, <:expr< Shell_sig.FunType Shell_sig.TermType $texpr$ >>
      in
         expr
 | [] ->
      base_expr body

let toploop_rewrite proc _loc name pvt_flag params =
   let base body = <:expr< Shell_sig.ConvExpr $body$ >>, <:expr< Shell_sig.ConvType >> in
      impr_toploop proc _loc name pvt_flag (loop_params _loc 0 <:expr< $lid: name$ >> base params)

let toploop_rule proc _loc name params =
   let base body = <:expr< Shell_sig.TacticExpr $body$ >>, <:expr< Shell_sig.TacticType >> in
      impr_toploop proc _loc name Public (loop_params _loc 0 <:expr< $lid: name$ >> base params)

(*
 * Build the wrap code.
 *)
let wrap_tactic_expr _loc =
   <:expr< Tactic_type.Tacticals.wrapT >>

let wrap_optimized _loc name arglist_name vars expr =
   let name = <:expr< $str:name$ >> in
      if vars = [] then
         <:expr< $wrap_tactic_expr _loc$ (Tactic_boot_sig . $uid:arglist_name$ $name$ ) $expr$ >>
      else
         <:expr< $wrap_tactic_expr _loc$ (Tactic_boot_sig . $uid:arglist_name$ ( $list:name :: vars$ )) $expr$ >>

let wrap_arg _loc arg v =
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
      <:expr< Tactic_boot_sig. $uid:s$ $v$ >>

let wrap_general _loc name wrap vars expr =
      <:expr< $wrap_tactic_expr _loc$ (Tactic_boot_sig.GeneralArgList
                                     [| $list:List.map2 (wrap_arg _loc) wrap vars$ |])
              $expr$ >>

let wrap_expr _loc name wrap expr =
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
            wrap_optimized _loc name "NoneArgList" vars expr
       | [IntArg] ->
            wrap_optimized _loc name "IntArgList" vars expr
       | [BoolArg] ->
            wrap_optimized _loc name "BoolArgList" vars expr
       | [StringArg] ->
            wrap_optimized _loc name "StringArgList" vars expr
       | [TermArg] ->
            wrap_optimized _loc name "TermArgList" vars expr
       | [IntArg; IntArg] ->
            wrap_optimized _loc name "IntIntArgList" vars expr
       | [IntArg; BoolArg] ->
            wrap_optimized _loc name "IntBoolArgList" vars expr
       | [IntArg; StringArg] ->
            wrap_optimized _loc name "IntStringArgList" vars expr
       | [IntArg; TermArg] ->
            wrap_optimized _loc name "IntTermArgList" vars expr
       | [BoolArg; IntArg] ->
            wrap_optimized _loc name "BoolIntArgList" vars expr
       | [BoolArg; BoolArg] ->
            wrap_optimized _loc name "BoolBoolArgList" vars expr
       | [BoolArg; StringArg] ->
            wrap_optimized _loc name "BoolStringArgList" vars expr
       | [BoolArg; TermArg] ->
            wrap_optimized _loc name "BoolTermArgList" vars expr
       | [StringArg; IntArg] ->
            wrap_optimized _loc name "StringIntArgList" vars expr
       | [StringArg; BoolArg] ->
            wrap_optimized _loc name "StringBoolArgList" vars expr
       | [StringArg; StringArg] ->
            wrap_optimized _loc name "StringStringArgList" vars expr
       | [StringArg; TermArg] ->
            wrap_optimized _loc name "StringTermArgList" vars expr
       | [TermArg; IntArg] ->
            wrap_optimized _loc name "TermIntArgList" vars expr
       | [TermArg; BoolArg] ->
            wrap_optimized _loc name "TermBoolArgList" vars expr
       | [TermArg; StringArg] ->
            wrap_optimized _loc name "TermStringArgList" vars expr
       | [TermArg; TermArg] ->
            wrap_optimized _loc name "TermTermArgList" vars expr
       | wrap ->
            wrap_general _loc name wrap vars expr
   in
      fun_expr _loc names expr

(*
 * Wrap a toploop expression.
 *)
let wrap_toploop_item _loc name ctyp expr =
   let rec collect wrap = function
      <:ctyp< tactic >> ->
         wrap_expr _loc name (List.rev wrap) expr
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
 * REWRITES                                                             *
 ************************************************************************)

let extract_expr _loc modname name =
   <:expr< Shell_command.extract [ $str:modname$; $str:name$ ] >>

(*
 * Define the resources for a rewrite.
 * The Tactic_type.pre_rewrite is passed as an argument,
 * along with the params, so that we can figure out its type.
 *)
let define_rewrite_resources proc _loc name force_private redex contractum assums addrs params resources name_id_expr =
   if resources.item_item = [] then
      <:expr< () >>
   else
   let define_resource {res_loc = _loc; res_name = name'; res_flag = flag; res_args = args} =
   let flag = if force_private then Private else flag in
      let processor = apply_annotation_processor _loc <:expr< $lid:"process_" ^ name' ^ "_resource_rw_annotation"$ >> args in
         impr_resource_list proc _loc flag name' <:expr<
            $processor$ $str:name$ $redex$ $contractum$ $assums$ $addrs$ $params$ $expr_of_loc _loc$ $name_id_expr$
         >>
   in
      bindings_let proc _loc resources <:expr< do { $list:List.map define_resource resources.item_item$ } >>

(*
 * A primitive rewrite is assumed true by fiat.
 *)
let define_rewrite want_checkpoint force_private code proc _loc rw expr =
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
      let $lid:redex_id$ = $expr_of_term proc _loc rw.rw_redex$ in
      let $lid:contractum_id$ = $expr_of_term proc _loc rw.rw_contractum$ in
      let $lid:rw_id$ =
         $refiner_expr _loc$.create_rewrite $lid:local_refiner_id$ $str:name$ $redex$ $contractum$
      in do {
         $prim_expr$;
         $define_rewrite_resources proc _loc name force_private redex contractum nil (empty_args_spec _loc) nil rw.rw_resources rw_id_expr$;
         $rewrite_of_pre_rewrite_expr _loc$ $rw_id_expr$ $empty_rw_args _loc$ $nil$
      }
    >> in
       checkpoint_resources want_checkpoint _loc name [
          <:str_item< value $lid:name$ = $wrap_exn proc _loc name create_rw$ >>;
          refiner_let _loc;
          refiner_ignore _loc;
          toploop_rewrite proc _loc name (if force_private then Private else Public) []
       ]

let add_crw_selector _loc res =
   let rec add_crw = function
      [] -> [ <:expr< ~{labels = Perv.crw_labels} >> ]
    | <:expr< ~{labels = $labels$} >> :: args -> <:expr< ~{labels = [Perv.select_crw :: $labels$]} >> :: args
    | arg :: args -> arg :: (add_crw args)
   in
      { res with res_args = add_crw res.res_args }

(*
 * Conditional rewrite is a little more complicated.
 *)
let define_cond_rewrite want_checkpoint code proc _loc crw expr =
   let name          = crw.crw_name in
   let rw_id         = "_$" ^ name ^ "_rewrite" in
   let rw_id_expr    = <:expr< $lid:rw_id$ >> in
   let rw_id_patt    = <:patt< $lid:rw_id$ >> in
   let lid_expr s    = <:expr< $lid:s$ >> in
   let args_expr     = <:expr< $lid:args_id$ >> in
   let params_expr   = <:expr< $lid:params_id$ >> in
   let subgoals_expr = <:expr< $lid:subgoals_id$ >> in
   let ivars, avars, tparams = split_params crw.crw_params in
   let all_ids, ivar_ids, avar_ids, tparam_ids = name_params crw.crw_params in
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
   let resources =
      if crw.crw_assums == [] then
         crw.crw_resources
      else
         { crw.crw_resources with item_item = List.map (add_crw_selector _loc) crw.crw_resources.item_item }
   in
   let create_expr = <:expr<
      let $lid:args_id$ = $args_spec_expr _loc ivars avars$ in
      let $lid:params_id$ = $list_expr _loc (expr_of_term proc _loc) tparams$ in
      let $lid:subgoals_id$ = $list_expr _loc (expr_of_term proc _loc) crw.crw_assums$ in
      let $lid:redex_id$ = $expr_of_term proc _loc crw.crw_redex$ in
      let $lid:contractum_id$ = $expr_of_term proc _loc crw.crw_contractum$ in
      let $rw_id_patt$ =
         $refiner_expr _loc$.create_cond_rewrite $lid:local_refiner_id$ $str:name$ (**)
            $args_expr$ $params_expr$ $subgoals_expr$ $redex$ $contractum$
      in do {
         $prim_expr$;
         $define_rewrite_resources proc _loc name false redex contractum subgoals_expr args_expr params_expr resources rw_id_expr$;
         $rw_id_expr$
      }
    >> in
    let rw_fun_expr =
        fun_expr _loc all_ids <:expr<
           $rewrite_of_pre_rewrite_expr _loc$ $rw_id_expr$ $args_val_expr _loc ivar_ids avar_ids$ $list_expr _loc lid_expr tparam_ids$ >>
    in
       checkpoint_resources want_checkpoint _loc name [
          <:str_item< value $rw_id_patt$ = $wrap_exn proc _loc name create_expr $ >>;
          <:str_item< value $lid:name$ = $rw_fun_expr$ >>;
          refiner_let _loc;
          refiner_ignore _loc;
          toploop_rewrite proc _loc name Public crw.crw_params
       ]

let prim_rewrite proc _loc rw =
   define_rewrite false false (prim_rewrite_expr _loc) proc _loc rw None

let define_term proc _loc class_term def =
   let rw =
      { rw_name = def.term_def_name;
        rw_redex = term_of_ty class_term;
        rw_contractum = def.term_def_value;
        rw_proof = xnil_term;
        rw_resources = def.term_def_resources
      }
   in
      define_rewrite false def.term_def_opaque (def_rewrite_expr _loc) proc _loc rw None

let prim_cond_rewrite proc _loc crw =
   define_cond_rewrite false (prim_cond_rewrite_expr _loc) proc _loc crw None

(*
 * Justify a rewrite with a tactic.
 *)
let derived_rewrite proc _loc rw expr =
   define_rewrite true false (derived_rewrite_expr _loc) proc _loc rw (Some expr)

let derived_cond_rewrite proc _loc crw expr =
   define_cond_rewrite true (derived_cond_rewrite_expr _loc) proc _loc crw (Some expr)

(*
 * Interactive forms.
 *)
let interactive_rewrite proc _loc rw =
   define_rewrite true false (delayed_rewrite_expr _loc) proc _loc rw (Some (extract_expr _loc proc.imp_name rw.rw_name))

let interactive_cond_rewrite proc _loc crw =
   define_cond_rewrite true (delayed_cond_rewrite_expr _loc) proc _loc crw (Some (extract_expr _loc proc.imp_name crw.crw_name))

(*
 * An ML rewrite performs the same action as a conditional rewrite,
 * but the ML code computes the rewrite.
 *)
let define_ml_rewrite proc _loc mlrw rewrite_expr =
   let name = mlrw.mlterm_name in
   let name_patt = <:patt< $lid:name$ >> in
   let lid_expr s = <:expr< $lid:s$ >> in
   let rewrite_id_expr = lid_expr rewrite_id in
   let redex_id_expr = lid_expr redex_id in
   let nil_term = <:expr< Refiner.Refiner.TermMan.xnil_term >> in (* XXX HACK *)

   let params = mlrw.mlterm_params in
   let all_ids, ivar_ids, avar_ids, tparam_ids = name_params params in
   let simple_flag = params = [] in
   let addrs_spec, addrs_val, create_ml_rewrite_expr =
      if simple_flag then
         empty_args_spec _loc,
         empty_rw_args _loc,
         <:expr< $refiner_expr _loc$ . create_ml_rewrite >>
      else
         raise(Invalid_argument("ML rewrites with parameters are not currently supported - the code is there, but needs to be cleaned up"))
         (*
         let ivars, avars, tparams = split_params params in
            list_expr _loc (expr_of_term proc _loc) tparams,
            <:expr< unknown >>,
            <:expr< $refiner_expr _loc$ . create_ml_cond_rewrite >>
          *)
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
      let $lid:stack_id$ = $apply_redex_expr _loc$ $rewrite_id_expr$ $addrs_val$ $lid:goal_id$ $params_expr$ in $rewrite_body$
   >> in
   let args_ids =
      if simple_flag then
         [goal_id]
      else
         [names_id; bnames_id; params_id; goal_id]
   in
   let body = <:expr<
      let $lid:redex_id$ = $expr_of_term proc _loc mlrw.mlterm_term$ in
      let $lid:rewrite_id$ =
         Refiner.Refiner.Rewrite.compile_redices Rewrite_sig.Strict $addrs_spec$ (**)
            [ $redex_id_expr$ :: $params_expr$ ]
      in
      let $lid:rewrite_id$ = $bindings_let proc _loc rewrite_expr (fun_expr _loc args_ids rewrite_body)$ in
      let $lid:rewrite_id$ = $create_ml_rewrite_expr$ $lid:local_refiner_id$ $str:name$ $rewrite_id_expr$ in
      do {
         $define_rewrite_resources proc _loc name false redex_id_expr nil_term (<:expr< [] >>) addrs_spec params_expr mlrw.mlterm_resources rewrite_id_expr$;
         $rewrite_id_expr$
      }
   >> in
      [
         <:str_item< value $name_patt$ =
            $rewrite_of_pre_rewrite_expr _loc$ ($wrap_exn proc _loc name body $) $addrs_val$ $list_expr _loc lid_expr tparam_ids$ >>;
         refiner_let _loc;
         refiner_ignore _loc
      ]

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*
 * Define the resources for a rule.
 * The Tactic_type.pre_tactic is passed as an argument,
 * along with the params, so that we can figure out its type.
 *)
let define_rule_resources proc _loc name args_id params_id assums_id resources name_rule_expr =
   if resources.item_item = [] then <:expr< () >> else
   let define_resource {res_loc = _loc; res_name = name'; res_flag = flag; res_args = args} =
      let processor = apply_annotation_processor _loc <:expr< $lid:"process_" ^ name' ^ "_resource_annotation"$ >> args in
         impr_resource_list proc _loc flag name' <:expr<
            $processor$ $str:name$ $lid:args_id$ $lid:params_id$ $lid:assums_id$ $expr_of_loc _loc$ $name_rule_expr$
         >>
   in
      bindings_let proc _loc resources <:expr< do { $list:List.map define_resource resources.item_item$ } >>

let define_rule prim_rule deffun proc _loc
    { rule_name = name;
      rule_params = params;
      rule_stmt = stmt;
      rule_resources = resources;
      _
    }
    extract =
   (* Check the specifications *)
   let lid_expr s = <:expr< $lid:s$ >> in

   (* Expressions *)
   let name_rule_id = "_$" ^ name ^ "_rule" in
   let ivars, avars, tparams = split_params params in
   let all_ids, ivar_ids, avar_ids, tparam_ids = name_params params in
   let labels, ext_args, mterm = split_mfunction stmt in
   let name_rule_expr = lid_expr name_rule_id in
   let name_value =
      fun_expr _loc all_ids <:expr<
         $tactic_of_rule_expr _loc$ $name_rule_expr$ $args_val_expr _loc ivar_ids avar_ids$ $list_expr _loc lid_expr tparam_ids$
      >>
   in
   let extract_args = if prim_rule then list_expr _loc (expr_of_term proc _loc) ext_args else <:expr< () >> in
   let rule_expr = <:expr<
      let $lid:args_id$ = $args_spec_expr _loc ivars avars$ in
      let $lid:params_id$ = $list_expr _loc (expr_of_term proc _loc) tparams$ in
      let $lid:assums_id$ = $expr_of_meta_term proc _loc mterm$ in
      let $lid:rule_id$ =
         $refiner_expr _loc$.create_rule $lid:local_refiner_id$ $str:name$ $lid:args_id$ $lid:params_id$ $lid:assums_id$
      in
      let $lid:name_rule_id$ =
         $tactic_type_expr _loc$.compile_rule $lid:local_refiner_id$ ($list_expr _loc (expr_of_label _loc) labels$) $lid:rule_id$
      in
      let _ = do {
         $refiner_expr _loc$.$lid:deffun$ $lid:local_refiner_id$ $str:name$ $lid:args_id$ $lid:params_id$ $lid:assums_id$ $extract_args$ $extract$;
         $define_rule_resources proc _loc name args_id params_id assums_id resources name_rule_expr$
      }
      in
         $name_rule_expr$
      >>
   in
      checkpoint_resources (not prim_rule) _loc name (**)
        [<:str_item< value $lid:name_rule_id$ = $wrap_exn proc _loc name rule_expr$ >>;
         <:str_item< value $lid:name$ = $name_value$ >>;
         refiner_let _loc;
         refiner_ignore _loc;
         toploop_rule proc _loc name params]

let prim_rule proc _loc ax extract =
   let extract_expr = expr_of_term proc _loc extract in
      define_rule true "prim_rule" proc _loc ax extract_expr

let derived_rule proc _loc ax tac =
   define_rule false "derived_rule" proc _loc ax tac

let interactive_rule proc _loc ax =
   define_rule false "delayed_rule" proc _loc ax (extract_expr _loc proc.imp_name ax.rule_name)

(*
 * An ML rule performs the same action as a normal one,
 * but the ML code computes the subgoals.
 *)
let define_ml_rule want_checkpoint proc _loc
    { mlterm_name       = name;
      mlterm_params     = params;
      mlterm_term       = redex;
      _
    } code =
   (* Names *)
   let name_rule_id = "_$" ^ name ^ "_rule" in
   let name_patt = <:patt< $lid:name$ >> in
   let lid_expr s = <:expr< $lid:s$ >> in

   let ivars, avars, tparams = split_params params in
   let all_ids, ivar_ids, avar_ids, tparam_ids = name_params params in
   let params_expr = list_expr _loc (expr_of_term proc _loc) tparams in

   let rule_body = <:expr<
      let ($lid:msequent_goal_id$, $lid:msequent_hyps_id$) = $dest_msequent_expr _loc$ $lid:goal_id$ in
      let $lid:stack_id$ = $apply_redex_expr _loc$ $lid:redex_id$ $lid:addrs_id$ $lid:msequent_goal_id$ $lid:params_id$ in
      let ($lid:subgoals_id$, $lid:extract_id$) =
         ($code.item_item$ $lid:addrs_id$ $lid:params_id$ $lid:msequent_goal_id$ $lid:msequent_hyps_id$)
      in
         ($lid:subgoals_id$, $lid:extract_id$)
      >>
   in
   let rule_let  = <:expr<
      let $lid:rule_id$ = $fun_expr _loc [addrs_id; goal_id; params_id] rule_body$ in
         $tactic_type_expr _loc$.compile_ml_rule $lid:local_refiner_id$ (**)
            ($create_ml_rule_expr _loc$ $lid:local_refiner_id$ $str:name$ $lid:rule_id$)
      >>
   in
   let body = <:expr<
      let $lid:redex_id$ =
         Refiner.Refiner.Rewrite.compile_redices Rewrite_sig.Strict $args_spec_expr _loc ivars avars$
            [ $expr_of_term proc _loc redex$ :: $params_expr$ ]
      in
         $bindings_let proc _loc code rule_let$
      >>
   in
   let rule_fun_expr =
      fun_expr _loc all_ids <:expr<
         $tactic_of_rule_expr _loc$ $lid:name_rule_id$ $args_val_expr _loc ivar_ids avar_ids$ $list_expr _loc lid_expr tparam_ids$
      >>
   in
      checkpoint_resources want_checkpoint _loc name (**)
         [<:str_item< value $lid:name_rule_id$ = $wrap_exn proc _loc name body$ >>;
          <:str_item< value $name_patt$ = $bindings_let proc _loc code rule_fun_expr$ >>;
          refiner_let _loc;
          refiner_ignore _loc]

let create_dform_expr _loc name modes options term expr =
   let string_expr s = <:expr< $str:s$ >> in
   let modes =
      match modes with
         Dform.Modes modes -> <:expr< Dform.Modes $list_expr _loc string_expr modes$ >>
       | Dform.ExceptModes modes -> <:expr< Dform.ExceptModes $list_expr _loc string_expr modes$ >>
       | Dform.AllModes -> <:expr< Dform.AllModes >>
       | Dform.PrimitiveModes -> <:expr< Dform.PrimitiveModes >>
   in
      <:expr< Dform.add_dform {
         Dform.dform_modes = $modes$;
         Dform.dform_name = $str: name$;
         Dform.dform_pattern = $term$;
         Dform.dform_options = $list_expr _loc (dform_option_expr _loc) options$;
         Dform.dform_print = $expr$
      } >>

(*
 * Define a display form expansion.
 *)
let define_dform proc _loc df expansion =
   let expr =
      create_dform_expr _loc df.dform_name df.dform_modes df.dform_options (expr_of_term proc _loc df.dform_redex)
         <:expr< Dform.DFormExpansion $expr_of_term proc _loc expansion$ >>
   in
      [<:str_item< $exp: wrap_exn proc _loc df.dform_name expr$ >>]

(*
 * Precedence definition relation.
 *)
let define_prec proc _loc s =
   [<:str_item< value $lid:s$ = Precedence.new_prec () >>]

let define_prec_rel proc _loc
    { prec_left = s;
      prec_right = s';
      prec_rel = rel
    } =
   let expr =
      match rel with
         NoRelation ->
            <:expr< () >>
       | LTRelation ->
            <:expr< $add_lt_expr _loc$ $lid:s$ $lid:s'$ >>
       | EQRelation ->
            <:expr< $add_eq_expr _loc$ $lid:s$ $lid:s'$ >>
       | GTRelation ->
            <:expr< $add_lt_expr _loc$ $lid:s'$ $lid:s$ >>
   in
   let name = sprintf "%s..%s" s s' in
      [<:str_item< $exp:wrap_exn proc _loc name expr$ >>]

(*
 * Pattern to match rewrite destruction.
 *)
let rewrite_type_patt _loc (kind, name) =
   let name = <:patt< $lid:string_of_symbol name$ >> in
      match kind with
         RewriteTermType ->
            <:patt< Rewrite_sig.RewriteTerm $name$ >>
       | RewriteStringType ->
            <:patt< Rewrite_sig.RewriteString $name$ >>
       | RewriteTokenType ->
            <:patt< Rewrite_sig.RewriteToken $name$ >>
       | RewriteVarType ->
            <:patt< Rewrite_sig.RewriteString (Rewrite_sig.RewriteMetaParam $name$) >>
       | RewriteNumType ->
            <:patt< Rewrite_sig.RewriteNum $name$ >>
       | RewriteShapeType ->
            <:patt< Rewrite_sig.RewriteShape $name$ >>
       | RewriteOperatorType ->
            <:patt< Rewrite_sig.RewriteOperator $name$ >>
       | RewriteLevelType ->
            <:patt< Rewrite_sig.RewriteLevel $name$ >>
       | RewriteUnsupportedType ->
            <:patt< _ >>

(*
 * An ml dterm is a display form that is computed in ML.
 *)
let define_ml_dform proc _loc
    { dform_name = name;
      dform_modes = modes;
      dform_options = options;
      dform_redex = t;
      _
    }
    { dform_ml_printer = printer;
      dform_ml_buffer = buffer;
      dform_ml_code = code
    } =
   let items = extract_redex_types (compile_redex Rewrite_sig.Relaxed Rewrite.empty_args_spec t) in
   let dform_expr = create_dform_expr _loc name modes options <:expr< $lid:term_id$ >> <:expr<
      Dform.DFormPrinter
         (fun [
            { Dform.dform_term = $lid:term_id$;
              Dform.dform_items = $list_patt _loc (rewrite_type_patt _loc) items$;
              Dform.dform_printer = $lid:printer$;
              Dform.dform_buffer = $lid:buffer$;
              Dform.dform_state = _ } ->
               $code.item_item$ $lid:term_id$
          | _ ->
               raise (Invalid_argument $str:"ML dform " ^ name ^ " (generated code)"$)
         ])
   >> in
      [<:str_item<
         let $lid:term_id$ = $expr_of_term proc _loc t$ in
         let $lid:redex_id$ =
            ($rewriter_expr _loc$ .compile_redices) Rewrite_sig.Relaxed $empty_args_spec _loc$ [ $lid:term_id$ ]
         in
            $bindings_let proc _loc code dform_expr$
       >>]

(*
 * Record a resource.
 *
 * type resource_name
 *)
let define_resource proc _loc name res =
   let intermediate = "$" ^ name ^ "_resource_intermediate" in
   let inp_name = input_type name in
   let outp_name = output_type name in
   let fqn = res_fqn [proc.imp_name] name in
      proc.imp_resources
         <- (name, (<:ctyp< $lid:inp_name$ >>, fqn)) :: proc.imp_resources;
      [<:str_item< type $inp_name$ = $res.res_input$ >>;
       <:str_item< type $outp_name$ = $res.res_output$ >>;
       <:str_item< value $lid:get_resource_name name$ =
         Mp_resource.create_resource $str:fqn$ (**)
            ( $res.res_body$ : Mp_resource.resource_info $lid:inp_name$ '$intermediate$ $lid:outp_name$ ) >>]

let rec is_list_expr = function
   MLast.ExUid(_, Ploc.VaVal "[]") -> true
 | MLast.ExApp(_, MLast.ExApp(_, MLast.ExUid(_, Ploc.VaVal "::"), _), tail) ->
      is_list_expr tail
 | _ -> false

let improve_resource proc _loc { improve_name = name; improve_flag = flag; improve_expr = expr } =
   let expr' = expr_of_bnd_expr proc _loc expr in
   let improve_expr =
      if is_list_expr expr.item_item then
         impr_resource_list proc _loc flag name expr'
      else
         impr_resource proc _loc flag name expr'
   in
      [<:str_item< ($improve_expr$) >> ]

(*
 * When a parent is included, we need to open all the ancestors,
 * and we need to patch in all the resources.
 *)
let define_parent proc _loc
    { parent_name = path;
      parent_resources = nresources
    } =
   let parent_path = parent_path_expr _loc path in
   let rec find_resource name = function
      [] ->
         List.iter (fun (_, name, _) ->
               eprintf "Resource: %s@." name) proc.imp_all_resources;
         Stdpp.raise_with_loc _loc (Invalid_argument (**)
                                       (Printf.sprintf "Filter_prog.define_parent: %s: resource %s unknown" (string_of_path path) name))
    | (path, name', _) :: _ when name = name' ->
         path
    | _ :: t ->
         find_resource name t
   in
   let make_ctyp (name, _) =
      let path = find_resource name proc.imp_all_resources in
         (name, (<:ctyp< $parent_path_ctyp _loc path$ . $lid:input_type name$ >>, res_fqn path name))
   in
      proc.imp_resources <- proc.imp_resources @ (List.map make_ctyp nresources);
      match path with
         [name] -> [
            <:str_item< Mp_resource.extends_theory $str:name$ >>;
            <:str_item< $exp:refiner_expr _loc$.join_refiner $lid: local_refiner_id$ $parent_path$.$lid: refiner_id$ >>;
            refiner_let _loc;
            refiner_ignore _loc;
         ]
       | _ ->
            Stdpp.raise_with_loc _loc (Invalid_argument "Including sub-theories not implemented")

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

let define_summary_item proc _loc = function
   { item_bindings = []; item_item = item } ->
      [item]
 | { item_bindings = bnds; item_item = <:str_item< value $list:[patt,expr]$ >> } ->
      let expr = <:expr< let $list:List.map (binding_let proc _loc) (List.rev bnds)$ in $expr$ >> in
         [<:str_item< value $list:[patt, expr]$ >>]
 | { item_bindings = bnds; item_item = item } ->
      [<:str_item< value $list:List.map (binding_let proc _loc) (List.rev bnds)$ >>; item]

(*
 * This function creates str_items for the toploop.
 *)
let add_toploop_item proc _loc name ctyp =
   let expr = toploop_item_expr _loc name ctyp in
      impr_toploop proc _loc name Public expr

let get_top_ctyp proc name =
   let ctyp = List.assoc name proc.imp_toploop in
      proc.imp_toploop <- List.remove_assoc name proc.imp_toploop;
      ctyp

(*
 * An regular item.
 *)
let wrap_toploop_item proc _loc ((patt, expr) as item) =
   match patt with
      <:patt< $lid:name$ >> when List.mem_assoc name proc.imp_toploop ->
         let ctyp = get_top_ctyp proc name in
         let expr = wrap_toploop_item _loc name ctyp expr in
            (patt, expr), [add_toploop_item proc _loc name ctyp]
     | _ ->
         item, []

let wrap_toploop_items proc _loc pel =
   let pel, resources = List.split (List.map (wrap_toploop_item proc _loc) pel) in
      pel, List.flatten resources

let rec wrap_summary_items proc = function
   [] -> []
 | (item, loc)::items ->
      let items = wrap_summary_items proc items in
      begin match item with
         MLast.StVal (_loc, Ploc.VaVal rec_flag, Ploc.VaVal pel) ->
            let pel, toploop = wrap_toploop_items proc _loc pel in
            let toploop = List.map (fun x -> x, _loc) toploop in
               (<:str_item< value $opt:rec_flag$ $list:pel$ >>, _loc) :: (toploop @ items)
       | MLast.StExt (_loc, Ploc.VaVal name, ctyp, _ ) when List.mem_assoc name proc.imp_toploop ->
            (item, loc) :: (add_toploop_item proc _loc name (get_top_ctyp proc name), _loc) :: items
       | _ ->
         (item, loc)::items
      end

(*
 * A magic block computes a hash value from the definitions
 * in the block.
 *)
let define_magic_block proc _loc { magic_name = name; magic_code = stmts } =
   let index = List.fold_left Filter_hash.hash_str_item 0 stmts in
      <:str_item< value  $lid:name$ = $int:string_of_int index$ >> :: stmts

let define_prefix _loc s =
   [<:str_item< value _ = $lid:Infix.prefix_name s$ >>]

(*
 * Prolog declares the refiner and dformer.
 *)
let implem_prolog proc _loc name =
   let term_let =
      if (proc.imp_terms = [])
         && (proc.imp_meta_terms = [])
         && (proc.imp_nums = [])
         && (proc.imp_opnames = [])
      then
         []
      else
         let marshalled_terms =
            Ml_term.string_of_term_lists (**)
               (List.rev proc.imp_terms)
               (List.rev proc.imp_meta_terms)
               (List.rev proc.imp_opnames)
               (List.rev proc.imp_nums)
         in
            [<:str_item< value ($lid:global_term_var$,
                                $lid:global_meta_term_var$,
                                $lid:global_opname_var$,
                                $lid:global_num_var$) =
                            Ml_term.term_arrays_of_string $str:String.escaped marshalled_terms$>>]
   in
      <:str_item< Lm_debug.show_loading $str: "Loading theory " ^ String.capitalize name ^ "%t"$ >>
      :: <:str_item< value $lid:local_refiner_id$ = $refiner_expr _loc$ . null_refiner $str: name$ >>
      :: term_let

(*
 * Trailing declarations.
 *)
let implem_postlog proc _loc =
   match proc.imp_toploop with
      (name, _) :: _ -> raise (Failure ("Topval "^name^" not implemented (or was duplicated in the interface)"))
    | [] ->
         let name = <:expr< $str:proc.imp_name$ >> in [
            <:str_item< Mp_resource.close_theory $name$ >>;
            <:str_item< value $lid:refiner_id$ = $refiner_expr _loc$.label_refiner $lid:local_refiner_id$ $name$ >>;
            <:str_item<
               Theory.record_theory {
                  Theory.thy_name = $name$;
                  Theory.thy_group = $str:proc.imp_group$;
                  Theory.thy_groupdesc = $str:proc.imp_groupdesc$;
                  Theory.thy_refiner = $lid:refiner_id$
               }>>;
            <:str_item< Lm_debug.show_loading $str: ("Finished loading " ^ String.capitalize proc.imp_name ^ "%t") $ >>]

(*
 * Now extract the program.
 *)
let extract_str_item proc (item, loc) =
   match item with
      Rewrite ({ rw_name = name; rw_proof = Primitive _; _ } as rw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: primrw: %s%t" name eflush;
         prim_rewrite proc loc rw
    | Rewrite ({ rw_name = name; rw_proof = Derived tac; _ } as rw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: rwthm: %s%t" name eflush;
         derived_rewrite proc loc rw tac
    | Rewrite ({ rw_name = name; rw_proof = Interactive _; _ } as rw)
    | Rewrite ({ rw_name = name; rw_proof = Incomplete; _ } as rw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: rwinteractive: %s%t" name eflush;
         interactive_rewrite proc loc rw
    | InputForm { iform_name = name; _ } ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: primrw: %s%t" name eflush;
         []
    | CondRewrite ({ crw_name = name; crw_proof = Primitive _; _ } as crw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: prim condrw: %s%t" name eflush;
         prim_cond_rewrite proc loc crw
    | CondRewrite ({ crw_name = name; crw_proof = Derived tac; _ } as crw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: thm condrw: %s%t" name eflush;
         derived_cond_rewrite proc loc crw tac
    | CondRewrite ({ crw_name = name; crw_proof = Interactive _; _ } as crw)
    | CondRewrite ({ crw_name = name; crw_proof = Incomplete; _ } as crw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: interactive condrw: %s%t" name eflush;
         interactive_cond_rewrite proc loc crw
    | MLRewrite ({ mlterm_name = name; mlterm_def = Some rewrite_expr; _ } as mlrw) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: ML rewrite: %s%t" name eflush;
         define_ml_rewrite proc loc mlrw rewrite_expr
    | MLRewrite ({ mlterm_name = name; mlterm_def = None; _ }) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: ML rewrite (unimplemented): %s%t" name eflush;
         raise (Failure "Filter_prog.extract_str_item: ML rewrite is not defined")
    | Rule ({ rule_name = name; rule_proof = Primitive t; _ } as item) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: prim rule: %s%t" name eflush;
         prim_rule proc loc item t
    | Rule ({ rule_name = name; rule_proof = Derived tac; _ } as item) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: thm rule: %s%t" name eflush;
         derived_rule proc loc item tac
    | Rule ({ rule_name = name; rule_proof = Interactive _; _ } as item)
    | Rule ({ rule_name = name; rule_proof = Incomplete; _ } as item) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: interactive rule: %s%t" name eflush;
         interactive_rule proc loc item
    | MLAxiom ({ mlterm_name = name; mlterm_def = Some rule_expr; _ } as mlrule) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: ML axiom: %s%t" name eflush;
         define_ml_rule false proc loc mlrule rule_expr
    | MLAxiom ({ mlterm_name = name; mlterm_def = None; _ }) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: ML axiom unimplemented: %s%t" name eflush;
         raise (Failure "Filter_prog.extract_str_item: ML axiom is not defined")
    | DForm ({ dform_def = TermDForm expansion; _ } as df) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: dform%t" eflush;
         define_dform proc loc df expansion
    | DForm ({ dform_def = MLDForm code; _ } as df) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: dform%t" eflush;
         define_ml_dform proc loc df code
    | DForm { dform_def = NoDForm; _ } ->
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
    | Resource (name, res) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: resource: %s%t" name eflush;
         define_resource proc loc name res
    | Improve ({ improve_name = name; _ } as impr) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: improve %s with ... %t" name eflush;
         improve_resource proc loc impr
    | Parent ({ parent_name = name; _ } as parent) ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: parent: %s%t" (string_of_path name) eflush;
         define_parent proc loc parent
    | SummaryItem item ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: summary item%t" eflush;
         define_summary_item proc loc item
    | ToploopItem item ->
         raise (Invalid_argument "Filter_prog.extract_str_item: we should not have ToploopItem in str")
    | MagicBlock block ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: magic block%t" eflush;
         define_magic_block proc loc block
    | MLGramUpd (Infix s)
    | MLGramUpd (Suffix s) ->
         define_prefix loc s
    | DefineTerm (shapeclass, class_term, def)
      when is_shape_normal shapeclass ->
         define_term proc loc class_term def
    | DefineTerm _
    | DeclareTypeClass _
    | DeclareType _
    | DeclareTerm _
    | DeclareTypeRewrite _
    | Id _
    | Comment _
    | PRLGrammar _ ->
         []
    | Module _ ->
         if !debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: infix%t" eflush;
         raise (Failure "Filter_prog.extract_str_item: nested modules are not implemented")

let extract_str_item_list proc ((_, loc) as item) =
   List.map (fun x -> x, loc) (extract_str_item proc item)

(*
 * Extract a signature.
 *)
let extract_str arg sig_info info resources name group groupdesc =
   let proc =
      { imp_sig_info = sig_info;
        imp_resources = [];
        imp_toploop = implem_toploop sig_info;
        imp_arg = arg;
        imp_name = name;
        imp_group = group;
        imp_groupdesc = groupdesc;
        imp_all_resources = resources;
        imp_terms = [];
        imp_num_terms = 0;
        imp_meta_terms = [];
        imp_num_meta_terms = 0;
        imp_nums = [];
        imp_num_nums = 0;
        imp_opnames = [];
        imp_num_opnames = 0
      }
   in
   let items = info_items info in
   let name =
      match items with
         [] -> ""
       | [_, loc]
       | _ :: (_, loc) :: _ -> Filename.remove_extension (Ploc.file_name loc)
   in
   let dummy_loc = make_dummy_loc name in
   let items = Lm_list_util.flat_map (extract_str_item_list proc) items in
   let items = wrap_summary_items proc items in
   let prolog = List.map (fun item -> item, dummy_loc) (implem_prolog proc dummy_loc name) in
   let postlog = List.map (fun item -> item, dummy_loc) (implem_postlog proc dummy_loc) in
      prolog @ items @ postlog

module ProofCaches = Filter_cache.MakeCaches (Proof_convert.Convert)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
