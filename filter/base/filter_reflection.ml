(*
 * Managing reflected terms.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_debug
open Lm_symbol
open Lm_printf
open Lm_lexer
open Lm_parser
open Lm_string_set
open Lm_location

open Opname
open Term_sig
open Simple_print
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermSubst
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError

open Tactic_type

(*
 * For expanding quotations.
 *)
type parse_state =
   { parse_quotation : string -> string -> term;
     parse_opname : op_kind -> string list -> shape_param list -> int list -> Opname.opname;
     parse_param : term -> param
   }

(*
 * Opnames.
 *)
let perv_opname       = mk_opname "Perv" nil_opname

(*
 * Variables.
 *)
let var_H      = Lm_symbol.add "H"
let var_step   = Lm_symbol.add "step"

(************************************************
 * Reflection quotations.  We don't explicitly
 * use ITT opnames here, but the environment must
 * include the operators defined in the following
 * module.
 *)
module type ReflectSig =
sig
   type t

   val create              : parse_state -> t
   val mk_lambda_term      : t -> var -> term -> term
   val mk_bind_term        : t -> var -> term -> term
   val mk_bind_vec_term    : t -> term -> var -> term -> term
   val mk_rev_bind_terms   : t -> hypothesis list -> term -> term
   val mk_mk_bterm_term    : t -> term -> term -> term -> term
   val mk_operator_term    : t -> param op_param -> term
   val mk_nil_term         : t -> term
   val mk_cons_term        : t -> term -> term -> term
   val mk_list_term        : t -> term list -> term
   val mk_pair_term        : t -> term -> term -> term
   val mk_length_term      : t -> term -> term
   val mk_append_term      : t -> term -> term -> term
   val mk_append_list_term : t -> term list -> term
   val mk_sequent_term     : t -> term -> term -> term -> term
   val mk_ty_list_term     : t -> term -> term
   val mk_exists_term      : t -> var -> term -> term -> term
   val mk_equal_term       : t -> term -> term -> term -> term
   val mk_number_term      : t -> int -> term
   val mk_ty_nat           : t -> term
   val mk_subst_term       : t -> term -> term -> term
   val mk_soapply_term     : t -> term -> term list -> term
   val mk_substl_term      : t -> term -> term -> term
   val mk_capply_term      : t -> term -> var list -> term
   val mk_map_term         : t -> var -> term -> term -> term
   val mk_add_term         : t -> term -> term -> term
   val mk_ty_sovar_term    : t -> term -> term
   val mk_ty_cvar_term     : t -> term -> term
   val mk_ty_step          : t -> term
   val mk_proof_rule_term  : t -> term
   val mk_sequent_arg_term : t -> term
   val mk_provable_term    : t -> term -> term
end;;

module Reflect : ReflectSig =
struct
   type t =
      { opname_lambda    : opname Lazy.t;
        opname_bind      : opname Lazy.t;
        opname_bind_vec  : opname Lazy.t;
        opname_mk_bterm  : opname Lazy.t;
        opname_operator  : opname Lazy.t;
        term_nil         : term Lazy.t;
        opname_cons      : opname Lazy.t;
        opname_pair      : opname Lazy.t;
        opname_length    : opname Lazy.t;
        opname_append    : opname Lazy.t;
        opname_sequent   : opname Lazy.t;
        opname_list      : opname Lazy.t;
        opname_exists    : opname Lazy.t;
        opname_equal     : opname Lazy.t;
        opname_number    : opname Lazy.t;
        term_nat         : term Lazy.t;
        opname_subst     : opname Lazy.t;
        opname_substl    : opname Lazy.t;
        opname_map       : opname Lazy.t;
        opname_add       : opname Lazy.t;
        opname_sovar     : opname Lazy.t;
        opname_cvar      : opname Lazy.t;
        term_ty_step     : term Lazy.t;
        term_proof_rule  : term Lazy.t;
        sequent_arg      : term Lazy.t;
        opname_provable  : opname Lazy.t
      }

   let mk_state_opname state op params arities =
      state.parse_opname NormalKind [op] params arities

   let create state =
      { opname_lambda    = Lazy.lazy_from_fun (fun () -> mk_state_opname state "lambda"   [] [1]);
        opname_bind      = Lazy.lazy_from_fun (fun () -> mk_state_opname state "bind"     [] [1]);
        opname_bind_vec  = Lazy.lazy_from_fun (fun () -> mk_state_opname state "bind"     [] [0; 1]);
        opname_mk_bterm  = Lazy.lazy_from_fun (fun () -> mk_state_opname state "mk_bterm" [] [0; 0; 0]);
        opname_operator  = Lazy.lazy_from_fun (fun () -> mk_state_opname state "operator" [ShapeOperator] []);
        term_nil         = Lazy.lazy_from_fun (fun () -> mk_simple_term (mk_state_opname state "nil" [] []) []);
        opname_cons      = Lazy.lazy_from_fun (fun () -> mk_state_opname state "cons"     [] [0; 0]);
        opname_pair      = Lazy.lazy_from_fun (fun () -> mk_state_opname state "pair"     [] [0; 0]);
        opname_length    = Lazy.lazy_from_fun (fun () -> mk_state_opname state "length"   [] [0]);
        opname_append    = Lazy.lazy_from_fun (fun () -> mk_state_opname state "append"   [] [0; 0]);
        opname_sequent   = Lazy.lazy_from_fun (fun () -> mk_state_opname state "sequent"  [] [0; 0; 0]);
        opname_list      = Lazy.lazy_from_fun (fun () -> mk_state_opname state "list"     [] [0]);
        opname_exists    = Lazy.lazy_from_fun (fun () -> mk_state_opname state "exists"   [] [0; 1]);
        opname_equal     = Lazy.lazy_from_fun (fun () -> mk_state_opname state "equal"    [] [0; 0; 0]);
        term_nat         = Lazy.lazy_from_fun (fun () -> mk_simple_term (mk_state_opname state "nat" [] []) []);
        opname_number    = Lazy.lazy_from_fun (fun () -> mk_state_opname state "number" [ShapeNumber] []);
        opname_subst     = Lazy.lazy_from_fun (fun () -> mk_state_opname state "subst"     [] [0; 0]);
        opname_substl    = Lazy.lazy_from_fun (fun () -> mk_state_opname state "substl"    [] [0; 0]);
        opname_map       = Lazy.lazy_from_fun (fun () -> mk_state_opname state "map"       [] [1; 0]);
        opname_add       = Lazy.lazy_from_fun (fun () -> mk_state_opname state "add"       [] [0; 0]);
        opname_sovar     = Lazy.lazy_from_fun (fun () -> mk_state_opname state "SOVar"     [] [0]);
        opname_cvar      = Lazy.lazy_from_fun (fun () -> mk_state_opname state "CVar"      [] [0]);
        term_ty_step     = Lazy.lazy_from_fun (fun () -> mk_simple_term (mk_state_opname state "ProofStep" [] []) []);
        term_proof_rule  = Lazy.lazy_from_fun (fun () -> mk_simple_term (mk_state_opname state "ProofRule" [] []) []);
        sequent_arg      = Lazy.lazy_from_fun (fun () -> mk_simple_term (mk_state_opname state "sequent_arg" [] []) []);
        opname_provable  = Lazy.lazy_from_fun (fun () -> mk_state_opname state "Provable"  [] [0]);
      }

   let mk_length_term info t =
      mk_dep0_term (Lazy.force info.opname_length) t

   let mk_lambda_term info v t =
      mk_dep1_term (Lazy.force info.opname_lambda) v t

   let mk_bind_term info v t =
      mk_dep1_term (Lazy.force info.opname_bind) v t

   let mk_bind_vec_term info d v t =
      mk_dep0_dep1_term (Lazy.force info.opname_bind_vec) v d t

   let rec mk_rev_bind_terms info vars t =
      match vars with
         v :: vars ->
            let t =
               match v with
                  Hypothesis (v, _) ->
                     mk_bind_term info v t
                | Context (v, _, _) ->
                     mk_bind_vec_term info (mk_length_term info (mk_var_term v)) v t
            in
               mk_rev_bind_terms info vars t
       | [] ->
            t

   let mk_mk_bterm_term info depth op subterms =
      mk_simple_term (Lazy.force info.opname_mk_bterm) [depth; op; subterms]

   let mk_operator_term info op =
      mk_term (mk_op (Lazy.force info.opname_operator) [make_param (Operator op)]) []

   let mk_nil_term info =
      Lazy.force info.term_nil

   let mk_cons_term info t1 t2 =
      mk_simple_term (Lazy.force info.opname_cons) [t1; t2]

   let rec mk_list_term info l =
      match l with
         [] ->
            mk_nil_term info
       | v :: l ->
            mk_cons_term info v (mk_list_term info l)

   let mk_pair_term info t1 t2 =
      mk_simple_term (Lazy.force info.opname_pair) [t1; t2]

   let mk_append_term info t1 t2 =
      mk_simple_term (Lazy.force info.opname_append) [t1; t2]

   let rec mk_append_list_term info l =
      match l with
         [] ->
            mk_nil_term info
       | [t] ->
            t
       | t :: l ->
            mk_append_term info t (mk_append_list_term info l)

   let mk_sequent_term info arg hyps concl =
      mk_simple_term (Lazy.force info.opname_sequent) [arg; hyps; concl]

   let mk_ty_list_term info t =
      mk_simple_term (Lazy.force info.opname_list) [t]

   let mk_exists_term info v t1 t2 =
      mk_dep0_dep1_term (Lazy.force info.opname_exists) v t1 t2

   let mk_equal_term info t1 t2 t3 =
      mk_simple_term (Lazy.force info.opname_equal) [t3; t1; t2]

   let mk_ty_nat info =
      Lazy.force info.term_nat

   let mk_number_term info i =
      mk_term (mk_op (Lazy.force info.opname_number) [make_param (Number (Lm_num.num_of_int i))]) []

   let mk_subst_term info t1 t2 =
      mk_simple_term (Lazy.force info.opname_subst) [t1; t2]

   let rec mk_soapply_term info t args =
      match args with
         arg :: args ->
            mk_soapply_term info (mk_subst_term info t arg) args
       | [] ->
            t

   let mk_substl_term info t1 t2 =
      mk_simple_term (Lazy.force info.opname_substl) [t1; t2]

   let rec mk_capply_term info t cvars =
      match cvars with
         v :: cvars ->
            mk_capply_term info (mk_substl_term info t (mk_var_term v)) cvars
       | [] ->
            t

   let mk_map_term info v t1 t2 =
      mk_dep1_dep0_term (Lazy.force info.opname_map) v t1 t2

   let mk_add_term info t1 t2 =
      mk_dep0_dep0_term (Lazy.force info.opname_add) t1 t2

   let mk_ty_sovar_term info t =
      mk_dep0_term (Lazy.force info.opname_sovar) t

   let mk_ty_cvar_term info t =
      mk_dep0_term (Lazy.force info.opname_cvar) t

   let mk_ty_step info =
      Lazy.force info.term_ty_step

   let mk_proof_rule_term info =
      Lazy.force info.term_proof_rule

   let mk_sequent_arg_term info =
      Lazy.force info.sequent_arg

   let mk_provable_term info t =
      mk_dep0_term (Lazy.force info.opname_provable) t
end;;

(*
 * When a term is quoted, quote all its subparts.
 *)
let xquote_opname = mk_opname "xquote" perv_opname
let xunquote_opname = mk_opname "xunquote" perv_opname
let xrulequote_opname = mk_opname "xrulequote" perv_opname

let is_xunquote_term t =
   is_dep0_term xunquote_opname t

let dest_xunquote_term t =
   dest_dep0_term xunquote_opname t

let is_xquote_term t =
   is_dep0_dep0_term xquote_opname t && not (is_var_term (snd (dest_dep0_dep0_term xquote_opname t)))

let flush_hyps info appends hyps =
   match hyps with
      [] ->
         appends
    | _ ->
         Reflect.mk_list_term info (List.rev hyps) :: appends

let sweep_quote_term info depth t =
   let rec sweepdn t =
      if is_var_term t || is_so_var_term t then
         t
      else if is_context_term t then
         raise (RefineError ("Filter_grammar.sweep_quote_term", StringTermError ("contexts cannot be quoted currently", t)))
      else if is_sequent_term t then
         raise (RefineError ("Filter_grammar.sweep_quote_term", StringTermError ("sequents cannot be quoted currently", t)))
      else if is_xunquote_term t then
         dest_xunquote_term t
      else
         let param = Reflect.mk_operator_term info (opparam_of_term t) in
         let bterms = List.map wrap_bterm (dest_term t).term_terms in
         let bterms = Reflect.mk_list_term info bterms in
            Reflect.mk_mk_bterm_term info depth param bterms

   and wrap_bterm bterm =
      let { bvars = vars; bterm = bterm } = dest_bterm bterm in
         List.fold_left (fun bterm v -> Reflect.mk_bind_term info v bterm) (sweepdn bterm) (List.rev vars)
   in
      sweepdn t

let dest_xquote_term state t =
   let info = Reflect.create state in
   let depth, t = dest_dep0_dep0_term xquote_opname t in
      sweep_quote_term info depth t

(************************************************************************
 * Rule quoting.
 *)
let maybe_new_var v vars =
   if SymbolSet.mem vars v then
      new_name v (SymbolSet.mem vars)
   else
      v

let var_z = Lm_symbol.add "z"

let is_xrulequote_term t =
   is_dep0_term xrulequote_opname t && is_meta_term (dest_dep0_term xrulequote_opname t)

let sweep_rulequote_term info socvars depth t =
   let rec sweepdn socvars t =
      if is_var_term t then
         socvars, t
      else if is_so_var_term t then
         sweep_sovar_term socvars t
      else if is_context_term t then
         sweep_context_term socvars t
      else if is_sequent_term t then
         sweep_sequent_term socvars t
      else if is_xunquote_term t then
         socvars, dest_xunquote_term t
      else
         let param = Reflect.mk_operator_term info (opparam_of_term t) in
         let socvars, bterms = List.fold_left wrap_bterm (socvars, []) (dest_term t).term_terms in
         let bterms = Reflect.mk_list_term info (List.rev bterms) in
            socvars, Reflect.mk_mk_bterm_term info depth param bterms

   and wrap_bterm (socvars, bterms) bterm =
      let { bvars = vars; bterm = bterm } = dest_bterm bterm in
      let socvars, bterm = sweepdn socvars bterm in
      let bterm = List.fold_left (fun bterm v -> Reflect.mk_bind_term info v bterm) bterm (List.rev vars) in
         socvars, bterm :: bterms

   and sweep_list socvars tl =
      let socvars, tl =
         List.fold_left (fun (socvars, tl) t ->
               let socvars, t = sweepdn socvars t in
                  socvars, t :: tl) (socvars, []) tl
      in
         socvars, List.rev tl

   and sweep_sovar_term socvars t =
      let x, cargs, args = dest_so_var t in
      let arity = List.length args in
      let socvars, args = sweep_list socvars args in
      let socvars = SymbolTable.add socvars x (false, cargs, arity) in
      let t = Reflect.mk_capply_term info (mk_var_term x) cargs in
      let t = Reflect.mk_soapply_term info t args in
         socvars, t

   and sweep_sequent_context_term socvars vars x cargs args =
      let arity = List.length args in
      let socvars, args = sweep_list socvars args in
      let socvars = SymbolTable.add socvars x (true, cargs, arity) in
      let t =
         match vars, cargs, args with
            [], [], [] ->
               mk_var_term x
          | _ ->
               let fv = List.fold_left SymbolSet.add (free_vars_terms args) cargs in
               let v_z = maybe_new_var var_z fv in
               let t = Reflect.mk_capply_term info (mk_var_term v_z) cargs in
               let t = Reflect.mk_soapply_term info t args in
               let t = Reflect.mk_rev_bind_terms info vars t in
               let t = Reflect.mk_map_term info v_z t (mk_var_term x) in
                  t
      in
         socvars, t

   and sweep_context_term socvars t =
      raise (RefineError ("sweep_rulequote_term", StringTermError ("contexts are not supported currently", t)))

   and sweep_sequent_term socvars t =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_concl = concl
          } = explode_sequent t
      in
      let socvars, arg = sweepdn socvars arg in
      let socvars, vars, appends, hyps =
         SeqHyp.fold (fun (socvars, vars, appends, hyps) _ h ->
               match h with
                  Hypothesis (x, t) ->
                     let socvars, t = sweepdn socvars t in
                     let hyp = Reflect.mk_rev_bind_terms info vars t in
                        socvars, h :: vars, appends, hyp :: hyps

                | Context (x, cargs, args) ->
                     let socvars, t = sweep_sequent_context_term socvars vars x cargs args in
                     let appends = flush_hyps info appends hyps in
                     let appends = t :: appends in
                        socvars, h :: vars, appends, []) (socvars, [], [], []) hyps
      in
      let appends = flush_hyps info appends hyps in
      let hyps = Reflect.mk_append_list_term info (List.rev appends) in
      let socvars, concl = sweepdn socvars concl in
      let concl = Reflect.mk_rev_bind_terms info vars concl in
      let t = Reflect.mk_sequent_term info arg hyps concl in
         socvars, t
   in
      sweepdn socvars t

(*
 * Sort the free second-order and context variables in order of dependencies.
 *)
let sort_socvars socvars =
   let rec step socvars l v =
      if SymbolTable.mem socvars v then
         let b, cargs, arity = SymbolTable.find socvars v in
         let socvars = SymbolTable.remove socvars v in
         let socvars, l = step_list socvars l cargs in
            socvars, (v, b, cargs, arity) :: l
      else
         socvars, l
   and step_list socvars l vars =
      match vars with
         v :: vars ->
            let socvars, l = step socvars l v in
               step_list socvars l vars
       | [] ->
            socvars, l
   in
   let rec loop socvars l =
      if SymbolTable.is_empty socvars then
         l
      else
         let v, _ = SymbolTable.choose socvars in
         let socvars, l = step socvars l v in
            loop socvars l
   in
      loop socvars []

(*
 * Existentially quantify the free second-order and context variables.
 *)
let rec quantify_socvars info socvars t =
   match socvars with
      (v, b, cargs, arity) :: socvars ->
         let len = Reflect.mk_number_term info arity in
         let len =
            List.fold_left (fun len v ->
                  Reflect.mk_add_term info (Reflect.mk_length_term info (mk_var_term v)) len) len cargs
         in
         let ty =
            if b then
               Reflect.mk_ty_cvar_term info len
            else
               Reflect.mk_ty_sovar_term info len
         in
         let t = Reflect.mk_exists_term info v ty t in
            quantify_socvars info socvars t
    | [] ->
         t

(*
 * Turn the socvars into wf subgoals.
 *)
let mk_socvar_wf_assum info h_v (v, b, cargs, arity) =
   let len = Reflect.mk_number_term info arity in
   let len =
      List.fold_left (fun len v ->
            Reflect.mk_add_term info (Reflect.mk_length_term info (mk_var_term v)) len) len cargs
   in
   let ty =
      if b then
         Reflect.mk_ty_cvar_term info len
      else
         Reflect.mk_ty_sovar_term info len
   in
   let v = mk_var_term v in
   let t = Reflect.mk_equal_term info v v ty in

   (* Make a sequent *)
   let h = Context (h_v, [], []) in
   let info =
      { sequent_args = Reflect.mk_sequent_arg_term info;
        sequent_hyps = SeqHyp.singleton h;
        sequent_concl = t
      }
   in
   let t = mk_sequent_term info in
      t

let mk_socvar_wf_assums info h_v socvars =
   List.map (mk_socvar_wf_assum info h_v) socvars

(*
 * Quote the rule.
 *)
let dest_xrulequote_term_inner state t =
   let info = Reflect.create state in
   let t, _, _ = mterms_of_parsed_mterms (fun _ -> true) t [] in

   (* New variables to work with *)
   let v_step = maybe_new_var var_step (free_vars_mterm t) in

   (* The depth is always 0 *)
   let d = Reflect.mk_number_term info 0 in

   (* Convert the terms in the rule *)
   let premises, goal = unzip_mfunction t in
   let socvars, premises =
      List.fold_left (fun (socvars, premises) (_, _, t) ->
            let socvars, t = sweep_rulequote_term info socvars d t in
            let premises = t :: premises in
               socvars, premises) (SymbolTable.empty, []) premises
   in
   let socvars, goal = sweep_rulequote_term info socvars d goal in
   let premises = Reflect.mk_list_term info (List.rev premises) in

   (* The inner term is an equality *)
   let ty_step = Reflect.mk_ty_step info in
   let t = Reflect.mk_pair_term info premises goal in
   let t = Reflect.mk_equal_term info (mk_var_term v_step) t ty_step in

   (* Quantify over the free context and second-order variables *)
   let socvars = sort_socvars socvars in
   let t = quantify_socvars info socvars t in

   (* The entire thing is a function that takes a step, and checks it *)
   let t = Reflect.mk_lambda_term info v_step t in
      t

let dest_xrulequote_term_raw state t =
   try
      let t = meta_term_of_term t in
         dest_xrulequote_term_inner state t
   with
      RefineError (s, err) ->
         raise (RefineForceError ("dest_xrulequote_term", s, err))

let dest_xrulequote_term state t =
   try
      let t = dest_dep0_term xrulequote_opname t in
      let t = meta_term_of_term t in
         dest_xrulequote_term_inner state t
   with
      RefineError (s, err) ->
         raise (RefineForceError ("dest_xrulequote_term", s, err))

(*
 * Build the wf thm.
 *
 * This has the form
 *
 *    <H> >- t IN ProofRule
 *)
let mk_rule_wf_thm state t =
   let info = Reflect.create state in
   let ty = Reflect.mk_proof_rule_term info in
   let t = Reflect.mk_equal_term info t t ty in
   let h = Context (var_H, [], []) in
   let info =
      { sequent_args = Reflect.mk_sequent_arg_term info;
        sequent_hyps = SeqHyp.singleton h;
        sequent_concl = t
      }
   in
   let t = mk_sequent_term info in
      MetaTheorem t

(*
 * Build the derived form.
 *     <H> >- ...
 *     <H> >- Provable{premise1}
 *     ...
 *     <H> >- Provable{premiseN}
 *     <H> >- Provable{goal}
 *)
let mk_provable_sequent_term info h_v t =
   let t = Reflect.mk_provable_term info t in
   let h = Context (h_v, [], []) in
   let info =
      { sequent_args = Reflect.mk_sequent_arg_term info;
        sequent_hyps = SeqHyp.singleton h;
        sequent_concl = t
      }
   in
      mk_sequent_term info

let mk_infer_thm state t =
   let info = Reflect.create state in

   (* The depth is always 0 *)
   let d = Reflect.mk_number_term info 0 in

   (* Convert the terms in the rule *)
   let premises, goal = unzip_mfunction t in
   let socvars, premises =
      List.fold_left (fun (socvars, premises) (_, _, t) ->
            let socvars, t = sweep_rulequote_term info socvars d t in
            let premises = t :: premises in
               socvars, premises) (SymbolTable.empty, []) premises
   in
   let socvars, goal = sweep_rulequote_term info socvars d goal in

   (* Choose a new context variable *)
   let fv = free_vars_terms (goal :: premises) in
   let h_v = Lm_symbol.new_name var_H (SymbolSet.mem fv) in

   (* Add the Provable predicates *)
   let premises = List.map (mk_provable_sequent_term info h_v) premises in
   let goal = mk_provable_sequent_term info h_v goal in

   (* Add the wf subgoals *)
   let socvars = sort_socvars socvars in
   let wf_premises = mk_socvar_wf_assums info h_v socvars in

   (* Build the sequent *)
   let mt = zip_mimplies (wf_premises @ premises) goal in
      mt

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
