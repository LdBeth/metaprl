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
open Term_ty_sig
open Term_meta_sig
open Simple_print
open Filter_shape
open Refiner.Refiner
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
     parse_opname    : op_kind -> string list -> shape_param list -> int list -> Opname.opname;
     parse_shape     : shape -> shape_class;
     parse_param     : term -> param
   }

(*
 * Opnames.
 *)
let perv_opname       = mk_opname "Perv" nil_opname

(*
 * Variables.
 *)
let var_H       = Lm_symbol.add "H"
let var_step    = Lm_symbol.add "step"
let var_witness = Lm_symbol.add "witness"
let var_none    = Lm_symbol.add "_"

(************************************************
 * Reflection quotations.  We don't explicitly
 * use ITT opnames here, but the environment must
 * include the operators defined in the following
 * module.
 *)
module type ReflectSig =
sig
   type t

   val create                 : parse_state -> t
   val mk_lambda_term         : t -> var -> term -> term
   val mk_bind_term           : t -> var -> term -> term
   val mk_bind_vec_term       : t -> term -> var -> term -> term
   val mk_rev_bind_terms      : t -> hypothesis list -> term -> term
   val mk_mk_bterm_term       : t -> term -> term -> term -> term
   val mk_mk_term_term        : t -> term -> term -> term
   val mk_operator_term       : t -> param op_param -> term
   val mk_nil_term            : t -> term
   val mk_cons_term           : t -> term -> term -> term
   val mk_list_term           : t -> term list -> term
   val mk_pair_term           : t -> term -> term -> term
   val mk_length_term         : t -> term -> term
   val mk_append_term         : t -> term -> term -> term
   val mk_append_list_term    : t -> term list -> term
   val mk_sequent_term        : t -> term -> term -> term -> term
   val mk_ty_list_term        : t -> term -> term
   val mk_exists_term         : t -> var -> term -> term -> term
   val mk_equal_term          : t -> term -> term -> term -> term
   val mk_number_term         : t -> int -> term
   val mk_ty_nat              : t -> term
   val mk_subst_term          : t -> term -> term -> term
   val mk_soapply_term        : t -> term -> term list -> term
   val mk_substl_term         : t -> term -> term -> term
   val mk_capply_term         : t -> term -> var list -> term
   val mk_map_term            : t -> var -> term -> term -> term
   val mk_add_term            : t -> term -> term -> term
   val mk_BTerm2_term         : t -> term -> term
   val mk_CVar_term           : t -> term -> term
   val mk_ProofStep_term      : t -> term -> term
   val mk_proof_step_term     : t -> term -> term -> term
   val mk_beq_proof_step_term : t -> term -> term -> term
   val mk_ProofRule_term      : t -> term -> term
   val mk_sequent_arg_term    : t -> term
   val mk_provable_term       : t -> term -> term
   val mk_Provable_term       : t -> term -> term -> term -> term
   val mk_Sequent_term        : t -> term
   val mk_meta_type_term      : t -> term
   val mk_meta_member_term    : t -> term -> term -> term
   val mk_Logic_term          : t -> term -> term
   val mk_type_term           : t -> term -> term
   val mk_assert_term         : t -> term -> term
   val mk_let_cvar_term       : t -> var -> term -> term -> int -> term -> term
   val mk_let_sovar_term      : t -> var -> term -> term -> int -> term -> term
   val mk_spread_term         : t -> term -> var -> var -> term -> term
   val mk_vsequent_term       : t -> term -> term list -> term -> term
   val mk_empty_logic_term    : t -> term
   val mk_cons_logic_term     : t -> term -> term -> term
   val mk_union_logic_term    : t -> term -> term -> term
end;;

module Reflect : ReflectSig =
struct
   (*
    * Table of opnames that we care about.
    *)
   let hash_index = ref 0

   let hash info =
      let index = !hash_index in
         incr hash_index;
         index, info

   let info_BTerm2            = hash ("BTerm",          [], [0])
   let info_CVar              = hash ("CVar",           [], [0])
   let info_Logic             = hash ("Logic",          [], [0])
   let info_ProofRule         = hash ("ProofRule",      [], [0])
   let info_ProofStep         = hash ("ProofStep",      [], [0])
   let info_Provable1         = hash ("Provable",       [], [0])
   let info_Provable3         = hash ("Provable",       [], [0; 0; 0])
   let info_Sequent           = hash ("Sequent",        [], [])
   let info_add               = hash ("add",            [], [0; 0])
   let info_append            = hash ("append",         [], [0; 0])
   let info_assert            = hash ("assert",         [], [0])
   let info_beq_proof_step    = hash ("beq_proof_step", [], [0; 0])
   let info_bind_vec          = hash ("bind",           [], [0; 1])
   let info_bind              = hash ("bind",           [], [1])
   let info_cons              = hash ("cons",           [], [0; 0])
   let info_equal             = hash ("equal",          [], [0; 0; 0])
   let info_exists            = hash ("exists",         [], [0; 1])
   let info_lambda            = hash ("lambda",         [], [1])
   let info_length            = hash ("length",         [], [0])
   let info_let_cvar          = hash ("let_cvar",       [], [0; 0; 0; 1])
   let info_let_sovar         = hash ("let_sovar",      [], [0; 0; 0; 1])
   let info_list              = hash ("list",           [], [0])
   let info_map               = hash ("map",            [], [1; 0])
   let info_meta_member       = hash ("meta_member",    [], [0; 0])
   let info_meta_type         = hash ("meta_type",      [], [])
   let info_mk_bterm          = hash ("mk_bterm",       [], [0; 0; 0])
   let info_mk_term           = hash ("mk_term",        [], [0; 0])
   let info_nat               = hash ("nat",            [], [])
   let info_nil               = hash ("nil",            [], [])
   let info_number            = hash ("number",         [ShapeNumber], [])
   let info_operator          = hash ("operator",       [ShapeOperator], [])
   let info_pair              = hash ("pair",           [], [0; 0])
   let info_proof_step        = hash ("proof_step",     [], [0; 0])
   let info_sequent           = hash ("sequent",        [], [0; 0; 0])
   let info_sequent_arg       = hash ("sequent_arg",    [], [])
   let info_spread            = hash ("spread",         [], [0; 2])
   let info_subst             = hash ("subst",          [], [0; 0])
   let info_substl            = hash ("substl",         [], [0; 0])
   let info_type              = hash ("type",           [], [0])
   let info_vsequent          = hash ("vsequent",       [], [0])
   let info_empty_logic       = hash ("empty_logic",    [], [])
   let info_cons_logic        = hash ("cons_logic",     [], [0; 0])
   let info_union_logic       = hash ("union_logic",    [], [0; 0])

   (*
    * Lazy opname creation.
    *)
   type t =
      { info_state   : parse_state;
        info_opnames : opname option array
      }

   let create state =
      { info_state   = state;
        info_opnames = Array.create !hash_index None
      }

   let find_opname info (id, data) =
      match info.info_opnames.(id) with
         Some opname ->
            opname
       | None ->
            let op, params, arities = data in
            let opname = info.info_state.parse_opname NormalKind [op] params arities in
               info.info_opnames.(id) <- Some opname;
               opname

   (************************************************************************
    * Actual constructors.
    *)
   let mk_length_term info t =
      mk_dep0_term (find_opname info info_length) t

   let mk_lambda_term info v t =
      mk_dep1_term (find_opname info info_lambda) v t

   let mk_bind_term info v t =
      mk_dep1_term (find_opname info info_bind) v t

   let mk_bind_vec_term info d v t =
      mk_dep0_dep1_term (find_opname info info_bind_vec) v d t

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

   let mk_mk_term_term info op subterms =
      mk_simple_term (find_opname info info_mk_term) [op; subterms]

   let mk_mk_bterm_term info depth op subterms =
      mk_simple_term (find_opname info info_mk_bterm) [depth; op; subterms]

   let mk_operator_term info op =
      mk_term (mk_op (find_opname info info_operator) [make_param (Operator op)]) []

   let mk_nil_term info =
      mk_simple_term (find_opname info info_nil) []

   let mk_cons_term info t1 t2 =
      mk_simple_term (find_opname info info_cons) [t1; t2]

   let rec mk_list_term info l =
      match l with
         [] ->
            mk_nil_term info
       | v :: l ->
            mk_cons_term info v (mk_list_term info l)

   let mk_pair_term info t1 t2 =
      mk_simple_term (find_opname info info_pair) [t1; t2]

   let mk_append_term info t1 t2 =
      mk_simple_term (find_opname info info_append) [t1; t2]

   let rec mk_append_list_term info l =
      match l with
         [] ->
            mk_nil_term info
       | [t] ->
            t
       | t :: l ->
            mk_append_term info t (mk_append_list_term info l)

   let mk_sequent_term info arg hyps concl =
      mk_simple_term (find_opname info info_sequent) [arg; hyps; concl]

   let mk_ty_list_term info t =
      mk_simple_term (find_opname info info_list) [t]

   let mk_exists_term info v t1 t2 =
      mk_dep0_dep1_term (find_opname info info_exists) v t1 t2

   let mk_equal_term info t1 t2 t3 =
      mk_simple_term (find_opname info info_equal) [t3; t1; t2]

   let mk_ty_nat info =
      mk_simple_term (find_opname info info_nat) []

   let mk_number_term info i =
      mk_term (mk_op (find_opname info info_number) [make_param (Number (Lm_num.num_of_int i))]) []

   let mk_subst_term info t1 t2 =
      mk_simple_term (find_opname info info_subst) [t1; t2]

   let rec mk_soapply_term info t args =
      match args with
         arg :: args ->
            mk_soapply_term info (mk_subst_term info t arg) args
       | [] ->
            t

   let mk_substl_term info t1 t2 =
      mk_simple_term (find_opname info info_substl) [t1; t2]

   let rec mk_capply_term info t cvars =
      match cvars with
         v :: cvars ->
            mk_capply_term info (mk_substl_term info t (mk_var_term v)) cvars
       | [] ->
            t

   let mk_map_term info v t1 t2 =
      mk_dep1_dep0_term (find_opname info info_map) v t1 t2

   let mk_add_term info t1 t2 =
      mk_dep0_dep0_term (find_opname info info_add) t1 t2

   let mk_BTerm2_term info t =
      mk_dep0_term (find_opname info info_BTerm2) t

   let mk_CVar_term info t =
      mk_dep0_term (find_opname info info_CVar) t

   let mk_ProofStep_term info t =
      mk_dep0_term (find_opname info info_ProofStep) t

   let mk_proof_step_term info t1 t2 =
      mk_dep0_dep0_term (find_opname info info_proof_step) t1 t2

   let mk_beq_proof_step_term info t1 t2 =
      mk_dep0_dep0_term (find_opname info info_beq_proof_step) t1 t2

   let mk_ProofRule_term info t =
      mk_dep0_term (find_opname info info_ProofRule) t

   let mk_sequent_arg_term info =
      mk_simple_term (find_opname info info_sequent_arg) []

   let mk_Sequent_term info =
      mk_simple_term (find_opname info info_Sequent) []

   let mk_Logic_term info t =
      mk_dep0_term (find_opname info info_Logic) t

   let mk_provable_term info t =
      mk_dep0_term (find_opname info info_Provable1) t

   let mk_Provable_term info t1 t2 t3 =
      mk_dep0_dep0_dep0_term (find_opname info info_Provable3) t1 t2 t3

   let mk_meta_member_term info t1 t2 =
      mk_dep0_dep0_term (find_opname info info_meta_member) t1 t2

   let mk_meta_type_term info =
      mk_simple_term (find_opname info info_meta_type) []

   let mk_type_term info t =
      mk_dep0_term (find_opname info info_type) t

   let mk_assert_term info t =
      mk_dep0_term (find_opname info info_assert) t

   let mk_dep0_dep0_dep0_dep1_term opname t1 t2 t3 v t4 =
      let bterms =
         [mk_simple_bterm t1;
          mk_simple_bterm t2;
          mk_simple_bterm t3;
          mk_bterm [v] t4]
      in
      let op = mk_op opname [] in
         mk_term op bterms

   let mk_let_cvar_term info v t1 t2 t3 t4 =
      let t3 = mk_number_term info t3 in
         mk_dep0_dep0_dep0_dep1_term (find_opname info info_let_cvar) t1 t2 t3 v t4

   let mk_let_sovar_term info v t1 t2 t3 t4 =
      let t3 = mk_number_term info t3 in
         mk_dep0_dep0_dep0_dep1_term (find_opname info info_let_sovar) t1 t2 t3 v t4

   let mk_spread_term info t1 v1 v2 t2 =
      mk_dep0_dep2_term (find_opname info info_spread) v1 v2 t1 t2

   let mk_vsequent_term info arg hyps concl =
      let seq =
         { sequent_args = mk_dep0_term (find_opname info info_vsequent) arg;
           sequent_hyps = SeqHyp.of_list (List.map (fun t -> Hypothesis (var_none, t)) hyps);
           sequent_concl = concl
         }
      in
         TermMan.mk_sequent_term seq

   let mk_empty_logic_term info =
      mk_simple_term (find_opname info info_empty_logic) []

   let mk_cons_logic_term info t1 t2 =
      mk_dep0_dep0_term (find_opname info info_cons_logic) t1 t2

   let mk_union_logic_term info t1 t2 =
      mk_dep0_dep0_term (find_opname info info_union_logic) t1 t2
end;;

type parse_info = Reflect.t

let create_parse_info = Reflect.create

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

let is_xquote0_term t =
   is_dep0_term xquote_opname t && not (is_var_term (dest_dep0_term xquote_opname t))

let sweep_quote0_term info t =
   let rec sweepdn t =
      if is_var_term t || is_so_var_term t then
         t
      else if is_context_term t then
         raise (RefineError ("Filter_grammar.sweep_quote_term", StringTermError ("contexts cannot be quoted currently", t)))
      else if is_sequent_term t then
         sweep_sequent_term t
      else if is_xunquote_term t then
         dest_xunquote_term t
      else
         let param = Reflect.mk_operator_term info (opparam_of_term t) in
         let bterms = List.map wrap_bterm (dest_term t).term_terms in
         let bterms = Reflect.mk_list_term info bterms in
            Reflect.mk_mk_term_term info param bterms

   and wrap_bterm bterm =
      let { bvars = vars; bterm = bterm } = dest_bterm bterm in
         List.fold_left (fun bterm v -> Reflect.mk_bind_term info v bterm) (sweepdn bterm) (List.rev vars)

   and sweep_sequent_term t =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_concl = concl
          } = explode_sequent t
      in
      let () =
         if SeqHyp.length hyps <> 0 then
            raise (RefineError ("sweep_quote_term", StringError "hypotheses not supported yet"))
      in
      let nil = Reflect.mk_nil_term info in
      let arg = sweepdn arg in
      let concl = sweepdn concl in
      let t = Reflect.mk_sequent_term info arg nil concl in
         t
   in
      sweepdn t

let dest_xquote0_term state t =
   let info = Reflect.create state in
   let t = dest_dep0_term xquote_opname t in
      sweep_quote0_term info t

(*
 * When a term is quoted, quote all its subparts.
 *)
let is_xunquote_term t =
   is_dep0_term xunquote_opname t

let dest_xunquote_term t =
   dest_dep0_term xunquote_opname t

let sweep_quote_term info depth t =
   let rec sweepdn t =
      if is_var_term t || is_so_var_term t then
         t
      else if is_context_term t then
         raise (RefineError ("Filter_grammar.sweep_quote_term", StringTermError ("contexts cannot be quoted currently", t)))
      else if is_sequent_term t then
         sweep_sequent_term t
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

   and sweep_sequent_term t =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_concl = concl
          } = explode_sequent t
      in
      let () =
         if SeqHyp.length hyps <> 0 then
            raise (RefineError ("sweep_quote_term", StringError "hypotheses not supported yet"))
      in
      let nil = Reflect.mk_nil_term info in
      let arg = sweepdn arg in
      let concl = sweepdn concl in
      let t = Reflect.mk_sequent_term info arg nil concl in
         t
   in
      sweepdn t

let dest_xquote_term state t =
   let info = Reflect.create state in
   let depth, t = dest_dep0_dep0_term xquote_opname t in
      sweep_quote_term info depth t

let quote_term info depth t =
   let depth = Reflect.mk_number_term info depth in
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

let sweep_rulequote_term info socvars t =
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
            socvars, Reflect.mk_mk_term_term info param bterms

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
      let nil = Reflect.mk_nil_term info in
      let socvars, arg = sweepdn socvars arg in
      let socvars, vars, hyps =
         SeqHyp.fold (fun (socvars, vars, hyps) _ h ->
               match h with
                  Hypothesis (x, t) ->
                     let socvars, t = sweepdn socvars t in
                     let hyp = Reflect.mk_rev_bind_terms info vars t in
                     let hyp = Reflect.mk_cons_term info hyp nil in
                        socvars, h :: vars, hyp :: hyps

                | Context (x, cargs, args) ->
                     let socvars, t = sweep_sequent_context_term socvars vars x cargs args in
                        socvars, h :: vars, t :: hyps) (socvars, [], []) hyps
      in
      let socvars, concl = sweepdn socvars concl in
      let concl = Reflect.mk_rev_bind_terms info vars concl in
      let t = Reflect.mk_vsequent_term info arg (List.rev hyps) concl in
         socvars, t
   in
      sweepdn socvars t

(*
 * Sort the free second-order and context variables in order of dependencies.
 *)
let sort_socvars socvars =
   let rec step socvars soindex cindex l v =
      if SymbolTable.mem socvars v then
         let b, cargs, arity = SymbolTable.find socvars v in
         let socvars = SymbolTable.remove socvars v in
         let i, soindex, cindex =
            if b then
               cindex, soindex, succ cindex
            else
               soindex, succ soindex, cindex
         in
         let socvars, soindex, cindex, l = step_list socvars soindex cindex l cargs in
            socvars, soindex, cindex, (v, i, b, cargs, arity) :: l
      else
         socvars, soindex, cindex, l
   and step_list socvars soindex cindex l vars =
      match vars with
         v :: vars ->
            let socvars, soindex, cindex, l = step socvars soindex cindex l v in
               step_list socvars soindex cindex l vars
       | [] ->
            socvars, soindex, cindex, l
   in
   let rec loop socvars soindex cindex l =
      if SymbolTable.is_empty socvars then
         l
      else
         let v, _ = SymbolTable.choose socvars in
         let socvars, soindex, cindex, l = step socvars soindex cindex l v in
            loop socvars soindex cindex l
   in
      loop socvars 0 0 []

(*
 * Existentially quantify the free second-order and context variables.
 *)
let rec quantify_socvars info t_witness socvars t =
   match socvars with
      (v, i, b, cargs, arity) :: socvars ->
         let len = Reflect.mk_number_term info arity in
         let len =
            List.fold_left (fun len v ->
                  Reflect.mk_add_term info (Reflect.mk_length_term info (mk_var_term v)) len) len cargs
         in
         let t =
            if b then
               Reflect.mk_let_cvar_term info v len t_witness i t
            else
               Reflect.mk_let_sovar_term info v len t_witness i t
         in
            quantify_socvars info t_witness socvars t
    | [] ->
         t

(*
 * Turn the socvars into wf subgoals.
 *)
let mk_socvar_wf_assum info h_v (v, _, b, cargs, arity) =
   let len = Reflect.mk_number_term info arity in
   let len =
      List.fold_left (fun len v ->
            Reflect.mk_add_term info (Reflect.mk_length_term info (mk_var_term v)) len) len cargs
   in
   let ty =
      if b then
         Reflect.mk_CVar_term info len
      else
         Reflect.mk_BTerm2_term info len
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
   let fv = free_vars_mterm t in
   let v_step = maybe_new_var var_step fv in
   let v_witness = maybe_new_var var_witness (SymbolSet.add fv v_step) in
   let t_step = mk_var_term v_step in
   let t_witness = mk_var_term v_witness in

   (* Convert the terms in the rule *)
   let premises, goal = unzip_mfunction t in
   let socvars, premises =
      List.fold_left (fun (socvars, premises) (_, _, t) ->
            let socvars, t = sweep_rulequote_term info socvars t in
            let premises = t :: premises in
               socvars, premises) (SymbolTable.empty, []) premises
   in
   let socvars, goal = sweep_rulequote_term info socvars goal in
   let premises = Reflect.mk_list_term info (List.rev premises) in

   (* The inner term is an equality *)
   let t = Reflect.mk_proof_step_term info premises goal in
   let t = Reflect.mk_beq_proof_step_term info (mk_var_term v_step) t in

   (* Quantify over the free context and second-order variables *)
   let socvars = sort_socvars socvars in
   let t = quantify_socvars info t_witness socvars t in

   (* The entire thing is a function that takes a step, and checks it *)
   let t = Reflect.mk_spread_term info t_step v_step v_witness t in
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
let mk_rule_wf_thm info t =
   let seq = Reflect.mk_Sequent_term info in
   let ty = Reflect.mk_ProofRule_term info seq in
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
 * Build the wf thm for a logic.
 *
 * This has the form
 *
 *    <H> >- t IN Logic
 *)
let mk_logic_wf_thm info t =
   let seq = Reflect.mk_Sequent_term info in
   let ty = Reflect.mk_Logic_term info seq in
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

let mk_infer_thm info t =
   (* Convert the terms in the rule *)
   let premises, goal = unzip_mfunction t in
   let socvars, premises =
      List.fold_left (fun (socvars, premises) (_, _, t) ->
            let socvars, t = sweep_rulequote_term info socvars t in
            let premises = t :: premises in
               socvars, premises) (SymbolTable.empty, []) premises
   in
   let socvars, goal = sweep_rulequote_term info socvars goal in

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

(*
 * Build a type checking rule.
 *)
let mk_type_check_thm info quote =
   (* Get the parts of ther quoted term *)
   let { ty_term   = t;
         ty_opname = opname;
         ty_params = params;
         ty_bterms = bterms;
         ty_type   = ty
       } = quote
   in
   let () =
      if params <> [] then
         raise (RefineError ("mk_type_check_thm", StringTermError ("terms with parameters are not implemented yet", t)))
   in

   (* Sequent arg *)
   let q_arg = Reflect.mk_meta_type_term info in
(*
   let q_arg = quote_term info 0 arg in
*)

   (* Capture won't happen because we are constructing the terms *)
   let h_v = var_H in

   (* Premises *)
   let premises, bterms, _ =
      List.fold_left (fun (premises, bterms, index) bterm ->
            let { ty_bvars = bvars;
                  ty_bterm = ty
                } = bterm
            in

            (* Build the sequent *)
            let hyps, vars, _ =
               List.fold_left (fun (hyps, vars, index) ty ->
                     let v = Lm_symbol.make "x" index in
                     let vars = v :: vars in
                     let hyps = Hypothesis (v, ty) :: hyps in
                        hyps, vars, succ index) ([Context (h_v, [], [])], [], 1) bvars
            in
            let bvars = List.fold_left (fun bvars v -> mk_var_term v :: bvars) [] vars in
            let t = mk_so_var_term (Lm_symbol.make "b" index) [default_contexts] bvars in
            let concl = Reflect.mk_meta_member_term info t ty in
            let seq_info =
               { sequent_args = q_arg;
                 sequent_hyps = SeqHyp.of_list (List.rev hyps);
                 sequent_concl = concl
               }
            in
            let premise = mk_sequent_term seq_info in

            (* Build the bterm *)
            let bterm = mk_bterm (List.rev vars) t in
               premise :: premises, bterm :: bterms, succ index) ([], [], 1) bterms
   in

   (* Build the goal sequent *)
   let t = mk_term (mk_op opname []) (List.rev bterms) in
   let t = Reflect.mk_meta_member_term info t ty in
   let h = Context (h_v, [], []) in
   let seq_info =
      { sequent_args = q_arg;
        sequent_hyps = SeqHyp.singleton h;
        sequent_concl = t
      }
   in
   let goal = mk_sequent_term seq_info in

   (* Zip the rule *)
   let mt = zip_mimplies (List.rev premises) goal in
      mt

(*
 * Build the term that defines a logic.
 *)
let mk_logic_info info t_logic rules var_p t_provable =
   (* The logic is a list of rules *)
   let logic = Reflect.mk_list_term info rules in

   (* The type of the logic is over Sequents *)
   let arg = Reflect.mk_sequent_arg_term info in
   let seq = Reflect.mk_Sequent_term info in
   let ty_logic = Reflect.mk_Logic_term info seq in
   let h = Context (var_H, [], []) in
   let seq_info =
      { sequent_args = arg;
        sequent_hyps = SeqHyp.singleton h;
        sequent_concl = Reflect.mk_equal_term info t_logic t_logic ty_logic
      }
   in
   let logic_wf = MetaTheorem (mk_sequent_term seq_info) in

   (* The provable judgment *)
   let provable = Reflect.mk_Provable_term info seq t_logic var_p in

   (* The provable wf rule *)
   let h = Context (var_H, [], []) in
   let concl = Reflect.mk_type_term info t_provable in
   let assum_info =
      { sequent_args = arg;
        sequent_hyps = SeqHyp.singleton h;
        sequent_concl = Reflect.mk_equal_term info var_p var_p seq
      }
   in
   let goal_info =
      { sequent_args = arg;
        sequent_hyps = SeqHyp.singleton h;
        sequent_concl = concl
      }
   in
   let provable_wf = zip_mimplies [mk_sequent_term assum_info] (mk_sequent_term goal_info) in
      logic, logic_wf, provable, provable_wf

(*
 * Logic construction.
 *)
let mk_empty_logic_term info =
   Reflect.mk_empty_logic_term info

let mk_cons_logic_term info t_rule t_logic =
   Reflect.mk_cons_logic_term info t_rule t_logic

let mk_union_logic_term info t_logic1 t_logic2 =
   Reflect.mk_union_logic_term info t_logic1 t_logic2

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
