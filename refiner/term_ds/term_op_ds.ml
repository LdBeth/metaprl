(*
 * Standard operations on terms.
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Authors: Alexey Nogin
 *)

INCLUDE "refine_error.mlh"

open Lm_symbol

open Opname
open Refine_error_sig
open Term_sig
open Term_ds_sig
open Term_ds

module TermOp
   (Term : TermDsSig with module TermTypes = TermType)
   (RefineError : RefineErrorSig
                  with type Types.level_exp = TermType.level_exp
                  with type Types.param = TermType.param
                  with type Types.term = TermType.term
                  with type Types.bound_term = TermType.bound_term)
=
struct
   open RefineError
   open TermType
   open Term
   module OpTypes = TermType

   (*
    * Terms with no subterms.
    *)
   let is_no_subterms_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = []
           } when Opname.eq opname' opname -> true
    | _ -> false

   (*
    * Terms with one subterm
    *)
   let is_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt]
           } when Opname.eq opname' opname -> bt.bvars = []
    | _ -> false

   let mk_dep0_term opname t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [mk_simple_bterm t]}}

   let dest_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
           term_terms = [bt]
         } when Opname.eq opname' opname -> dest_simple_bterm bt
    | _ -> REF_RAISE(RefineError ("dest_dep0_term", TermMatchError (t, "bad arity")))

   let one_subterm t = match get_core t with
      Term { term_terms = [bt]; _ }  -> dest_simple_bterm bt
    | _ -> REF_RAISE(RefineError ("one_subterm", TermMatchError (t, "bad arity")))

   let one_subterm_opname opname t = match get_core t with
      Term { term_op = { op_name = opname'; _ }; term_terms = [bt] }
         when Opname.eq opname' opname  ->
            dest_simple_bterm bt
    | _ -> REF_RAISE(RefineError ("one_subterm", TermMatchError (t, "bad arity")))

   (*
    * Terms with two subterms.
    *)
   let is_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt1; bt2]
           } when Opname.eq opname' opname ->
         bt1.bvars = [] && bt2.bvars = []
    | _ -> false

   let mk_dep0_dep0_term opname t1 t2 =
      core_term (
         Term {
               term_op = { op_name = opname; op_params = [] };
               term_terms = [mk_simple_bterm t1; mk_simple_bterm t2]})

   let dest_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] }; term_terms = [bt1 ; bt2 ] }
         when Opname.eq opname' opname ->
            dest_simple_bterm bt1, dest_simple_bterm bt2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let two_subterms t = match get_core t with
      Term { term_terms = [bt1; bt2]; _ } ->
         dest_simple_bterm bt1, dest_simple_bterm bt2
    | _ -> REF_RAISE(RefineError ("two_subterms", TermMatchError (t, "bad arity")))

   let two_subterms_opname opname t = match get_core t with
      Term { term_op = { op_name = opname'; _ }; term_terms = [bt1; bt2]}
        when Opname.eq opname' opname ->
         dest_simple_bterm bt1, dest_simple_bterm bt2
    | _ -> REF_RAISE(RefineError ("two_subterms", TermMatchError (t, "bad arity")))

   (*
    * Terms with three subterms.
    *)
   let is_dep0_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = ([_; _; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

   let mk_dep0_dep0_dep0_term opname t1 t2 t3 =
      core_term (
         Term {
            term_op = { op_name = opname; op_params = [] };
            term_terms = [mk_simple_bterm t1; mk_simple_bterm t2; mk_simple_bterm t3]})

   let dest_dep0_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt1; bt2; bt3]
           } when Opname.eq opname' opname ->
         dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep0_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Terms with four subterms.
    *)
   let is_dep0_dep0_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt1; bt2; bt3; bt4]
           } when Opname.eq opname' opname ->
         bt1.bvars = [] && bt2.bvars = [] && bt3.bvars = [] && bt4.bvars = []
    | _ -> false

   let mk_dep0_dep0_dep0_dep0_term opname t1 t2 t3 t4 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t1; mk_simple_bterm t2; mk_simple_bterm t3; mk_simple_bterm t4]}}

   let dest_dep0_dep0_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt1; bt2; bt3; bt4]
           } when Opname.eq opname' opname ->
         dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3, dest_simple_bterm bt4
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep0_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let is_two_subterm opname t = match get_core t with
      Term { term_op = { op_name = opname'; _ };
             term_terms = ([_; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

   let is_three_subterm opname t = match get_core t with
      Term { term_op = { op_name = opname'; _ };
             term_terms = ([_; _; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

(* unused
   let is_four_subterm opname t = match get_core t with
      Term { term_op = { op_name = opname' };
             term_terms = ([_; _; _; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false
*)

   let is_five_subterm opname t = match get_core t with
      Term { term_op = { op_name = opname'; _ };
             term_terms = ([_; _; _; _; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

   let three_subterms t = match get_core t with
      Term { term_terms = [bt1; bt2; bt3]; _ } ->
         dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3
    | _ -> REF_RAISE(RefineError ("three_subterms", TermMatchError (t, "bad arity")))

   let four_subterms t = match get_core t with
      Term { term_terms = [bt1; bt2; bt3; bt4]; _ } ->
         dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3, dest_simple_bterm bt4
    | _ -> REF_RAISE(RefineError ("four_subterms", TermMatchError (t, "bad arity")))

   let five_subterms t = match get_core t with
      Term { term_terms = [bt1; bt2; bt3; bt4; bt5]; _ } ->
          dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3, dest_simple_bterm bt4, dest_simple_bterm bt5
    | _ -> REF_RAISE(RefineError ("five_subterms", TermMatchError (t, "bad arity")))

   let six_subterms t = match get_core t with
      Term { term_terms = [bt1; bt2; bt3; bt4; bt5; bt6]; _ } ->
          dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3, dest_simple_bterm bt4, dest_simple_bterm bt5, dest_simple_bterm bt6
    | _ -> REF_RAISE(RefineError ("six_subterms", TermMatchError (t, "bad arity")))

   (************************************************************************
    * Nonsimple but useful forms                                           *
    ************************************************************************)

   (*
    * One string param.
    *)
   let is_string_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _] };
             term_terms = []
           } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s] }; term_terms = [] }
         when Opname.eq opname opname' -> s
    | _ ->
         REF_RAISE(RefineError ("dest_string_term", TermMatchError (t, "not a string term")))

   let dest_string_param t = match get_core t with
      Term { term_op = { op_params = String s :: _; _ }; _ } ->
         s
    | _ ->
         REF_RAISE(RefineError ("dest_string_param", TermMatchError (t, "no string parameter")))

   let mk_string_term opname s =
      { free_vars = Vars SymbolSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [String s] }; term_terms = [] }}

   (*
    * Two string params.
    *)
   let is_string_string_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _; String _] };
             term_terms = []
           } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_string_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s1; String s2] }; term_terms = [] }
         when Opname.eq opname opname' -> s1, s2
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_term", TermMatchError (t, "not a string-pair term")))

   let mk_string_string_term opname s1 s2 =
      { free_vars = Vars SymbolSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [String s1; String s2] }; term_terms = [] }}

   (*
    * One var parameter.
    *)
   let is_var_param_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Var _] };
             term_terms = []
           } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_var_param_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Var v] }; term_terms = [] }
         when Opname.eq opname opname' -> v
    | _ ->
         REF_RAISE(RefineError ("dest_var_param_term", TermMatchError (t, "not a var param term")))

   let mk_var_param_term opname s =
      if Opname.eq opname var_opname || Opname.eq opname context_opname then
         raise (Invalid_argument "Term_op_ds.mk_var_param_term: special opnames not allowed");
      { free_vars = Vars SymbolSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [Var s] }; term_terms = [] }}

   (*
    * One var parameter.
    *)
   let is_var_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Var _] };
             term_terms = [{ bvars = []; _ }]
           } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_var_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Var v] };
             term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         v, t
    | _ ->
         REF_RAISE(RefineError ("dest_var_dep0_term", TermMatchError (t, "not a var param term")))

   let dest_var_dep0_any_term t = match get_core t with
      Term { term_op = { op_params = [Var v]; _ };
             term_terms = [{ bvars = []; bterm = t }]
      } ->
         v, t
    | _ ->
         REF_RAISE(RefineError ("dest_var_dep0_term", TermMatchError (t, "not a var param term")))

   let mk_var_dep0_term opname v t =
      { free_vars = VarsDelayed;
        core = Term { term_op = { op_name = opname; op_params = [Var v] };
                      term_terms = [mk_simple_bterm t]
               }
      }

   let is_var_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Var _] };
             term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
           } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_var_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Var v] };
             term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } when Opname.eq opname opname' ->
         v, t1, t2
    | _ ->
         REF_RAISE(RefineError ("dest_var_dep0_dep0_term", TermMatchError (t, "not a var param term")))

   let mk_var_dep0_dep0_term opname s t1 t2 =
      { free_vars = VarsDelayed;
        core = Term { term_op = { op_name = opname; op_params = [Var s] };
                      term_terms = [mk_simple_bterm t1; mk_simple_bterm t2]
               }
      }

   (*
    * One string parameter, and one simple subterm.
    *)
   let is_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _] };
             term_terms = [{ bvars = []; _ }]
           } -> Opname.eq opname opname'
    | _ -> false

   let dest_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s] };
             term_terms = [bt]
           } when Opname.eq opname opname' -> s, dest_simple_bterm bt
    | _ -> REF_RAISE(RefineError ("dest_string_dep0_term", TermMatchError (t, "bad arity")))

   let mk_string_dep0_term opname s t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [mk_simple_bterm t] }}

   (*
    * Two string parameters, and one simple subterm.
    *)
   let is_string_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _; String _] };
             term_terms = [{ bvars = []; _ }]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_string_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s1; String s2] };
             term_terms = [bt]
           } when Opname.eq opname opname' ->
         s1, s2, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_string_dep0_any_term t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s1; String s2] };
             term_terms = [bt]
           } ->
         s1, s2, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_string_dep0_term opname s1 s2 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [mk_simple_bterm t] }}

   (*
    * One number parameter and one subterm.
    *)
   let is_number_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _] };
             term_terms = [ { bvars = []; _ } ]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_number_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number s1] };
             term_terms = [bt]
           } when Opname.eq opname opname' ->
         s1, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_dep0_any_term t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number s1] };
             term_terms = [bt]
           } ->
         s1, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_dep0_term opname s1 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [Number s1] };
           term_terms = [mk_simple_bterm t]}}

   let is_number_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _] };
             term_terms = [ { bvars = [_]; _ } ]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_number_dep1_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [Number s1] };
                term_terms = [ { bvars = [v]; bterm = t } ]
              } when Opname.eq opname opname' ->
            s1, v, t
       | _ ->
            REF_RAISE(RefineError ("dest_number_dep1_term", TermMatchError (t, "bad arity")))

   let dest_number_dep1_any_term t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [Number s1] };
                term_terms = [ { bvars = [v]; bterm = t } ]
              } ->
            s1, v, t
       | _ ->
            REF_RAISE(RefineError ("dest_number_dep1_any_term", TermMatchError (t, "bad arity")))

   let mk_number_dep1_term opname s1 v t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [Number s1] };
           term_terms = [mk_bterm [v] t]}}

   (*
    * Two number parameters and one subterm.
    *)
   let is_number_number_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _; Number _] };
             term_terms = [ { bvars = []; _ } ]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_number_number_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
             term_terms = [bt]
           } when Opname.eq opname opname' ->
         s1, s2, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_dep0_any_term t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
             term_terms = [bt]
           } ->
         s1, s2, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_dep0_term opname s1 s2 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [Number s1; Number s2] };
           term_terms = [mk_simple_bterm t]}}

   (*
    * Two number parameters, a string, and one subterm.
    *)
   let is_number_number_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _; Number _; String _] };
             term_terms = [ { bvars = []; _ } ]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_number_number_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
             term_terms = [bt]
           } when Opname.eq opname opname' ->
         s1, s2, s3, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_string_dep0_any_term t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
             term_terms = [bt]
           } ->
         s1, s2, s3, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_string_dep0_term opname s1 s2 s3 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [Number s1; Number s2; String s3] };
           term_terms = [mk_simple_bterm t]}}

   (*
    * Two number parameters, a string, and two subterms.
    *)
   let is_number_number_string_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _; Number _; String _] };
             term_terms = [ { bvars = []; _ }; { bvars = []; _ } ]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_number_number_string_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
             term_terms = [{bvars=[]; bterm=t1}; {bvars=[]; bterm=t2}]
           } when Opname.eq opname opname' ->
         s1, s2, s3, t1, t2
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_string_dep0_dep0_any_term t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
             term_terms = [{bvars=[]; bterm=t1}; {bvars=[]; bterm=t2}]
           } ->
         s1, s2, s3, t1, t2
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_string_dep0_dep0_term opname s1 s2 s3 t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [Number s1; Number s2; String s3] };
           term_terms = [{bvars=[]; bterm=t1}; {bvars=[]; bterm=t2}]}}

   (*
    * One string parameter, two subterms.
    *)
   let is_string_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _] };
             term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
           } when Opname.eq opname opname' -> true
    | _ ->
         false

   let dest_string_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s] };
             term_terms = [bt1;bt2]
           } when Opname.eq opname opname' ->
         let destr = dest_simple_bterm in
         s, destr bt1, destr bt2
    | _ ->
         REF_RAISE(RefineError ("dest_string_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_dep0_dep0_any_term t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s] };
             term_terms = [bt1;bt2]
           } ->
         let destr = dest_simple_bterm in
         s, destr bt1, destr bt2
    | _ ->
         REF_RAISE(RefineError ("dest_string_dep0_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_dep0_dep0_term opname s t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [mk_simple_bterm t1; mk_simple_bterm t2]}}

   let mk_string_dep0_dep0_dep0_term opname s t1 t2 t3 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [mk_simple_bterm t1; mk_simple_bterm t2; mk_simple_bterm t3]}}

   (*
    * Two string parameters, two subterms.
    *)
   let is_string_string_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _; String _] };
             term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
           } when Opname.eq opname opname' -> true
    | _ ->
         false

   let dest_string_string_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s1; String s2] };
             term_terms = [bt1;bt2]
           } when Opname.eq opname opname' ->
         let destr = dest_simple_bterm in
         s1, s2, destr bt1, destr bt2
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_string_dep0_dep0_any_term t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String s1; String s2] };
             term_terms = [bt1;bt2]
           } ->
         let destr = dest_simple_bterm in
         s1, s2, destr bt1, destr bt2
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_dep0_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_string_dep0_dep0_term opname s1 s2 t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [mk_simple_bterm t1; mk_simple_bterm t2]}}

   (*
    * One number param.
    *)
   let is_number_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _] };
             term_terms = []
           } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_number_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number n] };
             term_terms = []
           } when Opname.eq opname opname' -> n
    | _ -> REF_RAISE(RefineError ("dest_number_term", TermMatchError (t, "bad arity")))

   let dest_number_any_term t = match get_core t with
      Term { term_op = { op_params = [Number n]; _ }; term_terms = [] } ->
         n
    | _ ->
         REF_RAISE(RefineError ("dest_number_any_term", TermMatchError (t, "bad arity")))

   let mk_number_term opname n =
      { free_vars = Vars SymbolSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [Number n] };
           term_terms = [] }}

   (*
    * One universe param.
    *)
   let is_univ_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [MLevel _] };
             term_terms = []
           } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_univ_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [MLevel n] };
             term_terms = []
           } when Opname.eq opname opname' -> n
    | _ -> REF_RAISE(RefineError ("dest_univ_term", TermMatchError (t, "")))

   let mk_univ_term opname n =
      { free_vars = Vars SymbolSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [MLevel n] };
           term_terms = [] }}

   (*
    * One quote param.
    *)
   let is_quoted_term t = match get_core t with
      Term { term_op = { op_params = Quote::_; _ }; _ } -> true
    | _ -> false

   let unquote_term t = match get_core t with
      Term { term_op = { op_name = opname; op_params = Quote::params }; term_terms = bterms } ->
         if Opname.eq opname var_opname || Opname.eq opname sequent_opname then
            raise (Invalid_argument "Term_op_ds.unquote_term: support for variables and sequents not implementes");
         { free_vars = VarsDelayed;
           core = Term
            { term_op = { op_name = opname; op_params = params }; term_terms = bterms }
         }
    | _ -> REF_RAISE(RefineError ("unquote_term", TermMatchError (t, "not a quote term")))

   let quote_term t = match get_core t with
      Term { term_op = { op_name = opname; op_params = params }; term_terms = bterms } ->
         { free_vars = VarsDelayed;
           core = Term
            { term_op = { op_name = opname; op_params = Quote::params }; term_terms = bterms }
         }
    | FOVar _ | SOVar _ | Sequent _ | SOContext _ ->
         raise (Invalid_argument "Term_op_ds.quote_term: support for variables, sequents and contexts not implemented")
    | Subst _ | Hashed _ ->
         fail_core "Term_op_ds.quote_term"

   (*
    * One token param.
    *)
   let is_token_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Token _] };
             term_terms = []
           } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_token_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Token n] };
             term_terms = []
           } when Opname.eq opname opname' -> n
    | _ -> REF_RAISE(RefineError ("dest_token_term", TermMatchError (t, "bad arity")))

   let dest_token_param t = match get_core t with
      Term { term_op = { op_params = [Token n]; _ }; _ } -> n
    | _ -> REF_RAISE(RefineError ("dest_token_param", TermMatchError (t, "bad arity")))

   let mk_token_term opname n =
      { free_vars = Vars SymbolSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [Token n] };
           term_terms = [] }}

   (*
    * One token param.
    *)
   let is_token_simple_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [Token _] };
                term_terms = terms
              } when Opname.eq opname opname' ->
            no_bvars terms
       | _ ->
            false

   let dest_token_simple_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [Token n] };
                term_terms = terms
              } when Opname.eq opname opname' ->
            n, List.map dest_simple_bterm terms
       | _ ->
            REF_RAISE(RefineError ("dest_token_term", TermMatchError (t, "bad arity")))

   let mk_token_simple_term opname n terms =
      { free_vars = VarsDelayed;
        core = Term { term_op = { op_name = opname; op_params = [Token n] };
                      term_terms = List.map mk_simple_bterm terms
               }
      }

   (*
    * Bound term.
    *)
   let is_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [_]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep1_term opname v t =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [{ bvars = [v]; bterm = t }]}}

   let dest_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [v]; bterm = t }]
           } when Opname.eq opname' opname -> v,t
    | _ -> REF_RAISE(RefineError ("dest_dep1_term", TermMatchError (t, "bad arity")))

   let dest_dep1_any_term t = match get_core t with
      Term { term_op = { op_params = []; _ };
             term_terms = [{ bvars = [v]; bterm = t }]
           } -> v, t
    | _ -> REF_RAISE(RefineError ("dest_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep1_dep1_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{ bvars = [_]; _ }; { bvars = [_]; _ }]
         } -> Opname.eq opname' opname
       | _ ->
            false

   let mk_dep1_dep1_term opname v1 t1 v2 t2 =
      { free_vars = VarsDelayed;
        core = Term { term_op = { op_name = opname; op_params = [] };
                      term_terms =
                         [{ bvars = [v1]; bterm = t1 };
                          { bvars = [v2]; bterm = t2 }]
               }
      }

   let dest_dep1_dep1_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{ bvars = [v1]; bterm = t1 };
                              { bvars = [v2]; bterm = t2 }]
              } when Opname.eq opname' opname -> v1, t1, v2, t2
       | _ ->
            REF_RAISE(RefineError ("dest_dep1_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [_; _]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep2_term opname v1 v2 t =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [{ bvars = [v1;v2]; bterm = t }]}}

   let dest_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [v1;v2]; bterm = t }]
           } when Opname.eq opname' opname -> v1,v2,t
    | _ -> REF_RAISE(RefineError ("dest_dep2_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; _ }; { bvars = [_]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let is_dep0_dep1_any_term t = match get_core t with
      Term { term_op = { op_params = []; _ };
             term_terms = [{ bvars = []; _ }; { bvars = [_]; _ }] } -> true
    | _ -> false

   let mk_dep0_dep1_term opname v t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
               [ mk_simple_bterm t1;
                 { bvars = [v]; bterm = t2 }]}}

   let mk_dep0_dep1_any_term op v t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = op;
           term_terms =
               [ mk_simple_bterm t1;
                 { bvars = [v]; bterm = t2 }]}}

   let dest_dep0_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt1;bt2]
           } when Opname.eq opname' opname ->
      begin match (bt1, bt2) with
         ({ bvars = []; bterm = t1 }, { bvars = [v]; bterm = t2 }) ->
            v, t1, t2
       | _ -> REF_RAISE(RefineError ("dest_dep0_dep1_term", TermMatchError (t, "bad arity")))
      end
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep1_term", TermMatchError (t, "bad arity")))

   let dest_dep0_dep1_any_term t = match get_core t with
      Term { term_terms = [{ bvars = []; bterm = t1 }; { bvars = [v]; bterm = t2 }]; _ } -> v, t1, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep1_any_term", TermMatchError (t, "bad arity")))

   let is_dep1_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [_]; _ }; { bvars = []; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep1_dep0_term opname v t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
               [ { bvars = [v]; bterm = t1 };
                 { bvars = []; bterm = t2 }]}}

   let dest_dep1_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt1; bt2]
           } when Opname.eq opname' opname ->
      begin match (bt1, bt2) with
         ({ bvars = [v]; bterm = t1 }, { bvars = []; bterm = t2 }) ->
            v, t1, t2
       | _ -> REF_RAISE(RefineError ("dest_dep1_dep0_term", TermMatchError (t, "bad arity")))
      end
    | _ -> REF_RAISE(RefineError ("dest_dep1_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * First subterm of arity 2.
    *)
   let is_dep2_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [_; _]; _ }; { bvars = []; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep2_dep0_term opname v1 v2 t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [{ bvars = [v1; v2]; bterm = t1 };
             mk_simple_bterm t2]}}

   let dest_dep2_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [v1; v2]; bterm = t1 };
                           { bvars = []; bterm = t2 }]
           } when Opname.eq opname' opname -> v1, v2, t1, t2
    | _ -> REF_RAISE(RefineError ("dest_dep2_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Second subterm of arity 2.
    *)
   let is_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; _ }; { bvars = [_; _]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep2_term opname v1 v2 t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t1;
             { bvars = [v1; v2]; bterm = t2 }]}}

   let dest_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; bterm = t1 };
                           { bvars = [v1; v2]; bterm = t2 }]
           } when Opname.eq opname' opname -> v1, v2, t1, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep2_term", TermMatchError (t, "bad arity")))

   (*
    * Second subterm of arity 2.
    *)
   let is_dep0_dep3_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{ bvars = []; _ }; { bvars = [_; _; _]; _}]
              } -> Opname.eq opname' opname
       | _ ->
            false

   let mk_dep0_dep3_term opname v1 v2 v3 t1 t2 =
         { free_vars = VarsDelayed;
           core = Term
                  { term_op = { op_name = opname; op_params = [] };
                    term_terms =
                       [mk_simple_bterm t1;
                        { bvars = [v1; v2; v3]; bterm = t2 }]}}

   let dest_dep0_dep3_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{ bvars = []; bterm = t1 };
                              { bvars = [v1; v2; v3]; bterm = t2 }]
              } when Opname.eq opname' opname ->
            v1, v2, v3, t1, t2
       | _ ->
            REF_RAISE(RefineError ("dest_dep0_dep3_term", TermMatchError (t, "bad arity")))

   (*
    * Four subterms.
    *)
   let is_dep0_dep2_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; _ }; { bvars = [_; _]; _ };
                           { bvars = []; _ }; { bvars = [_; _]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep2_dep0_dep2_term opname t0 v11 v12 t1 base v21 v22 t2 =
         mk_term
            { op_name = opname; op_params = [] }
            [mk_simple_bterm t0;
             mk_bterm [v11; v12] t1;
             mk_simple_bterm base;
             mk_bterm [v21; v22] t2]

   let dest_dep0_dep2_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; bterm = t0 };
                           { bvars = [v11; v12]; bterm = t1 };
                           { bvars = []; bterm = base };
                           { bvars = [v21; v22]; bterm = t2 }]
           } when Opname.eq opname' opname ->
         t0, v11, v12, t1, base, v21, v22, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep2_dep0_dep2_term", TermMatchError (t, "bad arity")))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = [_]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep1_term opname t0 t1 v2 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bvars = [v2]; bterm = t2 }]}}

   let dest_dep0_dep0_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; bterm = t0 };
                           { bvars = []; bterm = t1 };
                           { bvars = [v2]; bterm = t2 }]
           } when Opname.eq opname' opname ->
         t0, t1, v2, t2
    | _ ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = [_; _]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep2_term opname t0 t1 v21 v22 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bvars = [v21;v22]; bterm = t2 }]}}

   let dest_dep0_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; bterm = t0 };
                           { bvars = []; bterm = t1 };
                           { bvars = [v21;v22]; bterm = t2 }]
           } when Opname.eq opname' opname ->
         t0, t1, v21, v22, t2
    | _ ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep2_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep1_any_term t = match get_core t with
      Term { term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = [_]; _ }]; _ } -> true
    | _ -> false

   let mk_dep0_dep0_dep1_any_term op t0 t1 v2 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = op;
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bvars = [v2]; bterm = t2 }]}}

   let dest_dep0_dep0_dep1_any_term t = match get_core t with
      Term { term_terms = [{ bvars = []; bterm = t0 };
                           { bvars = []; bterm = t1 };
                           { bvars = [v2]; bterm = t2 }];
             _
      } -> t0, t1, v2, t2
    | _ ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep1_any_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep1_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; _ }; { bvars = [_]; _ }; { bvars = [_]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep1_dep1_term opname t0 v1 t1 v2 t2 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_simple_bterm t0;
          mk_bterm [v1] t1;
          mk_bterm [v2] t2]

   let dest_dep0_dep1_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; bterm = t0 };
                           { bvars = [v1]; bterm = t1 };
                           { bvars = [v2]; bterm = t2 }]
           } when Opname.eq opname' opname -> t0, v1, t1, v2, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep1_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep2_dep2_dep0_dep0_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{ bvars = [_; _]; _ };
                              { bvars = [_; _]; _ };
                              { bvars = []; _ };
                              { bvars = []; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep2_dep2_dep0_dep0_term opname v11 v12 t1 v21 v22 t2 t3 t4 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_bterm [v11; v12] t1;
          mk_bterm [v21; v22] t2;
          mk_simple_bterm t3;
          mk_simple_bterm t4]

   let dest_dep2_dep2_dep0_dep0_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{ bvars = [v11; v12]; bterm = t1 };
                              { bvars = [v21; v22]; bterm = t2 };
                              { bvars = []; bterm = t3 };
                              { bvars = []; bterm = t4 }]
           } when Opname.eq opname' opname -> v11, v12, t1, v21, v22, t2, t3, t4
    | _ -> REF_RAISE(RefineError ("dest_dep2_dep2_dep0_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep3_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = [_; _; _]; _ }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep3_term opname t0 t1 v1 v2 v3 t2 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_simple_bterm t0;
          mk_simple_bterm t1;
          mk_bterm [v1; v2; v3] t2]

   let dest_dep0_dep0_dep3_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = []; bterm = t0 };
                           { bvars = []; bterm = t1 };
                           { bvars = [v1; v2; v3]; bterm = t2 }]
           } when Opname.eq opname' opname -> t0, t1, v1, v2, v3, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep0_dep3_term", TermMatchError (t, "bad arity")))

   (*
    * One subterm with opname.
    *)
(* unused
   let is_one_bsubterm opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [_]
           } when Opname.eq opname' opname -> true
    | _ -> false

   let dest_one_bsubterm opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bterm]
           } when Opname.eq opname' opname -> bterm
    | _ -> REF_RAISE(RefineError ("dest_one_bsubterm", TermMatchError (t, "bad arity")))

   let mk_one_bsubterm opname bt =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [bt] }}
*)

   (************************************************************************
    * TERM MAPS                                                            *
    ************************************************************************)

   (*
    * Sweep a function down through the term.
    *)
   let rec iter_down f t =
      f t;
      match get_core t with
         Term t ->
            List.iter (fun bterm -> iter_down f bterm.bterm) t.term_terms
       | FOVar _ ->
            ()
       | SOVar (_, _, terms) ->
            List.iter (iter_down f) terms
       | SOContext (_, t, _, terms) ->
            iter_down f t;
            List.iter (iter_down f) terms
       | Sequent { sequent_hyps = hyps;
                   sequent_concl = concl;
                   sequent_args = args
         } ->
            iter_down f args;
            SeqHyp.iter (function
               Hypothesis (_, t) ->
                  iter_down f t
             | Context (_, _, args) ->
                  List.iter (iter_down f) args) hyps;
            iter_down f concl
       | Subst _
       | Hashed _ ->
            fail_core "Term_op_ds.map_down"

   let rec iter_up f t =
      (match get_core t with
          Term t ->
             List.iter (fun bterm -> iter_up f bterm.bterm) t.term_terms
        | FOVar _ ->
             ()
        | SOVar (_, _, terms) ->
             List.iter (iter_up f) terms
        | SOContext (_, t, _, terms) ->
             iter_up f t;
             List.iter (iter_up f) terms
        | Sequent { sequent_hyps = hyps;
                    sequent_concl = concl;
                    sequent_args = args
          } ->
             iter_up f args;
             SeqHyp.iter (function
                Hypothesis (_, t) ->
                   iter_up f t
              | Context (_, _, args) ->
                   List.iter (iter_up f) args) hyps;
             iter_up f concl
        | Subst _
        | Hashed _ ->
             fail_core "Term_op_ds.map_down");
      f t

   (*
    * Sweep a function down through the term.
    *)
   let rec bterm_down f btrm =
      { bvars = btrm.bvars; bterm = map_down f btrm.bterm }

   and map_down f t =
      let t = f t in
      match get_core t with
         Term trm ->
            make_term { term_op = trm.term_op; term_terms = List.map (bterm_down f) trm.term_terms }
       | FOVar _ -> t
       | SOVar(v, conts, ts) -> core_term (SOVar(v, conts, List.map (map_down f) ts))
       | SOContext(v, t, conts, ts) -> core_term (SOContext(v, map_down f t, conts, List.map (map_down f) ts))
       | Sequent { sequent_hyps = hyps;
                   sequent_concl = concl;
                   sequent_args = args
         } ->
            let args = map_down f args in
            let hyps =
               SeqHyp.map (function
                  Hypothesis (v, t) ->
                     Hypothesis (v, map_down f t)
                | Context (v, vl, args) ->
                     Context (v, vl, List.map (map_down f) args)) hyps
            in
            let concl = map_down f concl in
               mk_sequent_term { sequent_hyps = hyps;
                                 sequent_concl = concl;
                                 sequent_args = args
               }
       | Subst _ | Hashed _ -> fail_core "Term_op_ds.map_down"

   let rec bterm_up f btrm =
      { bvars = btrm.bvars; bterm = map_up f btrm.bterm }

   and map_up f t =
      match get_core t with
         Term trm ->
            f (make_term { term_op = trm.term_op; term_terms = List.map (bterm_up f) trm.term_terms })
       | FOVar _ -> f t
       | SOVar(v, conts, ts) -> f (core_term (SOVar(v, conts, List.map (map_up f) ts)))
       | SOContext(v, t, conts, ts) -> f (core_term (SOContext(v, map_up f t, conts, List.map (map_up f) ts)))
       | Sequent { sequent_hyps = hyps;
                   sequent_concl = concl;
                   sequent_args = args
         } ->
            let args = map_up f args in
            let hyps =
               SeqHyp.map (function
                  Hypothesis (v, t) ->
                     Hypothesis (v, map_up f t)
                | Context (v, vl, args) ->
                     Context (v, vl, List.map (map_up f) args)) hyps
            in
            let concl = map_up f concl in
               f (mk_sequent_term { sequent_hyps = hyps;
                                 sequent_concl = concl;
                                 sequent_args = args
                  })
       | Subst _ | Hashed _ -> fail_core "Term_op_ds.map_up"
end
