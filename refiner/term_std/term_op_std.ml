(*
 * Standard operations on terms.
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

INCLUDE "refine_error.mlh"

open Refine_error_sig
open Term_std_sig
open Term_std

module TermOp
   (Term : TermStdSig
    with type level_exp_var = TermType.level_exp_var
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type operator = TermType.operator
    with type term = TermType.term
    with type bound_term = TermType.bound_term

    with type level_exp_var' = TermType.level_exp_var'
    with type level_exp' = TermType.level_exp'
    with type object_id = TermType.object_id
    with type param' = TermType.param'
    with type operator' = TermType.operator'
    with type term' = TermType.term'
    with type bound_term' = TermType.bound_term')
   (RefineError : RefineErrorSig
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type term = TermType.term
    with type bound_term = TermType.bound_term)
=
struct
   open RefineError
   open TermType

   type term = TermType.term
   type operator = TermType.operator
   type level_exp = TermType.level_exp

   (*
    * Helper functins for simple terms.
    *)
   let is_simple_bterm bterm =
      bterm.bvars = []

   let mk_simple_bterm t =
      { bvars = []; bterm = t }

   let dest_simple_bterm t = function
      { bvars = []; bterm = t } ->
         t
    | _ ->
         REF_RAISE(RefineError ("dest_simple_bterm", TermMatchError (t, "not a simple term")))

   let is_simple_bterms terms =
      List.for_all is_simple_bterm terms

   let mk_simple_bterms terms =
      List.map mk_simple_bterm terms

   let dest_simple_bterms t bterms =
      List.map (dest_simple_bterm t) bterms

   (*
    * Terms with no subterms.
    *)
   let is_no_subterms_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = []
      } ->
         Opname.eq opname' opname
    | _ ->
         false

   (*
    * Terms with one subterm
    *)
   let is_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_term opname t =
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }]
      }

   let dest_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname' opname -> t
    | t -> REF_RAISE(RefineError ("dest_dep0_term", TermMatchError (t, "not a dep0 term")))

   let one_subterm = function
      ({ term_terms = [{ bvars = []; bterm = t }]} : term) -> t
    | t -> REF_RAISE(RefineError ("one_subterm", TermMatchError (t, "not a single subterm")))

   let one_subterm_opname opname = function
      ({ term_op = { op_name = opname' }; term_terms = [{ bvars = []; bterm = t }]} : term)
      when Opname.eq opname' opname -> t
    | t -> REF_RAISE(RefineError ("one_subterm", TermMatchError (t, "not a single subterm")))

   (*
    * Terms with two subterms.
    *)
   let is_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_term opname = fun
      t1 t2 ->
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 };
                         { bvars = []; bterm = t2 }]
         }

   let dest_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = []; bterm = t2 }]
      } when Opname.eq opname' opname -> t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let two_subterms = function
      ({ term_terms = [{ bvars = []; bterm = a };
                       { bvars = []; bterm = b }]} : term) -> a, b
    | t -> REF_RAISE(RefineError ("two_subterms", TermMatchError (t, "bad arity")))

   let two_subterms_opname opname  = function
      ({ term_op = { op_name = opname' };
         term_terms = [{ bvars = []; bterm = a };
                       { bvars = []; bterm = b }]} : term)
      when Opname.eq opname' opname -> a, b
    | t -> REF_RAISE(RefineError ("two_subterms", TermMatchError (t, "bad arity")))

   (*
    * Terms with three subterms.
    *)
   let is_dep0_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep0_term opname = fun
      t1 t2 t3  ->
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 };
                         { bvars = []; bterm = t2 };
                         { bvars = []; bterm = t3 }]
         }

   let dest_dep0_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = []; bterm = t2 };
                      { bvars = []; bterm = t3 }]
      } when Opname.eq opname' opname -> t1, t2, t3
    | t -> REF_RAISE(RefineError ("dest_dep0_dep0_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Terms with four subterms.
    *)
   let is_dep0_dep0_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [] }; { bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep0_dep0_term opname = fun
      t1 t2 t3 t4 ->
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 };
                         { bvars = []; bterm = t2 };
                         { bvars = []; bterm = t3 };
                         { bvars = []; bterm = t4 }]
         }

   let dest_dep0_dep0_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = []; bterm = t2 };
                      { bvars = []; bterm = t3 };
                      { bvars = []; bterm = t4 }]
      } when Opname.eq opname' opname -> t1, t2, t3, t4
    | t -> REF_RAISE(RefineError ("dest_dep0_dep0_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let is_two_subterm opname = function
      { term_op = { op_name = opname' };
        term_terms = [{ bvars = [] }; { bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false

   let is_three_subterm opname = function
      { term_op = { op_name = opname' };
        term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false

   let is_four_subterm opname = function
      { term_op = { op_name = opname' };
        term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [] }; { bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false

   let is_five_subterm opname = function
      { term_op = { op_name = opname' };
        term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [] }; { bvars = [] }; { bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false

   let three_subterms = function
      { term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b };
                      { bvars = []; bterm = c }]} ->
         a, b, c
    | t -> REF_RAISE(RefineError ("three_subterms", TermMatchError (t, "bad arity")))

   let four_subterms = function
      { term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b };
                      { bvars = []; bterm = c };
                      { bvars = []; bterm = d }]} ->
         a, b, c, d
    | t ->
         REF_RAISE(RefineError ("four_subterms", TermMatchError (t, "bad arity")))

   let five_subterms = function
      { term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b };
                      { bvars = []; bterm = c };
                      { bvars = []; bterm = d };
                      { bvars = []; bterm = e }]} ->
         a, b, c, d, e
    | t ->
         REF_RAISE(RefineError ("five_subterms", TermMatchError (t, "bad arity")))

   let six_subterms = function
      { term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b };
                      { bvars = []; bterm = c };
                      { bvars = []; bterm = d };
                      { bvars = []; bterm = e };
                      { bvars = []; bterm = f }]} ->
         a, b, c, d, e, f
    | t ->
         REF_RAISE(RefineError ("six_subterms", TermMatchError (t, "bad arity")))

   (************************************************************************
    * Nonsimple but useful forms                                           *
    ************************************************************************)

   (*
    * One string param.
    *)
   let is_string_term opname = function
      { term_op = { op_name = opname'; op_params = [String _] };
        term_terms = []
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_term opname = function
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = []
      } when Opname.eq opname opname' ->
         s
    | t ->
         REF_RAISE(RefineError ("dest_string_term", TermMatchError (t, "not a string term")))

   let dest_string_param = function
      { term_op = { op_params = String s :: _ } } ->
         s
    | t ->
         REF_RAISE(RefineError ("dest_string_param", TermMatchError (t, "no string parameter")))

   let mk_string_term opname s =
      { term_op = { op_name = opname; op_params = [String s] }; term_terms = [] }

   (*
    * One string parameter, and one simple subterm.
    *)
   let is_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String _] };
        term_terms = [{ bvars = [] }]
      } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' -> s, t
    | t -> REF_RAISE(RefineError ("dest_string_dep0_term", TermMatchError (t, "bad arity")))

   let mk_string_dep0_term opname = fun
      s t ->
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   (*
    * Two string parameters, and one simple subterm.
    *)
   let is_string_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String _; String _] };
        term_terms = [{ bvars = [] }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, s2, t
    | t ->
         REF_RAISE(RefineError ("dest_string_string_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_string_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         s1, s2, t
    | t ->
         REF_RAISE(RefineError ("dest_string_string_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_string_dep0_term opname = fun
      s1 s2 t ->
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   (*
    * One number parameter and one subterm.
    *)
   let is_number_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _] };
        term_terms = [{ bvars = [] }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, t
    | t ->
         REF_RAISE(RefineError ("dest_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1] };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         s1, t
    | t ->
         REF_RAISE(RefineError ("dest_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_dep0_term opname = fun
      s1 t ->
         { term_op = { op_name = opname; op_params = [Number s1] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   let is_number_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _] };
        term_terms = [{ bvars = [_] }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1] };
        term_terms = [{ bvars = [v]; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, v, t
    | t ->
         REF_RAISE(RefineError ("dest_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_dep1_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1] };
        term_terms = [{ bvars = [v]; bterm = t }]
      } ->
         s1, v, t
    | t ->
         REF_RAISE(RefineError ("dest_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_dep1_term opname = fun
      s1 v t ->
         { term_op = { op_name = opname; op_params = [Number s1] };
           term_terms = [{ bvars = [v]; bterm = t }]
         }

   (*
    * Two number parameters and one subterm.
    *)
   let is_number_number_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _; Number _] };
        term_terms = [{ bvars = [] }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_number_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, s2, t
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         s1, s2, t
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_dep0_term opname = fun
      s1 s2 t ->
         { term_op = { op_name = opname; op_params = [Number s1; Number s2] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   (*
    * Two number parameters, a string, and one subterm.
    *)
   let is_number_number_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _; Number _; String _] };
        term_terms = [{ bvars = [] }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_number_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, s2, s3, t
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_string_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         s1, s2, s3, t
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_string_dep0_term opname = fun
      s1 s2 s3 t ->
         { term_op = { op_name = opname; op_params = [Number s1; Number s2; String s3] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   (*
    * Two string parameters, two subterms.
    *)
   let is_string_string_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String _; String _] };
        term_terms = [{ bvars = [] }; { bvars = [] }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_string_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } when Opname.eq opname opname' ->
         s1, s2, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_string_string_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_string_dep0_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } ->
         s1, s2, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_string_string_dep0_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_string_dep0_dep0_term opname = fun
      s1 s2 t1 t2 ->
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
         }

   (*
    * One number param.
    *)
   let is_number_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _] };
        term_terms = []
      } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_number_term opname = function
      { term_op = { op_name = opname'; op_params = [Number n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | t -> REF_RAISE(RefineError ("dest_number_term", TermMatchError (t, "bad arity")))

   let dest_number_any_term = function
      { term_op = { op_params = [Number n] };
        term_terms = []
      } ->
         n
    | t ->
         REF_RAISE(RefineError ("dest_number_any_term", TermMatchError (t, "bad arity")))

   let mk_number_term opname = function
      n ->
         { term_op = { op_name = opname; op_params = [Number n] };
           term_terms = []
         }

   (*
    * One universe param.
    *)
   let is_univ_term opname = function
      { term_op = { op_name = opname'; op_params = [MLevel _] };
        term_terms = []
      } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_univ_term opname = function
      { term_op = { op_name = opname'; op_params = [MLevel n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | t -> REF_RAISE(RefineError ("dest_univ_term", TermMatchError (t, "bad arity")))

   let mk_univ_term opname = function
      n ->
         { term_op = { op_name = opname; op_params = [MLevel n] };
           term_terms = []
         }

   (*
    * One token param.
    *)
   let is_token_term opname = function
      { term_op = { op_name = opname'; op_params = [Token _] };
        term_terms = []
      } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_token_term opname = function
      { term_op = { op_name = opname'; op_params = [Token n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | t -> REF_RAISE(RefineError ("dest_token_term", TermMatchError (t, "bad arity")))

   let mk_token_term opname = function
      n ->
         { term_op = { op_name = opname; op_params = [Token n] };
           term_terms = []
         }

   (*
    * One token param, and no bound subterms.
    *)
   let is_token_simple_term opname = function
      { term_op = { op_name = opname'; op_params = [Token _] };
        term_terms = terms
      } when Opname.eq opname opname' ->
         is_simple_bterms terms
    | _ ->
         false

   let dest_token_simple_term opname = function
      { term_op = { op_name = opname'; op_params = [Token n] };
        term_terms = terms
      } as t when Opname.eq opname opname' ->
         n, dest_simple_bterms t terms
    | t ->
         REF_RAISE(RefineError ("dest_token_term", TermMatchError (t, "bad arity")))

   let mk_token_simple_term opname n terms =
      { term_op = { op_name = opname; op_params = [Token n] };
        term_terms = mk_simple_bterms terms
      }

   (*
    * Bound term.
    *)
   let is_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep1_term opname = fun
      v t -> { term_op = { op_name = opname; op_params = [] };
               term_terms = [{ bvars = [v]; bterm = t }]
             }

   let dest_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v]; bterm = t }]
      } when Opname.eq opname' opname -> v, t
    | t -> REF_RAISE(RefineError ("dest_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep1_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_] }; { bvars = [_] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep1_dep1_term opname v1 t1 v2 t2 =
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = [v1]; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      }

   let dest_dep1_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v1]; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, t1, v2, t2
    | t -> REF_RAISE(RefineError ("dest_dep1_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_;_] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep2_term opname = fun
      v1 v2 t -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = [v1;v2]; bterm = t }]
                 }

   let dest_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v1;v2]; bterm = t }]
      } when Opname.eq opname' opname -> v1, v2, t
    | t -> REF_RAISE(RefineError ("dest_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [_] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let is_dep0_dep1_any_term = function
      { term_op = { op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [_] }]
      } -> true
    | _ -> false

   let mk_dep0_dep1_term opname = fun
      v t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = []; bterm = t1 };
                                 { bvars = [v]; bterm = t2 }]
                 }

   let mk_dep0_dep1_any_term op = fun
      v t1 t2 -> { term_op = op;
                   term_terms = [{ bvars = []; bterm = t1 };
                                 { bvars = [v]; bterm = t2 }]
                 }

   let dest_dep0_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v]; bterm = t2 }]
      } when Opname.eq opname' opname -> v, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep1_term", TermMatchError (t, "bad arity")))

   let dest_dep0_dep1_any_term = function
      { term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v]; bterm = t2 }]
      } -> v, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep1_any_term", TermMatchError (t, "bad arity")))

   let is_dep1_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_] }; { bvars = [] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep1_dep0_term opname = fun
      v t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = [v]; bterm = t1 };
                                 { bvars = []; bterm = t2 }]
                 }

   let dest_dep1_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v]; bterm = t1 };
                      { bvars = []; bterm = t2 }]
      } when Opname.eq opname' opname -> v, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep1_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * First subterm of arity 2.
    *)
   let is_dep2_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_; _] }; { bvars = [] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep2_dep0_term opname = fun
      v1 v2 t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = [v1; v2]; bterm = t1 };
                                 { bvars = []; bterm = t2 }]
                 }

   let dest_dep2_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v1; v2]; bterm = t1 };
                      { bvars = []; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, v2, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep2_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Second subterm of arity 2.
    *)
   let is_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [_; _] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep2_term opname = fun
      v1 v2 t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = []; bterm = t1 };
                                 { bvars = [v1; v2]; bterm = t2 }]
                 }

   let dest_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v1; v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, v2, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep2_term", TermMatchError (t, "bad arity")))

   (*
    * Second subterm of arity 3.
    *)
   let is_dep0_dep3_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [_; _; _] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep3_term opname = fun
      v1 v2 v3 t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                          term_terms = [{ bvars = []; bterm = t1 };
                                        { bvars = [v1; v2; v3]; bterm = t2 }]
                        }

   let dest_dep0_dep3_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v1; v2; v3]; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, v2, v3, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_dep0_dep3_term", TermMatchError (t, "bad arity")))

   (*
    * Four subterms.
    *)
   let is_dep0_dep2_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [_; _] }; { bvars = [] }; { bvars = [_; _] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep2_dep0_dep2_term opname = fun
      t0 v11 v12 t1 base v21 v22 t2 -> { term_op = { op_name = opname; op_params = [] };
                                         term_terms = [{ bvars = []; bterm = t0 };
                                                       { bvars = [v11; v12]; bterm = t1 };
                                                       { bvars = []; bterm = base };
                                                       { bvars = [v21; v22]; bterm = t2 }]
                                       }

   let dest_dep0_dep2_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = [v11; v12]; bterm = t1 };
                      { bvars = []; bterm = base };
                      { bvars = [v21; v22]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, v11, v12, t1, base, v21, v22, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep2_dep0_dep2_term", TermMatchError (t, "bad arity")))

   let is_dep2_dep2_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_; _] }; { bvars = [_; _] }; { bvars = [] }; { bvars = [] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep2_dep2_dep0_dep0_term opname v11 v12 t1 v21 v22 t2 t3 t4 =
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = [v11; v12]; bterm = t1 };
                      { bvars = [v21; v22]; bterm = t2 };
                      { bvars = []; bterm = t3 };
                      { bvars = []; bterm = t4 }]
      }

   let dest_dep2_dep2_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v11; v12]; bterm = t1 };
                      { bvars = [v21; v22]; bterm = t2 };
                      { bvars = []; bterm = t3 };
                      { bvars = []; bterm = t4 }];
      } when Opname.eq opname' opname -> v11, v12, t1, v21, v22, t2, t3, t4
    | t -> REF_RAISE(RefineError ("dest_dep2_dep2_dep0_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_] }]
      } when Opname.eq opname' opname ->
         true
    | _ ->
         false

   let mk_dep0_dep0_dep1_term opname = fun
      t0 t1 v2 t2 -> { term_op = { op_name = opname; op_params = [] };
                          term_terms = [{ bvars = []; bterm = t0 };
                                        { bvars = []; bterm = t1 };
                                        { bvars = [v2]; bterm = t2 }]
                        }

   let dest_dep0_dep0_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } when Opname.eq opname' opname ->
         t0, t1, v2, t2
    | t ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep1_any_term = function
      { term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_] }] } ->
         true
    | _ ->
         false

   let mk_dep0_dep0_dep1_any_term op = fun
      t0 t1 v2 t2 -> { term_op = op;
                       term_terms = [{ bvars = []; bterm = t0 };
                                     { bvars = []; bterm = t1 };
                                     { bvars = [v2]; bterm = t2 }]
                     }

   let dest_dep0_dep0_dep1_any_term = function
      { term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } ->
         t0, t1, v2, t2
    | t ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep1_any_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep1_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [_] }; { bvars = [_] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep1_dep1_term opname = fun
      t0 v1 t1 v2 t2 -> { term_op = { op_name = opname; op_params = [] };
                          term_terms = [{ bvars = []; bterm = t0 };
                                        { bvars = [v1]; bterm = t1 };
                                        { bvars = [v2]; bterm = t2 }]
                        }

   let dest_dep0_dep1_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = [v1]; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, v1, t1, v2, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep1_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep3_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_; _; _] }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep0_dep3_term opname = fun
      t0 t1 v1 v2 v3 t2 -> { term_op = { op_name = opname; op_params = [] };
                             term_terms = [{ bvars = []; bterm = t0 };
                                           { bvars = []; bterm = t1 };
                                           { bvars = [v1; v2; v3]; bterm = t2 }]
                           }

   let dest_dep0_dep0_dep3_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v1; v2; v3]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, t1, v1, v2, v3, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep0_dep3_term", TermMatchError (t, "bad arity")))

   (*
    * One subterm with opname.
    *)
   let is_one_bsubterm opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [_]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let dest_one_bsubterm opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bterm]
      } when Opname.eq opname' opname -> bterm
    | t -> REF_RAISE(RefineError ("dest_one_bsubterm", TermMatchError (t, "bad arity")))

   let mk_one_bsubterm opname = fun
      bterm -> { term_op = { op_name = opname; op_params = [] }; term_terms = [bterm] }

   (*
    * Sweep a function down through the term.
    *)
   let rec map_down f t =
      let { term_op = op; term_terms = bterms } = f t in
      let apply { bvars = vars; bterm = t } =
         { bvars = vars; bterm = map_down f t }
      in
         { term_op = op; term_terms = List.map apply bterms }

   let rec map_up f { term_op = op; term_terms = bterms } =
      let apply { bvars = vars; bterm = t } =
         { bvars = vars; bterm = map_up f t }
      in
      let t = { term_op = op; term_terms = List.map apply bterms } in
         f t
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
