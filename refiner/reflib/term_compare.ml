(*
 * This function copies a term, producing as much
 * aliasing as possible.  It is parameterized by the
 * term type, so it can be used for conversion between
 * term types.
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
 * Modified by: Eli Barzilay, Alexey Nogin, Yegor Bryukhov
 *)

open Lm_debug

open Term_sig
open Termmod_sig
open Term_compare_sig

let debug_memo =
   create_debug (**)
      { debug_name = "memo";
        debug_description = "Display memo operations";
        debug_value = false
      }

module TermCompare : TermCompareSig =
functor (Term : TermModuleSig) ->
struct
   module CTerm = Term.Term
   module CType = Term.TermType

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type c_term = CTerm of CType.term' | CSeq of CType.esequent

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Compare that the elements on the lists are equal.
    *)
   let list_mem_eq = Lm_list_util.compare_eq

   (*
    * Comparison functions.
    *)
   let compare_level_var { CType.le_var = v1; CType.le_offset = offset1 }
                         { CType.le_var = v2; CType.le_offset = offset2 } =
      v1 == v2 & offset1 == offset2

   let compare_level { CType.le_const = const1; CType.le_vars = vars1 }
                     { CType.le_const = const2; CType.le_vars = vars2 } =
      const1 = const2 & list_mem_eq vars1 vars2

   let compare_param param1 param2 =
      match param1, param2 with
         Number    n1,         Number    n2         -> Lm_num.eq_num n1 n2
       | String    s1,         String    s2         -> s1 = s2
       | Token     s1,         Token     s2         -> s1 = s2
       | Var       v1,         Var       v2         -> v1 = v2
       | Quote       ,         Quote                -> true
       | MNumber   s1,         MNumber   s2         -> s1 = s2
       | MString   s1,         MString   s2         -> s1 = s2
       | MToken    s1,         MToken    s2         -> s1 = s2
       | MLevel    l1,         MLevel    l2         -> l1 == l2
       | ObId      oid1,       ObId      oid2       -> list_mem_eq oid1 oid2
       | ParamList params1,    ParamList params2    -> list_mem_eq params1 params2
       | _ -> false

   let compare_operator { CType.op_name = opname1; CType.op_params = params1 }
                        { CType.op_name = opname2; CType.op_params = params2 } =
      Opname.eq opname1 opname2 & list_mem_eq params1 params2

   let compare_term { CType.term_op = op1; CType.term_terms = bterms1 }
                    { CType.term_op = op2; CType.term_terms = bterms2 } =
      (op1 == op2) & list_mem_eq bterms1 bterms2

   let rec compare_hyps hyp1 hyp2 i =
      (i < 0) ||
      ((match (CTerm.SeqHyp.get hyp1 i), (CTerm.SeqHyp.get hyp2 i) with
           Term_sig.Hypothesis (v1,t1),       Term_sig.Hypothesis (v2,t2)       -> v1 = v2 && t1 == t2
         | Term_sig.Context    (v1,cts1,ts1), Term_sig.Context    (v2,cts2,ts2) -> v1 = v2 && cts1=cts2 && list_mem_eq ts1 ts2
         | _ -> false) &&
       (compare_hyps hyp1 hyp2 (pred i)))

   let compare_cterm t1 t2 =
      match (t1,t2) with
         CTerm t1, CTerm t2 ->
            compare_term t1 t2
       | CSeq { CType.sequent_args = arg1; CType.sequent_hyps = hyp1; CType.sequent_concl = concl1},
         CSeq { CType.sequent_args = arg2; CType.sequent_hyps = hyp2; CType.sequent_concl = concl2} ->
            (arg1 == arg2) &&
            (concl1 == concl2) &&
            (CTerm.SeqHyp.length hyp1 = CTerm.SeqHyp.length hyp2) &&
            (compare_hyps hyp1 hyp2 (CTerm.SeqHyp.length hyp1 - 1))
       | _ -> false

   let compare_bterm { CType.bvars = bvars1; CType.bterm = bterm1 }
       { CType.bvars = bvars2; CType.bterm = bterm2 } =
      bvars1 = bvars2 & bterm1 == bterm2

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
