(* This file implements terms' recursive hashing module
 * based on weak arrays
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov, Alexey Nogin
 *)

open Term_sig
open Termmod_sig
open Infinite_weak_array
open Weak_memo
open Opname

module TermHash
   (ToTerm : Termmod_sig.TermModuleSig) 

   (TermHeader : Term_header_sig.TermHeaderSig 
      with type term = ToTerm.TermType.term
      with type param = ToTerm.TermType.param
      with type meta_term = ToTerm.TermType.meta_term

      with type 'a descriptor = 'a InfiniteWeakArray.descriptor
      with type 'a weak_descriptor = 'a InfiniteWeakArray.weak_descriptor) =

struct

   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType

   type param_header = TermHeader.param_header
   type param_weak_header = TermHeader.param_weak_header
   type term_header = TermHeader.term_header
   type term_weak_header = TermHeader.term_weak_header
   type meta_term_header = TermHeader.meta_term_header
   type meta_term_weak_header = TermHeader.meta_term_weak_header

   type param = TType.param
   type term = TType.term
   type meta_term = TType.meta_term


   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type term_index = TType.term TheWeakMemo.descriptor
   type meta_term_index = TType.meta_term TheWeakMemo.descriptor

   type t =
      { param_hash     : (t, TermHeader.param_header,
                             TermHeader.param_weak_header,
                             TType.param
                         ) TheWeakMemo.t;
        opname_hash    : (t, Opname.opname,
                             Opname.opname,
                             Opname.opname
                         ) TheWeakMemo.t;
        term_hash      : (t, TermHeader.term_header,
                             TermHeader.term_weak_header,
                             TType.term
                         ) TheWeakMemo.t;
        meta_term_hash : (t, TermHeader.meta_term_header,
                             TermHeader.meta_term_weak_header,
                             TType.meta_term
                         ) TheWeakMemo.t
      }


   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   let constr_level_var {TermHeader.le_var=var; TermHeader.le_offset=offset} =
      TTerm.make_level_var 
      { TType.le_var = var;
        TType.le_offset = offset
      }

   let constr_level { TermHeader.le_const=c; TermHeader.le_vars=vars } =
      TTerm.make_level
      { TType.le_const = c;
        TType.le_vars = List.map constr_level_var vars
      }

   let p_constr_param info param_header =
     TTerm.make_param
     (
      match param_header with
         TermHeader.Number n1 ->            TType.Number n1
       | TermHeader.String s1 ->            TType.String s1
       | TermHeader.Token s1 ->             TType.Token s1
       | TermHeader.Level l1 ->             TType.Level (constr_level l1)
       | TermHeader.Var v1 ->               TType.Var v1
       | TermHeader.MNumber s1 ->           TType.MNumber s1
       | TermHeader.MString s1 ->           TType.MString s1
       | TermHeader.MToken s1 ->            TType.MToken s1
       | TermHeader.MLevel s1 ->            TType.MLevel s1
       | TermHeader.MVar s1 ->              TType.MVar s1
       | TermHeader.ObId oid1 ->            TType.ObId (List.map (TheWeakMemo.retrieve info.param_hash info) oid1)
       | TermHeader.ParamList p1 ->         TType.ParamList (List.map (TheWeakMemo.retrieve info.param_hash info) p1)
       | TermHeader.MSum (p11, p21) ->      TType.MSum (TheWeakMemo.retrieve info.param_hash info p11, TheWeakMemo.retrieve info.param_hash info p21)
       | TermHeader.MDiff (p11, p21) ->     TType.MDiff (TheWeakMemo.retrieve info.param_hash info p11, TheWeakMemo.retrieve info.param_hash info p21)
       | TermHeader.MProduct (p11, p21) ->  TType.MProduct (TheWeakMemo.retrieve info.param_hash info p11, TheWeakMemo.retrieve info.param_hash info p21)
       | TermHeader.MQuotient (p11, p21) -> TType.MQuotient (TheWeakMemo.retrieve info.param_hash info p11, TheWeakMemo.retrieve info.param_hash info p21)
       | TermHeader.MRem (p11, p21) ->      TType.MRem (TheWeakMemo.retrieve info.param_hash info p11, TheWeakMemo.retrieve info.param_hash info p21)
       | TermHeader.MLessThan (p11, p21) -> TType.MLessThan (TheWeakMemo.retrieve info.param_hash info p11, TheWeakMemo.retrieve info.param_hash info p21)
       | TermHeader.MEqual (p11, p21) ->    TType.MEqual (TheWeakMemo.retrieve info.param_hash info p11, TheWeakMemo.retrieve info.param_hash info p21)
       | TermHeader.MNotEqual (p11, p21) -> TType.MNotEqual (TheWeakMemo.retrieve info.param_hash info p11, TheWeakMemo.retrieve info.param_hash info p21)
     )

   let p_constr_operator info (opname_index, param_indices) =
      TTerm.make_op
      { TType.op_name = TheWeakMemo.retrieve info.opname_hash info opname_index;
        TType.op_params = List.map (TheWeakMemo.retrieve info.param_hash info) param_indices
      }


   let p_constr_bterm info { TermHeader.bvars=bvs; TermHeader.bterm=term_index } =
      TTerm.make_bterm
      { TType.bvars = bvs;
        TType.bterm = TheWeakMemo.retrieve info.term_hash info term_index
      }

   let p_constr_hyp info hyp =
      match hyp with
         TermHeader.Hypothesis (v, t) -> TType.Hypothesis (v, TheWeakMemo.retrieve info.term_hash info t)
       | TermHeader.Context (v, trms) -> TType.Context (v, List.map (TheWeakMemo.retrieve info.term_hash info) trms)

   let p_constr_tterm info { TermHeader.op_name = op; TermHeader.op_params = params; TermHeader.term_terms = bterms } =
      TTerm.make_term
      { TType.term_op = p_constr_operator info (op, params);
        TType.term_terms = List.map (p_constr_bterm info) bterms }

   let p_constr_term info th =
      match th with
         TermHeader.Seq { TermHeader.seq_arg = arg; TermHeader.seq_hyps = hyps; TermHeader.seq_goals = goals } ->
               ToTerm.TermMan.mk_sequent_term
               { TType.sequent_args = TheWeakMemo.retrieve info.term_hash info arg;
                 TType.sequent_hyps  = TTerm.SeqHyp.of_list  (List.map (p_constr_hyp info) hyps);
                 TType.sequent_goals = TTerm.SeqGoal.of_list (List.map (TheWeakMemo.retrieve info.term_hash info) goals)
               }
       | TermHeader.Term th -> p_constr_tterm info th

   let p_constr_meta_term info mt =
      match mt with
      TermHeader.MetaTheorem t ->
         TType.MetaTheorem (TheWeakMemo.retrieve info.term_hash info t)
    | TermHeader.MetaImplies (t1, t2) ->
         TType.MetaImplies (TheWeakMemo.retrieve info.meta_term_hash info t1,
                            TheWeakMemo.retrieve info.meta_term_hash info t2)
    | TermHeader.MetaFunction (t1, mt1, mt2) ->
         TType.MetaFunction (TheWeakMemo.retrieve info.term_hash info t1,
                             TheWeakMemo.retrieve info.meta_term_hash info mt1,
                             TheWeakMemo.retrieve info.meta_term_hash info mt2)
    | TermHeader.MetaIff (mt1, mt2) ->
         TType.MetaIff (TheWeakMemo.retrieve info.meta_term_hash info mt1,
                        TheWeakMemo.retrieve info.meta_term_hash info mt2)
    | TermHeader.MetaLabeled (l, mt) ->
         TType.MetaLabeled (l, TheWeakMemo.retrieve info.meta_term_hash info mt)

   let p_create hash_size array_size =
      {
        param_hash = TheWeakMemo.create hash_size array_size TermHeader.weak_param_header TermHeader.compare_param_header p_constr_param;
        opname_hash = TheWeakMemo.create hash_size array_size (fun x -> x) eq (fun x y -> y);
        term_hash = TheWeakMemo.create hash_size array_size TermHeader.weak_term_header TermHeader.compare_term_header p_constr_term;
        meta_term_hash = TheWeakMemo.create hash_size array_size TermHeader.weak_meta_term_header TermHeader.compare_meta_term_header p_constr_meta_term
      }

   let p_lookup info th = TheWeakMemo.lookup info.term_hash info th

   let p_unsafe_lookup info th = TheWeakMemo.unsafe_lookup info.term_hash info th

   let p_retrieve info ti = TheWeakMemo.retrieve info.term_hash info ti

   let p_lookup_meta info mth = TheWeakMemo.lookup info.meta_term_hash info mth

   let p_unsafe_lookup_meta info mth = TheWeakMemo.unsafe_lookup info.meta_term_hash info mth

   let p_retrieve_meta info mti = TheWeakMemo.retrieve info.meta_term_hash info mti

   let global_hash = p_create 17 1024

   let lookup th = p_lookup global_hash th
   let unsafe_lookup th = p_unsafe_lookup global_hash th
   let retrieve ti = p_retrieve global_hash ti
   let lookup_meta mth = p_lookup_meta global_hash mth
   let unsafe_lookup_meta mth = p_unsafe_lookup_meta global_hash mth
   let retrieve_meta mti = p_retrieve_meta global_hash mti
end   

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_norm, term_copy_weak"
 * End:
 * -*-
 *)
