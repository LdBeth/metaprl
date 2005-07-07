(* This file implements terms' normalization (DAG-ization)
 * It stores and access terms stored in Term_hash's structure
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

open Term_header_constr

module TermNorm
   (ToTerm : Termmod_sig.TermModuleInternalSig)
   (TermHash : Term_hash_sig.TermHashInternalSig
      with type param = ToTerm.TermType.param
      with type param' = ToTerm.TermType.param'
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term
      with type msequent = ToTerm.Refine .msequent) =

struct
   module THC = TermHeaderConstr (ToTerm) (ToTerm) (TermHash)

   type t = TermHash.t
   type term = ToTerm.TermType.term
   type term_index = TermHash.term_index
   type term_weak_index = TermHash.term_weak_index
   type meta_term = ToTerm.TermType.meta_term
   type meta_term_index = TermHash.meta_term_index
   type meta_term_weak_index = TermHash.meta_term_weak_index
   type msequent = ToTerm.Refine .msequent
   type msequent_index = TermHash.msequent_index
   type msequent_weak_index = TermHash.msequent_weak_index

   let weaken_term = TermHash.weaken_term
   let weaken_meta_term = TermHash.weaken_meta_term
   let weaken_msequent = TermHash.weaken_msequent

   let p_add info t = TermHash.p_lookup info (THC.make_term_header info t)
   let p_normalize info t = TermHash.p_retrieve info (p_add info t)

   let p_add_meta info t = TermHash.p_lookup_meta info (THC.make_meta_term_header info t)
   let p_normalize_meta info t = TermHash.p_retrieve_meta info (p_add_meta info t)

   let p_add_msequent info t = TermHash.p_lookup_msequent info (THC.make_msequent_header info t)
   let p_normalize_msequent info t = TermHash.p_retrieve_msequent info (p_add_msequent info t)

   let p_retrieve = TermHash.p_retrieve
   let p_retrieve_meta = TermHash.p_retrieve_meta
   let p_retrieve_msequent = TermHash.p_retrieve_msequent

   let global_hash = TermHash.global_hash

   let add = TermHash.lookup_term (THC.make_term_header global_hash)
   let normalize t = TermHash.retrieve (add t)
   let retrieve = TermHash.retrieve

   let add_meta t = TermHash.lookup_meta (THC.make_meta_term_header global_hash t)
   let normalize_meta t = TermHash.retrieve_meta (add_meta t)
   let retrieve_meta = TermHash.retrieve_meta

   let add_msequent t = TermHash.lookup_msequent (THC.make_msequent_header global_hash t)
   let normalize_msequent t = TermHash.retrieve_msequent (add_msequent t)
   let retrieve_msequent = TermHash.retrieve_msequent

   let compare_terms = TermHash.compare_terms
   let compare_meta_terms = TermHash.compare_meta_terms
   let compare_msequents = TermHash.compare_msequents
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
