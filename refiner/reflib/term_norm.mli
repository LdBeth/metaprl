(* This file is an interface for terms' normalization (DAG-ization)
 * It introduces interface for storing and accessing terms stored
 * in Term_hash's structure
 *
 * -----------------------------------------------------------------
 * This file is part of Nuprl-Light, a modular, higher order
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

open Infinite_weak_array

module TermNorm 
   (ToTerm : Termmod_sig.TermModuleSig) 

   (TermHeader : Term_header_sig.TermHeaderSig
      with type term = ToTerm.TermType.term
      with type param = ToTerm.TermType.param
      with type meta_term = ToTerm.TermType.meta_term

      with type 'a descriptor = 'a InfiniteWeakArray.descriptor
      with type 'a weak_descriptor = 'a InfiniteWeakArray.weak_descriptor)

   (TermHash : Term_hash_sig.TermHashSig
      with type param_header = TermHeader.param_header
      with type param_weak_header = TermHeader.param_weak_header
      with type term_header = TermHeader.term_header
      with type term_weak_header = TermHeader.term_weak_header
      with type meta_term_header = TermHeader.meta_term_header
      with type meta_term_weak_header = TermHeader.meta_term_weak_header

      with type param = ToTerm.TermType.param
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term) :

sig

(*
 * Adds term to Term_hash's structure and returns term's index in this structure
 *)
   val p_add : TermHash.t -> ToTerm.TermType.term -> TermHash.term_index
(*
 * Normalize term by means of Term_hash structure's data
 * It removes all duplications of term's fragments
 *)
   val p_normalize :
     TermHash.t -> ToTerm.TermType.term -> ToTerm.TermType.term
(*
 * Restore term from its index
 *)
   val p_retrieve :
     TermHash.t -> TermHash.term_index -> ToTerm.TermType.term

(*
 * Same functions for meta_terms
 *)
   val p_add_meta :
     TermHash.t -> ToTerm.TermType.meta_term -> TermHash.meta_term_index
   val p_normalize_meta :
     TermHash.t -> ToTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val p_retrieve_meta :
     TermHash.t -> TermHash.meta_term_index -> ToTerm.TermType.meta_term

(*
 * Synonym to Term_hash's global copy
 *)
   val global_hash : TermHash.t

(*
 * Versions of previous functions operating with global copy of hashing structure
 *)
   val add : ToTerm.TermType.term -> TermHash.term_index
   val normalize : ToTerm.TermType.term -> ToTerm.TermType.term
   val retrieve : TermHash.term_index -> ToTerm.TermType.term

   val add_meta : ToTerm.TermType.meta_term -> TermHash.meta_term_index
   val normalize_meta :
     ToTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val retrieve_meta :
     TermHash.meta_term_index -> ToTerm.TermType.meta_term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
