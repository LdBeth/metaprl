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

open Term_hash

module TermNorm :
  functor(ToTerm : Termmod_sig.TermModuleSig) ->
  functor(TermHash : TermHashSig with module ToTermPar = ToTerm) ->
sig

(*
 * Adds term to Term_hash's structure and returns term's index in this structure
 *)
   val p_add : TermHash.t -> TermHash.ToTermPar.TermType.term -> TermHash.term_index
(*
 * Normalize term by means of Term_hash structure's data
 * It removes all duplications of term's fragments
 *)
   val p_normalize :
     TermHash.t -> TermHash.ToTermPar.TermType.term -> TermHash.ToTermPar.TermType.term
(*
 * Restore term from its index
 *)
   val p_retrieve :
     TermHash.t -> TermHash.term_index -> TermHash.ToTermPar.TermType.term

(*
 * Same functions for meta_terms
 *)
   val p_add_meta :
     TermHash.t -> TermHash.ToTermPar.TermType.meta_term -> TermHash.meta_term_index
   val p_normalize_meta :
     TermHash.t -> TermHash.ToTermPar.TermType.meta_term -> TermHash.ToTermPar.TermType.meta_term
   val p_retrieve_meta :
     TermHash.t -> TermHash.meta_term_index -> TermHash.ToTermPar.TermType.meta_term

(*
 * Synonym to Term_hash's global copy
 *)
   val global_hash : TermHash.t

(*
 * Versions of previous functions operating with global copy of hashing structure
 *)
   val add : TermHash.ToTermPar.TermType.term -> TermHash.term_index
   val normalize : TermHash.ToTermPar.TermType.term -> TermHash.ToTermPar.TermType.term
   val retrieve : TermHash.term_index -> TermHash.ToTermPar.TermType.term

   val add_meta : TermHash.ToTermPar.TermType.meta_term -> TermHash.meta_term_index
   val normalize_meta :
     TermHash.ToTermPar.TermType.meta_term -> TermHash.ToTermPar.TermType.meta_term
   val retrieve_meta :
     TermHash.meta_term_index -> TermHash.ToTermPar.TermType.meta_term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
