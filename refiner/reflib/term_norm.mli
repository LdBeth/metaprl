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
 * Author: Yegor Bryukhov
 *)

open Term_hash

module TermNorm :
  functor(ToTerm : Termmod_sig.TermModuleSig) ->
sig

(*
 * Adds term to Term_hash's structure and returns term's index in this structure
 *)
   val p_add : TermHash(ToTerm).t -> ToTerm.TermType.term -> TermHash(ToTerm).term_index
(*
 * Normalize term by means of Term_hash structure's data
 * It removes all duplications of term's fragments
 *)
   val p_normalize :
     TermHash(ToTerm).t -> ToTerm.TermType.term -> ToTerm.TermType.term
(*
 * Restore term from its index
 *)
   val p_retrieve :
     TermHash(ToTerm).t -> TermHash(ToTerm).term_index -> ToTerm.TermType.term

(*
 * Same functions for meta_terms
 *)
   val p_add_meta :
     TermHash(ToTerm).t -> ToTerm.TermType.meta_term -> TermHash(ToTerm).meta_term_index
   val p_normalize_meta :
     TermHash(ToTerm).t -> ToTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val p_retrieve_meta :
     TermHash(ToTerm).t -> TermHash(ToTerm).meta_term_index -> ToTerm.TermType.meta_term

(*
 * Synonym to Term_hash's global copy
 *)
   val global_hash : TermHash(ToTerm).t

(*
 * Versions of previous functions operating with global copy of hashing structure
 *)
   val add : ToTerm.TermType.term -> TermHash(ToTerm).term_index
   val normalize : ToTerm.TermType.term -> ToTerm.TermType.term
   val retrieve : TermHash(ToTerm).term_index -> ToTerm.TermType.term

   val add_meta : ToTerm.TermType.meta_term -> TermHash(ToTerm).meta_term_index
   val normalize_meta :
     ToTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val retrieve_meta :
     TermHash(ToTerm).meta_term_index -> ToTerm.TermType.meta_term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
