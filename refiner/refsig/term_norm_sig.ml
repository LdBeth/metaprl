(* This file is an interface for terms' normalization (DAG-ization)
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

module type TermNormSig =
sig

   type t
   type term
   type term_index
   type term_weak_index
   type meta_term
   type meta_term_index
   type meta_term_weak_index
   type msequent
   type msequent_index
   type msequent_weak_index

   (*
    * Allow clients to weaken.
    *)
   val weaken_term : term_index -> term_weak_index
   val weaken_meta_term : meta_term_index -> meta_term_weak_index
   val weaken_msequent : msequent_index -> msequent_weak_index

   (*
    * Convert terms.
    *)
   val p_add : t -> term -> term_index
   val p_normalize : t -> term -> term
   val p_retrieve : t -> term_index -> term

   (*
    * Same functions for meta_terms
    *)
   val p_add_meta : t -> meta_term -> meta_term_index
   val p_normalize_meta : t -> meta_term -> meta_term
   val p_retrieve_meta : t -> meta_term_index -> meta_term

   (*
    * Same functions for msequents.
    *)
   val p_add_msequent : t -> msequent -> msequent_index
   val p_normalize_msequent : t -> msequent -> msequent
   val p_retrieve_msequent : t -> msequent_index -> msequent

   (*
    * Synonym to Term_hash's global copy
    *)
   val global_hash : t

   (*
    * Versions of previous functions operating with global copy of hashing structure
    *)
   val add : term -> term_index
   val normalize : term -> term
   val retrieve : term_index -> term

   val add_meta : meta_term -> meta_term_index
   val normalize_meta : meta_term -> meta_term
   val retrieve_meta : meta_term_index -> meta_term

   val add_msequent : msequent -> msequent_index
   val normalize_msequent : msequent -> msequent
   val retrieve_msequent : msequent_index -> msequent

   (*
    * Use of this function may is not advised for casual users.
    *)
   val compare_terms : term_index -> term_index -> int
   val compare_meta_terms : meta_term_index -> meta_term_index -> int
   val compare_msequents : msequent_index -> msequent_index -> int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
