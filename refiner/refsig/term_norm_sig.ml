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
   type meta_term
   type meta_term_index

   val p_add : t -> term -> term_index
   val p_normalize : t -> term -> term
(*
 * Restore term from its index
 *)
   val p_retrieve :
     t -> term_index -> term

(*
 * Same functions for meta_terms
 *)
   val p_add_meta : t -> meta_term -> meta_term_index
   val p_normalize_meta : t -> meta_term -> meta_term
   val p_retrieve_meta : t -> meta_term_index -> meta_term

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
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
