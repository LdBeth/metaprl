(* This file is an interface for terms' recursive hashing module
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
open Lm_symbol

module type TermHashSig =
sig
   DEFTOPMACRO TERM_HASH_SIG =

      type param
      type param'
      type term
      type hashed_param
      type meta_term
      type msequent

      (*
       * Objects of these types describe terms and meta_terms without
       * the objects themselves being referred to from GC.
       *)
      type term_index
      type meta_term_index
      type msequent_index

   (*
      type hypothesis_header
      type bound_term_header
      type true_term_header
      type seq_header
      type term_header
      type meta_term_header
      type msequent_header
   *)

      (*
       * XXX HACK! jyh: these are here temporarily until Alexey gets rid of them.
       *)
      type hypothesis_header =
         Hypothesis of term_index
       | HypBinding of var * term_index
       | Context of var * var list * term_index list

      type bound_term_header =
         { bvars: var list;
           bterm: term_index
         }

      type true_term_header =
         { op_name: Opname.opname;
           op_params: hashed_param list;
           term_terms: bound_term_header list
         }

      type seq_header =
         { seq_arg: term_index;
           seq_hyps: hypothesis_header list;
           seq_goals: term_index list
         }

      type term_header =
         Term of true_term_header
       | Seq of seq_header
       | FOVar of var
       | SOVar of var * var list * term_index list

      type meta_term_header =
         MetaTheorem of term_index
       | MetaImplies of meta_term_index * meta_term_index
       | MetaFunction of term_index * meta_term_index * meta_term_index
       | MetaIff of meta_term_index * meta_term_index
       | MetaLabeled of string * meta_term_index

      type msequent_header =
         term_index list * term_index

      (*
       * Allow clients to weaken.
       *)
      type term_weak_index
      type meta_term_weak_index
      type msequent_weak_index

      val weaken_term : term_index -> term_weak_index
      val weaken_meta_term : meta_term_index -> meta_term_weak_index
      val weaken_msequent : msequent_index -> msequent_weak_index

      (*
       * term's hashing structure
       *)
      type t

      (*
       * Construct term-objects from headers
       *)
      val p_constr_param : t -> param' -> hashed_param
      val p_constr_term : t -> term_header -> term
      val p_constr_meta_term : t -> meta_term_header -> meta_term
      val p_constr_msequent : t -> msequent_header -> msequent

      (*
       * Creates new hashing structure
       *)
      val p_create : int -> (unit -> unit) list -> t

      val p_add_cc : t -> (unit -> unit) list -> unit
      val p_get_cc : t -> (unit -> unit) list

      (*
       * Functions for storing and accessing objects to hashing structure
       *)
      val p_lookup : t -> term_header -> term_index
      val p_unsafe_lookup : t -> term_header -> term_index
      val p_retrieve : t -> term_index -> term

      val p_lookup_meta : t -> meta_term_header -> meta_term_index
      val p_unsafe_lookup_meta : t -> meta_term_header -> meta_term_index
      val p_retrieve_meta : t -> meta_term_index -> meta_term

      val p_lookup_msequent : t -> msequent_header -> msequent_index
      val p_unsafe_lookup_msequent : t -> msequent_header -> msequent_index
      val p_retrieve_msequent : t -> msequent_index -> msequent

      (*
       * Globally accessible copy
       *)
      val global_hash : t

      (*
       * As previous but operate with global copy of data
       *)
      val constr_param : param' -> hashed_param

      val lookup : term_header -> term_index
      val unsafe_lookup : term_header -> term_index
      val retrieve : term_index -> term

      val lookup_meta : meta_term_header -> meta_term_index
      val unsafe_lookup_meta : meta_term_header -> meta_term_index
      val retrieve_meta : meta_term_index -> meta_term

      val lookup_msequent : msequent_header -> msequent_index
      val unsafe_lookup_msequent : msequent_header -> msequent_index
      val retrieve_msequent : msequent_index -> msequent

      (*
       * Use of this function is not advised for casual users.
       *)
      val compare_terms : term_index -> term_index -> int
      val compare_meta_terms : meta_term_index -> meta_term_index -> int
      val compare_msequents : msequent_index -> msequent_index -> int

      (*
       * In case we want to index something by terms
       *)
      module HashTerm : Hashtbl.S with type key = term_index
      module HashBTerm : Hashtbl.S with type key = bound_term_header
      module HashHyp : Hashtbl.S with type key = hypothesis_header

   END

   USETOPMACRO TERM_HASH_SIG END
end

module type TermHashInternalSig =
sig
   USETOPMACRO TERM_HASH_SIG END

   (*
    * This is included for a performance optimization on
    * terms, since the term may store its index internally.
    *)
   val lookup_term : (term -> term_header) -> term -> term_index

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_norm, term_copy_weak"
 * End:
 * -*-
 *)
