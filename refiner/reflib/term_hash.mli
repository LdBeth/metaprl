(* This file is an interface for terms' recursive hashing module
 * based on weak arrays
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

open Weak_memo
open Term_header
open Infinite_weak_array

module type TermHashSig =
sig

   module ToTermPar : Termmod_sig.TermModuleSig

(*
 * Objects of this types refers to terms and meta_terms and prevents objects them
 * refered to from GC
 *)
   type term_index
   type meta_term_index

(*
 * term's hashing structure
 *)
   type t =
      { param_hash     : (t, TermHeader(ToTermPar).param_header,
                             TermHeader(ToTermPar).param_weak_header,
                             ToTermPar.TermType.param
                         ) WeakMemo(Simplehashtbl.Simplehashtbl)(InfiniteWeakArray).t;
        opname_hash    : (t, Opname.opname,
                             Opname.opname,
                             Opname.opname
                         ) WeakMemo(Simplehashtbl.Simplehashtbl)(InfiniteWeakArray).t;
        term_hash      : (t, TermHeader(ToTermPar).term_header,
                             TermHeader(ToTermPar).term_weak_header,
                             ToTermPar.TermType.term
                         ) WeakMemo(Simplehashtbl.Simplehashtbl)(InfiniteWeakArray).t;
        meta_term_hash : (t, TermHeader(ToTermPar).meta_term_header,
                             TermHeader(ToTermPar).meta_term_weak_header,
                             ToTermPar.TermType.meta_term
                         ) WeakMemo(Simplehashtbl.Simplehashtbl)(InfiniteWeakArray).t
      }

(*
 * Construct term-objects from headers
 *)    
   val p_constr_param : t -> TermHeader(ToTermPar).param_header -> ToTermPar.TermType.param
   val p_constr_term : t -> TermHeader(ToTermPar).term_header -> ToTermPar.TermType.term
   val p_constr_meta_term : t -> TermHeader(ToTermPar).meta_term_header -> ToTermPar.TermType.meta_term

(*
 * Creates new hashing structure
 *)
   val p_create : int -> int -> t

(*
 * Functions for storing and accessing objects to hashing structure
 *)
   val p_lookup : t -> TermHeader(ToTermPar).term_header -> term_index
   val p_unsafe_lookup : t -> TermHeader(ToTermPar).term_header -> term_index
   val p_retrieve : t -> term_index -> ToTermPar.TermType.term

   val p_lookup_meta : t -> TermHeader(ToTermPar).meta_term_header -> meta_term_index
   val p_unsafe_lookup_meta : t -> TermHeader(ToTermPar).meta_term_header -> meta_term_index
   val p_retrieve_meta : t -> meta_term_index -> ToTermPar.TermType.meta_term

(*
 * Globally accessible copy
 *)
   val global_hash : t

(*
 * As previous but operate with global copy of data
 *)
   val lookup : TermHeader(ToTermPar).term_header -> term_index
   val unsafe_lookup : TermHeader(ToTermPar).term_header -> term_index
   val retrieve : term_index -> ToTermPar.TermType.term

   val lookup_meta : TermHeader(ToTermPar).meta_term_header -> meta_term_index
   val unsafe_lookup_meta : TermHeader(ToTermPar).meta_term_header -> meta_term_index
   val retrieve_meta : meta_term_index -> ToTermPar.TermType.meta_term
end

module TermHash :
  functor(ToTerm : Termmod_sig.TermModuleSig) -> (TermHashSig with module ToTermPar = ToTerm)

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_norm, term_copy_weak"
 * End:
 * -*-
 *)
