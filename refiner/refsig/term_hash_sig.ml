(* This file is an interface for terms' recursive hashing module
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

module type TermHashSig =
sig
   
   type param_header
   type param_weak_header
   type term_header
   type term_weak_header
   type meta_term_header
   type meta_term_weak_header

   type param
   type term
   type meta_term

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
      { param_hash     : (t, param_header,
                             param_weak_header,
                             param
                         ) TheWeakMemo.t;
        opname_hash    : (t, Opname.opname,
                             Opname.opname,
                             Opname.opname
                         ) TheWeakMemo.t;
        term_hash      : (t, term_header,
                             term_weak_header,
                             term
                         ) TheWeakMemo.t;
        meta_term_hash : (t, meta_term_header,
                             meta_term_weak_header,
                             meta_term
                         ) TheWeakMemo.t
      }

(*
 * Construct term-objects from headers
 *)    
   val p_constr_param : t -> param_header -> param
   val p_constr_term : t -> term_header -> term
   val p_constr_meta_term : t -> meta_term_header -> meta_term

(*
 * Creates new hashing structure
 *)
   val p_create : int -> int -> t

(*
 * Functions for storing and accessing objects to hashing structure
 *)
   val p_lookup : t -> term_header -> term_index
   val p_unsafe_lookup : t -> term_header -> term_index
   val p_retrieve : t -> term_index -> term

   val p_lookup_meta : t -> meta_term_header -> meta_term_index
   val p_unsafe_lookup_meta : t -> meta_term_header -> meta_term_index
   val p_retrieve_meta : t -> meta_term_index -> meta_term

(*
 * Globally accessible copy
 *)
   val global_hash : t

(*
 * As previous but operate with global copy of data
 *)
   val lookup : term_header -> term_index
   val unsafe_lookup : term_header -> term_index
   val retrieve : term_index -> term

   val lookup_meta : meta_term_header -> meta_term_index
   val unsafe_lookup_meta : meta_term_header -> meta_term_index
   val retrieve_meta : meta_term_index -> meta_term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_norm, term_copy_weak"
 * End:
 * -*-
 *)
