(* This file is an interface for terms' conversion
 * From one Term-module to another
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

module TermCopy2Weak 
   (FromTerm : Termmod_sig.TermModuleSig) 

   (FromHeader : Term_header_sig.TermHeaderSig
      with type term = FromTerm.TermType.term
      with type param = FromTerm.TermType.param
      with type meta_term = FromTerm.TermType.meta_term

      with type 'a descriptor = 'a InfiniteWeakArray.descriptor
      with type 'a weak_descriptor = 'a InfiniteWeakArray.weak_descriptor)

   (FromHash : Term_hash_sig.TermHashSig
      with type param_header = FromHeader.param_header
      with type param_weak_header = FromHeader.param_weak_header
      with type term_header = FromHeader.term_header
      with type term_weak_header = FromHeader.term_weak_header
      with type meta_term_header = FromHeader.meta_term_header
      with type meta_term_weak_header = FromHeader.meta_term_weak_header

      with type param = FromTerm.TermType.param
      with type term = FromTerm.TermType.term
      with type meta_term = FromTerm.TermType.meta_term) 

   (ToTerm : Termmod_sig.TermModuleSig) 

   (ToHeader : Term_header_sig.TermHeaderSig
      with type term = ToTerm.TermType.term
      with type param = ToTerm.TermType.param
      with type meta_term = ToTerm.TermType.meta_term

      with type 'a descriptor = 'a InfiniteWeakArray.descriptor
      with type 'a weak_descriptor = 'a InfiniteWeakArray.weak_descriptor)

   (ToHash : Term_hash_sig.TermHashSig
      with type param_header = ToHeader.param_header
      with type param_weak_header = ToHeader.param_weak_header
      with type term_header = ToHeader.term_header
      with type term_weak_header = ToHeader.term_weak_header
      with type meta_term_header = ToHeader.meta_term_header
      with type meta_term_weak_header = ToHeader.meta_term_weak_header

      with type param = ToTerm.TermType.param
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term) :

sig

(*
 * Pair of Term_hash's structures
 *)
   type t

   val p_create : int -> int -> t

   val p_convert :
      t -> FromTerm.TermType.term -> ToTerm.TermType.term
   val p_revert :
      t -> ToTerm.TermType.term -> FromTerm.TermType.term

   val p_convert_meta :
      t -> FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val p_revert_meta :
      t -> ToTerm.TermType.meta_term -> FromTerm.TermType.meta_term

(*
 * Pair of synonyms to Term_hash's globals
 *)
   val global_hash : t

   val convert : FromTerm.TermType.term -> ToTerm.TermType.term
   val revert : ToTerm.TermType.term -> FromTerm.TermType.term

   val convert_meta :
      FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val revert_meta :
      ToTerm.TermType.meta_term -> FromTerm.TermType.meta_term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
