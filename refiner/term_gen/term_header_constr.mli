(* This file is an interface for term-headers' constructors
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

open Infinite_weak_array

module TermHeaderConstr 
   (FromTerm : Termmod_sig.TermModuleSig) 
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
   val make_param_header :
      TermHash.t -> FromTerm.TermType.param -> TermHeader.param_header
   val make_term_header :
      TermHash.t -> FromTerm.TermType.term -> TermHeader.term_header
   val make_meta_term_header :
      TermHash.t -> FromTerm.TermType.meta_term -> TermHeader.meta_term_header
end
