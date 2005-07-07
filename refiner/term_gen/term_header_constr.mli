(* This file is an interface for term-headers' constructors
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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


module TermHeaderConstr
   (FromTerm : Termmod_sig.TermModuleSig)
   (ToTerm : Termmod_sig.TermModuleSig)

   (TermHash : Term_hash_sig.TermHashSig
      with type param = ToTerm.TermType.param
      with type param' = ToTerm.TermType.param'
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term
      with type msequent = ToTerm.Refine .msequent) :

sig
   val make_term_header :
      TermHash.t -> FromTerm.TermType.term -> TermHash.term_header
   val make_meta_term_header :
      TermHash.t -> FromTerm.TermType.meta_term -> TermHash.meta_term_header
   val make_msequent_header :
      TermHash.t -> FromTerm.Refine .msequent -> TermHash.msequent_header
end
