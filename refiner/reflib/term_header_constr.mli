(* This file is an interface for term-headers' constructors
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

open Term_header
open Term_hash

module TermHeaderConstr :
  functor(FromTerm : Termmod_sig.TermModuleSig) ->
  functor(ToTerm : Termmod_sig.TermModuleSig) ->
sig
   val make_param_header :
      TermHash(ToTerm).t -> FromTerm.TermType.param -> TermHeader(ToTerm).param_header
   val make_term_header :
      TermHash(ToTerm).t -> FromTerm.TermType.term -> TermHeader(ToTerm).term_header
   val make_meta_term_header :
      TermHash(ToTerm).t -> FromTerm.TermType.meta_term -> TermHeader(ToTerm).meta_term_header
end
