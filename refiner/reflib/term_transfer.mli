(* This file is part of Nuprl-Light, a modular, higher order
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

open Termmod_sig
open Simplehash_sig
open Bi_memo
open Term_compare_sig

module MakeTermCopy :
  functor(Hash : SimpleHashSig) ->
  functor(TermCompare: TermCompareSig) ->
  functor(FromTerm : TermModuleSig) ->
  functor(ToTerm : TermModuleSig) ->
sig

   type t

   val create : unit -> t

   val copy_term : t -> FromTerm.TermType.term -> ToTerm.TermType.term
   val back_term : t -> ToTerm.TermType.term -> FromTerm.TermType.term

   val copy_meta_term : t -> FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val back_meta_term : t -> ToTerm.TermType.meta_term -> FromTerm.TermType.meta_term

   val copy_term_single : FromTerm.TermType.term -> ToTerm.TermType.term
   val back_term_single : ToTerm.TermType.term -> FromTerm.TermType.term

   val copy_meta_term_single : FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val back_meta_term_single : ToTerm.TermType.meta_term -> FromTerm.TermType.meta_term

end

