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

module TermCopy2Weak :
  functor(FromTerm : Termmod_sig.TermModuleSig) ->
  functor(ToTerm : Termmod_sig.TermModuleSig) ->
  functor(SourceHash : Term_hash.TermHashSig with module ToTermPar = FromTerm) ->
  functor(TargetHash : Term_hash.TermHashSig with module ToTermPar = ToTerm) ->
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
(*
 * Distinguished conversions
 *)
val normalize_term :
  Refiner_std.Refiner.TermType.term -> Refiner.Refiner.TermType.term
val normalize_meta_term :
  Refiner_std.Refiner.TermType.meta_term ->
  Refiner.Refiner.TermType.meta_term
val denormalize_term :
  Refiner.Refiner.TermType.term -> Refiner_std.Refiner.TermType.term
val denormalize_meta_term :
  Refiner.Refiner.TermType.meta_term ->
  Refiner_std.Refiner.TermType.meta_term
*)
(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
