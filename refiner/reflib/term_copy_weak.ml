(* This file implements terms' conversion from one Term-module
 * to another
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

open Term_hash
open Term_header_constr

module TermCopyWeak =
  functor(FromTerm : Termmod_sig.TermModuleSig) ->
  functor(ToTerm   : Termmod_sig.TermModuleSig) ->
struct

   module TermHash = Term_hash.TermHash(ToTerm)
   module THC = TermHeaderConstr(FromTerm)(ToTerm)

   let p_add info t = TermHash.p_lookup info (THC.make_term_header info t)
   let p_convert info t = TermHash.p_retrieve info (p_add info t)

   let p_add_meta info t = TermHash.p_lookup_meta info (THC.make_meta_term_header info t)
   let p_convert_meta info t = TermHash.p_retrieve_meta info (p_add_meta info t)

   let p_retrieve = TermHash.p_retrieve
   let p_retrieve_meta = TermHash.p_retrieve_meta

   let global_hash = TermHash.global_hash

   let add = p_add TermHash.global_hash
   let convert = p_convert TermHash.global_hash

   let add_meta = p_add_meta TermHash.global_hash
   let convert_meta = p_convert_meta TermHash.global_hash

   let retrieve = TermHash.retrieve
   let retrieve_meta = TermHash.retrieve_meta

end

module NormalizeTerm =
   TermCopyWeak (Refiner_std.Refiner) (Refiner.Refiner)

module DenormalizeTerm =
   TermCopyWeak (Refiner.Refiner) (Refiner_std.Refiner)

let normalize_term = NormalizeTerm.convert
let normalize_meta_term = NormalizeTerm.convert_meta
let denormalize_term = DenormalizeTerm.convert
let denormalize_meta_term = DenormalizeTerm.convert_meta

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
