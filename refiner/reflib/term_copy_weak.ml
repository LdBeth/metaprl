(* This file implements terms' conversion from one Term-module
 * to another
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
open Termmod_sig
open Termmod_hash_sig

module TermCopyWeak
   (FromTerm : TermModuleSig)
   (ToTerm : TermModuleHashSig) =
struct
   module TermHash = ToTerm.TermHash
   module THC = ToTerm.TermHeaderConstr (FromTerm)

   let p_add info t = TermHash.p_lookup info (THC.make_term_header info t)
   let p_convert info t = TermHash.p_retrieve info (p_add info t)

   let p_add_meta info t = TermHash.p_lookup_meta info (THC.make_meta_term_header info t)
   let p_convert_meta info t = TermHash.p_retrieve_meta info (p_add_meta info t)

   let p_add_msequent info t = TermHash.p_lookup_msequent info (THC.make_msequent_header info t)
   let p_convert_msequent info t = TermHash.p_retrieve_msequent info (p_add_msequent info t)

   let p_retrieve = TermHash.p_retrieve
   let p_retrieve_meta = TermHash.p_retrieve_meta
   let p_retrieve_msequent = TermHash.p_retrieve_msequent

   let global_hash = TermHash.global_hash

   let add = p_add TermHash.global_hash
   let convert = p_convert TermHash.global_hash

   let add_meta = p_add_meta TermHash.global_hash
   let convert_meta = p_convert_meta TermHash.global_hash

   let add_msequent = p_add_msequent TermHash.global_hash
   let convert_msequent = p_convert_msequent TermHash.global_hash

   let retrieve = TermHash.retrieve
   let retrieve_meta = TermHash.retrieve_meta
   let retrieve_msequent = TermHash.retrieve_msequent
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
