(* This file is an interface for terms' conversion
 * From one Term-module to another
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
open Refiner_sig

module TermCopyWeak
   (FromTerm : RefinerSig)
   (ToTerm : RefinerSig) :
sig
   (*
    * Convert terms and meta_terms from FromTerm-module to ToTerm-module
    *)
   val p_add :
      ToTerm.TermHash.t -> FromTerm.TermType.term -> ToTerm.TermHash.term_index
   val p_convert :
      ToTerm.TermHash.t -> FromTerm.TermType.term -> ToTerm.TermType.term

   val p_add_meta :
      ToTerm.TermHash.t -> FromTerm.TermType.meta_term -> ToTerm.TermHash.meta_term_index
   val p_convert_meta :
      ToTerm.TermHash.t -> FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term

   val p_add_msequent :
      ToTerm.TermHash.t -> FromTerm.Refine.msequent -> ToTerm.TermHash.msequent_index
   val p_convert_msequent :
      ToTerm.TermHash.t -> FromTerm.Refine.msequent -> ToTerm.Refine.msequent

   (*
    * Same functions operating with global hashing structure
    *)
   val add : FromTerm.TermType.term -> ToTerm.TermHash.term_index
   val convert : FromTerm.TermType.term -> ToTerm.TermType.term

   val add_meta : FromTerm.TermType.meta_term -> ToTerm.TermHash.meta_term_index
   val convert_meta : FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term

   val add_msequent : FromTerm.Refine.msequent -> ToTerm.TermHash.msequent_index
   val convert_msequent : FromTerm.Refine.msequent -> ToTerm.Refine.msequent
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
