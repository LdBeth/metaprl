(* This file is an interface for terms' normalization (DAG-ization)
 * It introduces interface for storing and accessing terms stored
 * in Term_hash's structure
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

open Weak_memo

module TermNorm
   (ToTerm : Termmod_sig.TermModuleInternalSig)

   (TermHash : Term_hash_sig.TermHashInternalSig
      with type param = ToTerm.TermType.param
      with type param' = ToTerm.TermType.param'
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term
      with type msequent = ToTerm.Refine .msequent) :

   Term_norm_sig.TermNormSig
      with type t = TermHash.t
      with type term = ToTerm.TermType.term
      with type term_index = TermHash.term_index
      with type meta_term = ToTerm.TermType.meta_term
      with type meta_term_index = TermHash.meta_term_index
      with type msequent = ToTerm.Refine .msequent
      with type msequent_index = TermHash.msequent_index

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
