(* This file is an interface for terms' recursive hashing module
 * based on weak arrays
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
open Term_hash_sig

module TermHash (ToTerm : Termmod_sig.TermModuleInternalSig) :
   TermHashInternalSig
      with type param = ToTerm.TermType.param
      with type param' = ToTerm.TermType.param'
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term
      with type msequent = ToTerm.Refine .msequent

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_norm, term_copy_weak"
 * End:
 * -*-
 *)
