(*
 * A grammar for terms as quotatations.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Opname
open Refiner.Refiner
open Term
open TermMeta
open TermShape
open Filter_type

(*
 * Productions to extend.
 *)
module type TermGrammarSig =
sig
   val mk_opname : MLast.loc -> opname_fun
   val term_eoi : term Grammar.Entry.e
   val term : term Grammar.Entry.e
   val quote_term : quote_term Grammar.Entry.e
   val mterm : meta_term Grammar.Entry.e
   val bmterm : meta_term Grammar.Entry.e
   val singleterm : aterm Grammar.Entry.e
   val bound_term : aterm Grammar.Entry.e
   val xdform : term Grammar.Entry.e
end

module MakeTermGrammar (TermGrammar : TermGrammarSig) : TermGrammarSig

(*
 * Debugging.
 *)
val debug_grammar : bool ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
