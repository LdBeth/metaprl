(*
 * Term quotation tester.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

open Printf
open Nl_debug

open Ml_string
open Term_grammar

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_quote%t" eflush


(************************************************************************
 * GRAMMAR                                                              *
 ************************************************************************)

(*
 * Grammar.
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   let rec mk_opname = function
      op::t ->
         Opname.mk_opname op (mk_opname t)
    | [] ->
         Opname.nil_opname

   (*
    * Term grammar.
    *)
   let gram = Grammar.create Plexer.make
   let term_eoi = Grammar.Entry.create gram "term"
   let term = Grammar.Entry.create gram "term"
   let quote_term = Grammar.Entry.create gram "quote_term"
   let mterm = Grammar.Entry.create gram "mterm"
end

module TermGrammar = MakeTermGrammar (TermGrammarBefore)

(************************************************************************
 * QUOTATION                                                            *
 ************************************************************************)

(*
 * String -> string translator.
 *)
let term_exp s =
   let cs = Gstream.of_string s in
   let t = Grammar.Entry.parse TermGrammar.term_eoi cs in
   let file = StringFile.create () in
      StringPrint.print_term file t;
      StringFile.get file

let _ = Quotation.add "term" term_exp
let _ = Quotation.default := "term"

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
