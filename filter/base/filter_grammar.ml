(*
 * Just add the infixes to the current grammar.
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

open Mp_debug
open Printf

open MLast
open Pcaml

open Infix

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_grammar%t"

(*
 * Unit is just used as a dummy.
 *)
module type UnitSig =
sig
end

(*
 * Enclose the grammar in a functor so that it is not always evaluated.
 *)
module MakeFilterGrammar (Unit : UnitSig) =
struct
   EXTEND
       GLOBAL: expr;

      (*
       * Pre-add some infix operators.
       *)
      expr: AFTER "expr1" (**)
         [LEFTA
          [ t1 = expr; op = "THEN"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "THENL"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "orelseT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "andalsoT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "orthenT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenLT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenFLT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "then_OnEachT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "then_OnFirstT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "then_OnLastT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "then_OnSameConclT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenLabLT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenMT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenMLT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenAT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenALT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenWT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenET"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenPT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenC"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "orelseC"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "..." ->
             <:expr< $lid:"tryAutoT"$ $t1$ >>
          ]];
   END
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
