(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)

open Printf
open Mp_debug
open Refiner.Refiner.Term
open Evaluator

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Itt_redrules%t"

include Itt_theory

(*
let _ = add_simple_rule <<apply{lambda{x.'b};'a}>> (make_1subst_term <<'b>> "x" <<'a>>)

let _ = add_simple_rule <<spread{'a,'b;x,y.'t}>> (make_2subst_term <<'t>> "x" "y" <<'a>> <<'b>>)

let _ = add_simple_rule <<decide{inl{'a};x.'s;y.'t}>> (make_1subst_term <<'s>> "a" <<'x>>)

let _ = add_simple_rule <<decide{inr{'b};x.'s;y.'t}>> (make_1subst_term <<'t>> "b" <<'y>>)

let _ = add_simple_rule <<list_ind{nil;'s;x,y,u.'t}>> <<'s>>

let _ = add_simple_rule <<list_ind{cons{'a;'b};'s;x,y,u.'t}>>
           (make_subst_term <<'t>> ["x";"y";"u"] [<<'a>>;<<'b>>;<<list_ind{'b;'s;x,y,u.'t}>>])

let _ = add_reduction_rule <<add{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (n1+n2) |
              _ -> raise (Failure "Evaluator: ITT!add"))

let _ = add_reduction_rule <<sub{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (n1-n2) |
              _ -> raise (Failure "Evaluator: ITT!sub"))

let _ = add_reduction_rule <<mul{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (n1*n2) |
              _ -> raise (Failure "Evaluator: ITT!mul"))

let _ = add_reduction_rule <<div{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (if n2=0 then 0 else n1/n2) |
              _ -> raise (Failure "Evaluator: ITT!div"))

let _ = add_reduction_rule <<rem{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (if n2=0 then 0 else n1 mod n2) |
              _ -> raise (Failure "Evaluator: ITT!rem"))

let _ =
   let make_ind_term n = mk_ind_term (mk_natural_number_term n) "x" "y" <<'s>> <<'b>> "u" "v" <<'t>> in
   add_reduction_rule <<ind{natural_number[@n:n];x,y.'s;'b;u,v.'t}>>
           (function
              [Number n] ->
                 if n=0 then <<'b>> else
                 if n>0 then make_2subst_term <<'t>> "u" "v" (mk_natural_number_term n) (make_ind_term (n-1))
                 else make_2subst_term <<'s>> "x" "y" (mk_natural_number_term n) (make_ind_term (n+1)) |
              _ ->  raise (Failure "Evaluator: ITT!ind"))

*)


