(*
 * Display all the elements in a particular theory.
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
 * Copyright C 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or at your option any later version.
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

include Itt_theory

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Itt_int

let dest_level_param t =
   let { term_op = op } = dest_term t in
      match dest_op op with
         { op_params = [param] } ->
            begin
               match dest_param param with
                  MLevel s ->
                     s
                | _ ->
                     raise (RefineError ("dest_level_param", TermMatchError (t, "param type")))
            end
       | { op_params = [] } ->
            raise (RefineError ("dest_level_param", TermMatchError (t, "no params")))
       | _ ->
            raise (RefineError ("dest_level_param", TermMatchError (t, "too many params")))

ml_rw test_rw : add{number[i:n]; number[j:n]} == fun
   goal ->
   let i = dest_number <:con< number[i:n] >> in
   let j = dest_number <:con< number[j:n] >> in
      mk_number_term (Mp_num.add_num i j), []
 | fun _ extracts ->
   << it >>, extracts

ml_rule cumulativity 'H :
   sequent ['ext] { 'H >- cumulativity[j:l, i:l] } == fun
   goal ->
   let i = dest_level_param <:con< univ[i:l] >> in
   let j = dest_level_param <:con< univ[j:l] >> in
      if level_le j i then
         []
      else
         raise (RefineError ("cumulativity", StringError "failed"))

 | fun _ extracts ->
      << it >>, extracts

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
