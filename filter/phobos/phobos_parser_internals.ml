(*
 * Generic parser internals.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2001 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)

open Phobos_type

(*
 * Parser's stack.
 *)
let stack_top = Stack.top

let stack_push stack el =
   Stack.push el stack;
   stack

let stack_pop stack =
   let _ = Stack.pop stack in
      stack

(*
 * Pop |lst|-many pairs and return them.
 *)
let state_of_stack_symbol = function
   Sta_state i ->
      i
 | Sta_term _ ->
      raise (Invalid_argument "state_of_stack_symbol: not a state")

let term_of_stack_symbol = function
   Sta_term term ->
      term
 | Sta_state _ ->
      raise (Invalid_argument "term_of_stack_symbol: not a term")

let stack_pop_list stack lst =
   List.fold_left (fun (stack, states, terms) _ ->
      let state = Stack.pop stack in
      let term = Stack.pop stack in
         stack, (state_of_stack_symbol state) :: states, (**)
            (term_of_stack_symbol term) :: terms) (stack, [], []) lst

let stack_size = Stack.length

let current_state_of_stack stack =
   match stack_top stack with
      Sta_state i ->
         i
    | Sta_term _ ->
         raise (Invalid_argument "current_state_of_stack")

(* Destroys stack *)
let result_term_of_stack stack =
   let stack = stack_pop stack in
      match stack_top stack with
         Sta_state _ ->
            raise (Invalid_argument "result_term_of_stack")
       | Sta_term term ->
            term

let is_epsilon_transformation = function
   [Empty] ->
      true
 | _ ->
      false

(*
 * Return if there are only two problems, and
 * each are Reduce/Reduce or Shift/Reduce.
 *)
let is_rr_problem = function
   (Reduce _) :: [(Reduce _)] ->
      true
 | _ ->
      false

let is_sr_problem = function
   (Reduce prod_id) :: [(Shift i)]
 | (Shift i) :: [Reduce prod_id] ->
      true, Some (prod_id, i)
 | _ ->
      false, None

(*
 * If there are more than one actions to choose from,
 * pick Shift from Shift/Reduce, otherwise pick first one.
 *)
let select_action = function
   a :: [] ->
      a
 | a :: [b] ->
      (match a, b with
         (* Resolve Shift/Reduce as Shift *)
         Shift _, Reduce _ ->
            a
       | Reduce _, Shift _ ->
            b
         (* Otherwise give up *)
       | _ ->
            a)
 | a :: rest ->
      a
 | [] ->
      Error

