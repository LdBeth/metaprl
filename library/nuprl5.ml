(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
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
 * Authors: Lori Lorigo, Richard Eaton
 *)

open Refine_error_sig

open Printf
open Lm_debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Opname
open Lm_num

let _ =
   show_loading "Loading Nuprl5%t"

let nuprl5_opname = mk_opname "!nuprl5_implementation!" nil_opname
let nuprl5_opname_p opname = opname = nuprl5_opname

(* parameter mapping *)

let make_bool_parameter b =
  make_param (ParamList
		[(make_param (Token "bool")); (make_param (Number (Lm_num.num_of_int (if b then 1 else 0))))])

let make_time_parameter time =
  make_param (ParamList
		[(make_param (Token "time")); (make_param (Number time))])

let time_parameter_p p =
  match (dest_param p) with
    ParamList [h; a; b] -> (match (dest_param h) with
      Token s -> if s = "time" then (match (dest_param a) with
	Number i -> (match (dest_param b) with
	  Number i -> true
      	| _ -> false)
      | _ -> false) else false
    | _ -> false)
  | _ -> false

let bool_parameter_p p =
  match (dest_param p) with
    ParamList [h; v] -> (match (dest_param h) with
      Token s -> if s = "bool" then (match (dest_param v) with
	Number i when Lm_num.is_integer_num i -> let i = Lm_num.int_of_num i in (i = 1 || i = 0)
      | _ -> false) else false
    | _ -> false)
  | _ -> false

let destruct_time_parameter p =
  match (dest_param p) with
    ParamList [h; n] -> (match (dest_param h) with
      Token s -> (if s = "time" then (match (dest_param n) with
	Number i -> i
      | _ -> raise (Invalid_argument "destruct_time_parameter_b"))
      else raise (Invalid_argument "destruct_time_parameter_c"))
    | _ -> raise (Invalid_argument "destruct_time_parameter_d"))
  | _ -> raise (Invalid_argument "destruct_time_parameter_e")


let destruct_bool_parameter p =
  match (dest_param p) with
    ParamList [h; v] -> (match (dest_param h) with
      Token s -> if s = "bool" then (match (dest_param v) with
	Number i when Lm_num.is_integer_num i -> let i = Lm_num.int_of_num i in i = 1
      | _ -> raise (Invalid_argument "destruct_bool_parameter"))
      else raise (Invalid_argument "destruct_bool_parameter")
    | _ -> raise (Invalid_argument "destruct_bool_parameter"))
  | _ -> raise (Invalid_argument "destruct_bool_parameter")


(* common terms *)

let mk_nuprl5_op pl = mk_op nuprl5_opname pl
let mk_nuprl5_simple_op s = mk_op nuprl5_opname [(make_param (Token s))]

let nuprl_is_op_term opname term =
    match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = _
    } when term_op' = opname -> true
    | _ -> false

let nuprl_is_not_term = nuprl_is_op_term (mk_nuprl5_simple_op "not")
let nuprl_is_exists_term = nuprl_is_op_term (mk_nuprl5_simple_op "exists")
let nuprl_is_or_term = nuprl_is_op_term (mk_nuprl5_simple_op "or")
let nuprl_is_and_term = nuprl_is_op_term (mk_nuprl5_simple_op "and")
let nuprl_is_implies_term = nuprl_is_op_term (mk_nuprl5_simple_op "implies")
let nuprl_is_all_term = nuprl_is_op_term (mk_nuprl5_simple_op "all")

let nuprl_is_var_term term =
  match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = _ } ->
    match dest_op term_op' with
    {op_name = opname; op_params = [p1; p2] } ->
    (match (dest_param p1) with
     Token "variable" -> true
     | _ -> false)
    | _ -> false

let nuprl_dest_var term =
  match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = [] } ->
   (match dest_op term_op' with
    {op_name = opname; op_params = [p1; p2]} ->
     (match (dest_param p2) with
     Var p -> p
     | x -> failwith "nuprl_dest_var")
   | x -> failwith "nuprl_dest_var")
 | x -> failwith "nuprl_dest_var"

let nuprl_dest_all term =
   match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = [tp ; prop] }
    when term_op' = (mk_nuprl5_simple_op "all") ->
      (match dest_bterm tp with
      { bvars = []; bterm = t1 } ->
         (match dest_bterm prop with { bvars = [x]; bterm = t2 } ->
          x, t1, t2
         | _ -> failwith "nuprl_dest_all")
      | _ -> failwith "nuprl_dest_all")
    | _ -> failwith "nuprl_dest_all"

let nuprl_dest_exists term =
   match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = [tp ; prop] }
    when term_op' = (mk_nuprl5_simple_op "exists") ->
      (match dest_bterm tp with
      { bvars = []; bterm = t1 } ->
         (match dest_bterm prop with { bvars = [x]; bterm = t2 } ->
          x, t1, t2
         | _ -> failwith "nuprl_dest_exists")
      | _ -> failwith "nuprl_dest_exists")
    | _ -> failwith "nuprl_dest_exists"


let nuprl_dest_not term =
   match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = [prop] }
    when term_op' = (mk_nuprl5_simple_op "not") ->
      (match dest_bterm prop with
      { bvars = []; bterm = t } ->
          t
         | _ -> failwith "nuprl_dest_not")
    | _ -> failwith "nuprl_dest_not"


let nuprl_dest_or term =
   match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = [left; right] }
    when term_op' = (mk_nuprl5_simple_op "or") ->
      (match dest_bterm left with
      { bvars = []; bterm = t1 } ->
         (match dest_bterm right with { bvars = []; bterm = t2 } ->
          t1, t2
         | _ -> failwith "nuprl_dest_or")
      | _ -> failwith "nuprl_dest_or")
    | _ -> failwith "nuprl_dest_or"


let nuprl_dest_implies term =
   match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = [tp; prop] }
    when term_op' = (mk_nuprl5_simple_op "implies") ->
      (match dest_bterm tp with
      { bvars = []; bterm = t1 } ->
         (match dest_bterm prop with { bvars = []; bterm = t2 } ->
          t1, t2
         | _ -> failwith "nuprl_dest_implies")
      | _ -> failwith "nuprl_dest_implies")
    | _ -> failwith "nuprl_dest_implies"


let nuprl_dest_and term =
   match Lib_term.dest_term term with
   { term_op = term_op'; term_terms = [left; right] }
    when term_op' = (mk_nuprl5_simple_op "and") ->
      (match dest_bterm left with
      { bvars = []; bterm = t1 } ->
         (match dest_bterm right with { bvars = []; bterm = t2 } ->
          t1, t2
         | _ -> failwith "nuprl_dest_and")
      | _ -> failwith "nuprl_dest_and")
    | _ -> failwith "nuprl_dest_and"


(*
let nuprl_is_all_term = function
    { term_op = { op_name = nuprl5_opname; op_params = [make_param (Token "all")] };
      term_terms = [{ bvars = [] }; { bvars = [_] }]
    }  -> true
    | _ -> false
*)
