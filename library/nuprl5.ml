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

open Printf
open Mp_debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Opname
open Mp_num

let _ =
   show_loading "Loading Nuprl5%t"

let nuprl5_opname = mk_opname "!nuprl5_implementation!" nil_opname

(* parameter mapping *)

let make_bool_parameter b =
  make_param (ParamList
		[(make_param (Token "bool")); (make_param (Number (Mp_num.Int (if b then 1 else 0))))])

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
	Number (Mp_num.Int i) -> (i = 1) or (i = 0)
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
	Number (Mp_num.Int i) -> i = 1
      | _ -> raise (Invalid_argument "destruct_bool_parameter"))
      else raise (Invalid_argument "destruct_bool_parameter")
    | _ -> raise (Invalid_argument "destruct_bool_parameter"))
  | _ -> raise (Invalid_argument "destruct_bool_parameter")

(* common terms *)

(*
let itoken_term s = mk_token_term nuprl5_opname s
let inatural_term i = mk_number_term nuprl5_opname i
*)



