(*
 * Constants and common functions.
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
open Refiner.Refiner.TermType
open Refiner.Refiner.Term

(*
 * Special symbols and constants.
 *)

let start_state_id = 1

let global_start_string = "%start%"

let global_start_symbol = NonTerminal global_start_string

let bogus_symbol = Terminal "???"

let eof_symbol = Eof

let stack_start_state = Sta_state start_state_id

let bogus_pos = ("<default>", 0, 0, 0, 0)

(*
 * Helper functions.
 *)
let string_add = String.concat ""

let string_format = Printf.sprintf

let list_mem lst el = List.mem el lst

let list_add lst el = List.append lst [el]

let rec list_firstn accum n = function
   head :: rest ->
      if n < 0 then
         raise (Invalid_argument "list_firstn:n")
      else
      if n = 0 then
         List.rev accum
      else
         list_firstn (head :: accum) (n-1) rest
 | [] ->
      if n = 0 then
         List.rev accum
      else
         raise (Invalid_argument "list_firstn")

let list_first_n lst n = list_firstn [] n lst

let list_from_nth lst n =
   let lst = List.rev lst in
   let lst = list_first_n lst (List.length lst - n + 1) in
      List.rev lst

let list_nth lst n = List.nth lst (n-1)

let array_sub array i1 i2 = Array.sub array i1 (i2-i1+1)

let string_of_file name =
   let inx = open_in name in
   let size = in_channel_length inx in
   let buf = String.create size in
      really_input inx buf 0 size;
      close_in inx;
      buf

let first_of_option = function
   Some (s, _) ->
      Some s
 | None ->
      None

(* Breaking up terms and bound terms *)
let breakup_term term =
   let term = dest_term term in
   let op = dest_op term.term_op in
      op.op_name, dest_params op.op_params, term.term_terms

let breakup_bterm bterm =
   let { bvars = bound_vars;
         bterm = term
       } = dest_bterm bterm
   in
      (* Ignore bound variables! *)
      breakup_term term

 
