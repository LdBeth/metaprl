(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
 *
 *)
open Lm_symbol
open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Nuprl5
open Opname

module Nuprl_JLogic =
struct
   let is_all_term = nuprl_is_all_term
   let dest_all t =
      let v, t1, t2 = nuprl_dest_all t in
         v, t1, t2
   let is_exists_term = nuprl_is_exists_term
   let dest_exists t =
      let v, t1, t2 = nuprl_dest_exists t in
         v, t1, t2
   let is_and_term = nuprl_is_and_term
   let dest_and = nuprl_dest_and
   let is_or_term = nuprl_is_or_term
   let dest_or = nuprl_dest_or
   let is_implies_term = nuprl_is_implies_term
   let dest_implies = nuprl_dest_implies
   let is_not_term = nuprl_is_not_term
   let dest_not = nuprl_dest_not

   let is_box_term _ = false
   let dest_box _ =
      raise (Invalid_argument "Itt_logic: at this time there is no box-modlaity in ITT")

   type inference = (string * term * term) list
   let empty_inf = []
   let append_inf inf t1 t2 r =
      ((Jall.ruletable r), t1, t2) :: inf
end

module NuprlJProver = Jall.JProver(Nuprl_JLogic)

let jprover mult_limit (tlist,concl) =
  print_string "calling jprover";
  NuprlJProver.prover (Some mult_limit) tlist concl

(* jprover function returns string*term*term list, convert to term *)

let token s = Token (mk_opname s nil_opname)

let ijprover_op = Nuprl5.mk_nuprl5_op [(make_param (token "jprover"))]

let ijprover_term (s, t1, t2) =
  mk_term ijprover_op [mk_bterm [] (Basic.istring_term s); mk_bterm [] t1; mk_bterm [] t2]

let print_jlist l =
  List.iter (fun (s, t1, t2) ->
		 (print_newline();
		  print_string s;
                  Mbterm.print_term t1;
		  Mbterm.print_term t2))
             l

let jprover_result_to_term l =
  print_string "Calling jprover result ";
  print_jlist l;
  Basic.list_to_ilist_map ijprover_term l

let replace_nuprl_var_terms term = term

let debug_term = ref Basic.ivoid_term

let jprover_debug_hook t =
  print_string "calling jprover hook ";
  let x = Basic.hd_of_icons_term Basic.icons_op t and
      y = Basic.tl_of_icons_term Basic.icons_op t in
  let result =
  try (jprover_result_to_term
       (jprover (Basic.number_of_inatural_term x) ((Basic.map_isexpr_to_list (function a -> a)
		    (Basic.hd_of_icons_term Basic.icons_op y)),
	         (replace_nuprl_var_terms (Basic.tl_of_icons_term Basic.icons_op y)))))
  with e -> mk_term ijprover_op [] in
  debug_term := result;
  print_newline();
  print_string "Result is:  ";
  Mbterm.print_term result;
  let mbterm =
       try Mbterm.mbterm_of_term result with
       e -> (print_string "mbterm failed";
	     Mbterm.mbterm_of_term (mk_term ijprover_op [])) in
  result

let jprover_hook t =
  let mult_limit = Basic.number_of_inatural_term (Basic.hd_of_icons_term Basic.icons_op t) and
      term = Basic.tl_of_icons_term Basic.icons_op t in
  jprover_result_to_term
    (jprover mult_limit ((Basic.map_isexpr_to_list (function a -> a)
		 (Basic.hd_of_icons_term Basic.icons_op term)),
	      (replace_nuprl_var_terms (Basic.tl_of_icons_term Basic.icons_op term))))
