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
 *	
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Nuprl5

module Nuprl_JLogic =
struct
	let is_all_term = nuprl_is_all_term
        let dest_all = nuprl_dest_all
	let is_exists_term = nuprl_is_exists_term
        let dest_exists = nuprl_dest_exists
	let is_and_term = nuprl_is_and_term
        let dest_and = nuprl_dest_and
	let is_or_term = nuprl_is_or_term
        let dest_or = nuprl_dest_or
	let is_implies_term = nuprl_is_implies_term
        let dest_implies = nuprl_dest_implies
	let is_not_term = nuprl_is_not_term
        let dest_not = nuprl_dest_not
end


module Nuprl_JProver = Jall.JProver(Nuprl_JLogic) 

let myconcl = ref Basic.inil_term
let myhyps = ref [Basic.inil_term]
let jprover (tlist,concl) = 
  myconcl := concl;
  myhyps := tlist;
  Nuprl_JProver.prover (tlist,concl)


(* jprover fun returns string*term*term list, convert to term *)

let ijprover_op = Nuprl5.mk_nuprl5_op [(make_param (Token "jprover"))]

let ijprover_term (s, t1, t2) = 
  mk_term ijprover_op [mk_bterm [] (Basic.istring_term s); mk_bterm [] t1; mk_bterm [] t2]

let jprover_result_to_term l = 
  Basic.list_to_ilist_map ijprover_term l

let jprover_run t = 
 jprover_result_to_term (jprover ((Basic.map_isexpr_to_list (function y -> y) (Basic.hd_of_icons_term Basic.icons_op t)), (Basic.tl_of_icons_term Basic.icons_op t))) 