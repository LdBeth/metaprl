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

(* jprover fun returns string*term*term list, convert to term *)

let ijprover_op = Nuprl5.mk_nuprl5_op [(make_param (Token "jprover"))]

let ijprover_term s t1 t2 = 
  mk_term ijprover_op [mk_bterm [] (Basic.istring_term s); mk_bterm [] t1; mk_bterm [] t2]

let jprover_result_to_term l = 
  let f (s, t1, t2) = ijprover_term s t1 t2 in
  Basic.list_to_ilist_map f l
