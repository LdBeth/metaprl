(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
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

open List;;
open Hashtbl;;

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type opname = string list;;

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * We hash-cons the opnames.
 *)
let (optable : (opname, opname) Hashtbl.t) = Hashtbl.create 97;;

(*
 * Constructors.
 *)
let nil_opname = [];;
add optable nil_opname nil_opname;;

let mk_opname s name =
   let op = s::name in
      try find optable op with
         Not_found -> add optable op op; op;;

let make_opname =
   let rec aux = function
      [] -> nil_opname
    | h::t -> mk_opname h (aux t)
   in
      aux;;

let normalize_opname = make_opname;;

(*
 * Destructor.
 *)
let dest_opname op = op;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
