(*
 * Simplified version of termTable.
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

open Refiner.Refiner.Term

type 'a term_dtable
type 'a term_dextract

type 'a pair_fun = (term * term * 'a) list -> term * term -> 'a

val new_dtable : unit -> 'a term_dtable

val insert_left : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val insert_right : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val insert : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val join_tables : 'a term_dtable -> 'a term_dtable -> 'a term_dtable
val extract : 'a term_dtable -> 'a term_dextract
val lookup : 'a term_dextract -> term -> term -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
