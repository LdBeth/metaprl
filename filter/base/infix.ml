(*
 * This file just exists to extract the code for adding and infix
 * expression.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Pcaml

(*
 * Make an infix expression.
 *)
let make_infix loc op e1 e2 =
   let lop = "prefix_" ^ op in
      <:expr< $lid:lop$ $e1$ $e2$ >>

(*
 * Add an infix keyword.
 * This is computed in infix.ml.
 *)
let add_infix (keyword : string) =
   EXTEND
      GLOBAL: expr;

      expr: BEFORE "top" (**)
         [[ e1 = expr; op = STRING $keyword$; e2 = expr ->
             make_infix loc op e1 e2
          ]];
   END

(*
 * Remove the infix keyword.
 *)
let remove_infix (keyword : string) =
   DELETE_RULE
      expr:
         expr; STRING $keyword$; expr
   END

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
