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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Pcaml

(*
 * Make an infix expression.
 *)
let make_infix loc op e1 e2 =
  let lop = "prefix_" ^ op in
  MLast.ExApp (loc, MLast.ExApp (loc, MLast.ExLid (loc, lop), e1), e2)

(*
 * Add an infix keyword.
 * This is computed in infix.ml.
 *)
let add_infix (keyword : string) =
  Grammar.extend
    (let _ = (expr : 'expr Grammar.Entry.e) in
     [Grammar.Entry.obj (expr : 'expr Grammar.Entry.e),
      Some (Gramext.Before "top"),
      [None, None,
       [[Gramext.Sself; Gramext.Stoken ("", keyword); Gramext.Sself],
        Gramext.action
          (fun (e2 : 'expr) (op : string) (e1 : 'expr) (loc : int * int) ->
             (make_infix loc op e1 e2 : 'expr))]]])

(*
 * Remove the infix keyword.
 *)
let remove_infix (keyword : string) =
  Grammar.delete_rule expr
    [Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e));
     Gramext.Stoken ("", keyword);
     Gramext.Snterm (Grammar.Entry.obj (expr : 'expr Grammar.Entry.e))]

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
