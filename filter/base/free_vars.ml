(*
 * Compute the free vars of a term.
 * This is all the lids.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

open Lm_debug
open Lm_printf

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Free_vars%t"


(*
 * Compute the binding variables.
 *)
let rec patt_bvars bvars = function
   <:patt< $p1$ . $p2$ >>
 | <:patt< ($p1$ as $p2$) >>
 | <:patt< $p1$ $p2$ >>
 | <:patt< $p1$ | $p2$ >>
 | <:patt< $p1$ .. $p2$ >> ->
      patt_bvars (patt_bvars bvars p1) p2
 | <:patt< _ >>
 | <:patt< $chr:_$ >>
 | <:patt< $int:_$ >>
 | <:patt< $str:_$ >>
 | <:patt< $uid:_$ >> ->
      bvars
 | <:patt< $lid:i$ >> ->
      if List.mem i bvars then
         bvars
      else
         i :: bvars
 | <:patt< { $list:ppl$ } >> ->
      List.fold_left (fun bvars (p1, p2) -> patt_bvars (patt_bvars bvars p1) p2) bvars ppl
 | <:patt< ( $list:pl$ ) >> ->
      List.fold_left patt_bvars bvars pl
 | <:patt< ( $p$ : $_$ ) >> ->
      patt_bvars bvars p
 | _ ->
      raise (Failure "patt_bvars: pattern not recognized")

let patt_vars patt =
   patt_bvars [] patt

(*
 * Compute free vars of an Ast.expr.
 *)
let free_vars expr =
   let rec free bvars l = function
      <:expr< $lid:v$ >> ->
         if List.mem v bvars || List.mem v l then
            l
         else
            v :: l
    | <:expr< $e1$ . $e2$ >>
    | <:expr< $e1$ $e2$ >>
    | <:expr< $e1$ .( $e2$ ) >>
    | <:expr< $e1$ := $e2$ >>
    | <:expr< $e1$ .[ $e2$ ] >> ->
         free bvars (free bvars l e1) e2
    | <:expr< [| $list:el$ |] >>
    | <:expr< do { $list:el$ } >>
    | <:expr< ( $list:el$ ) >> ->
         List.fold_left (free bvars) l el
    | <:expr< $chr:_$ >>
    | <:expr< $flo:_$ >>
    | <:expr< $int:_$ >>
    | <:expr< $str:_$ >>
    | <:expr< $uid:_$ >> ->
         l
(*
    | <:expr< ( $e1$ :> $_$ ) >> ->
*)
    | MLast.ExCoe (_, e, _, _)
    | <:expr< $e$ # $_$ >>
    | MLast.ExLmd (_, _, _, e)
    | <:expr< ( $e$ : $_$ ) >> ->
         free bvars l e
    | <:expr< for $v$ = $e1$ $to:_$ $e2$ do { $list:el$ } >> ->
         List.fold_left (free (v::bvars)) (free bvars (free bvars l e1) e2) el
    | <:expr< fun [ $list:pwel$ ] >> ->
         free_pwel bvars l pwel
    | <:expr< if $e1$ then $e2$ else $e3$ >> ->
         free bvars (free bvars (free bvars l e1) e2) e3
    | <:expr< let $opt:_$ $list:pel$ in $e$ >> ->
         free_pel bvars l e pel
    | <:expr< match $e$ with [ $list:pwel$ ] >>
    | <:expr< try $e$ with [ $list:pwel$ ] >> ->
         free_pwel bvars (free bvars l e) pwel
    | <:expr< new $list:_$ >> ->
         bvars
    | <:expr< {< $list:sel$ >} >> ->
         List.fold_left (fun l (_, el) -> free bvars l el) l sel
    | <:expr< { $list:eel$ } >> ->
         List.fold_left (fun l (_, el) -> free bvars l el) l eel
    | <:expr< while $e$ do { $list:el$ } >> ->
         List.fold_left (free bvars) (free bvars l e) el
    | _ ->
         raise (Failure "free_vars: expression not recognized")
   and free_pwel bvars l = function
      (patt, _, e)::tl ->
          free_pwel bvars (free (patt_bvars bvars patt) l e) tl
     | [] ->
          l
   and free_pel bvars l e = function
      (patt, e)::tl ->
          free_pel (patt_bvars bvars patt) (free bvars l e) e tl
     | [] ->
          free bvars l e
   in
      free [] [] expr

let _ = ()

(*
 * Find some new variable names.
 *)
let new_vars expr l =
   let free = free_vars expr in
   let rec new_var i v =
      let vname = sprintf "%s%d" v i in
         if List.mem vname free then
            new_var (i + 1) v
         else
            vname
   in
      List.map (new_var 0) l

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
