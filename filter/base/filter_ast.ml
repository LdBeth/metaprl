(*
 * Build expressions.
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
 *)
open Lm_debug

open Ml_format_sig
open Ml_format

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_ast%t"

(*
 * ML expression converter.
 *)
let build_printed_term loc t =
   let rec build_application = function
      [] ->
         raise (Invalid_argument "build_application")
    | [f] ->
         build f
    | f::a ->
         let a_expr = build_application a in
         let f_expr = build f in
            <:expr< $f_expr$ $a_expr$ >>
   and build_list = function
      h::t ->
         let t_expr = build_list t in
         let h_expr = build h in
            <:expr< [ $h_expr$ :: $t_expr$ ] >>
    | [] ->
         <:expr< [] >>
   and build_var = function
      [h] ->
         if Lm_ctype.is_capitalized h then
            <:expr< $uid:h$ >>
         else
            <:expr< $lid:h$ >>
    | h::t ->
         <:expr< $uid:h$ . $build_var t$ >>
    | [] ->
         raise (Invalid_argument "build_application")
   and build_patt = function
      ML_Module_Var [h] ->
         if Lm_ctype.is_capitalized h then
            <:patt< $uid:h$ >>
         else
            <:patt< $lid:h$ >>
    | ML_Module_Var (h::t) ->
         <:patt< $uid:h$ . $build_patt (ML_Module_Var t)$ >>
    | _ ->
         raise (Invalid_argument "build_application")
   and build = function
      ML_Var v ->
         <:expr< $lid:v$ >>
    | ML_Int i ->
         let i' = string_of_int i in
            <:expr< $int:i'$ >>
    | ML_Num n ->
         <:expr< Lm_num.num_of_string $str:Lm_num.string_of_num n$ >>
    | ML_String s ->
         <:expr< $str:s$ >>
    | ML_List l ->
         build_list l
    | ML_Apply l ->
         build_application l
    | ML_Tuple l ->
         let exprs = List.map build l in
            <:expr< ( $list:exprs$ ) >>
    | ML_Record l ->
         let fields = List.map (function (name, expr) -> (build_patt name, build expr)) l in
            <:expr< { $list:fields$ } >>
    | ML_Module_Var l ->
         build_var l
    | ML_Let (v, expr, body) ->
         let binding_expr = [<:patt< $lid:v$ >>, build expr] in
         let body_expr = build body in
            <:expr< let $list:binding_expr$ in $body_expr$ >>
   in
      build t

let build_ml_term loc t =
   build_printed_term loc (FormatTerm.format_term t)

let build_ml_mterm loc mterm =
   build_printed_term loc (FormatTerm.format_mterm mterm)

(*
 * Construct an expression list.
 *)
let list_expr loc f l =
   let rec map = function
      h::t ->
         let hd = f h in
         let tl = map t in
            <:expr< [ $hd$ :: $tl$ ] >>
    | [] ->
         <:expr< [] >>
   in
      map l

(*
 * Construct an expression list.
 *)
let apply_patt loc f l =
   let rec map = function
      [h] ->
         f h
    | h::t ->
         let hd = f h in
         let tl = map t in
            <:patt< $hd$ $tl$ >>
    | [] ->
         raise (Invalid_argument "apply_patt")
   in
      map l

(*
 * Construct an expression list.
 *)
let list_patt loc f l =
   let rec map = function
      [] ->
         <:patt< [] >>
    | h::t ->
         let hd = f h in
         let tl = map t in
            <:patt< [ $hd$ :: $tl$ ] >>
   in
      map l

(*
 * A multiple argument function (curried)
 *)
let rec fun_expr loc ids body =
   match ids with
      h::t ->
         <:expr< fun $lid:h$ -> $fun_expr loc t body$ >>
    | [] ->
         body

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
