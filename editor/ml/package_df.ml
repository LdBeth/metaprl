(*
 * Display all the elements in a particular theory.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

include Base_theory

include Io_proof
include Package_info

open Printf
open Nl_debug

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Rformat
open Dform_print
open Dform

open Filter_summary
open Filter_ocaml
open Filter_cache

open Io_proof
open Package_info
open Package

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Package_df%t" eflush

(*
 * Our display parameters.
 *)
let tabstop = 3
let min_screen_width = 80

(*
 * This comment function removes expressions.
 *)
let comment =
   let loc = 0, 0 in
   let null_term = mk_var_term "..." in
   let comment ttype _ t =
      match ttype with
         ExprTerm ->
            null_term
       | _ ->
            t
   in
      comment

let identity x = x
let term_of_expr = term_of_expr [] comment
let term_of_type = term_of_type comment
let term_of_sig_item = term_of_sig_item comment
let term_of_str_item = term_of_str_item [] comment

let convert_intf =
   let null_term = mk_var_term "..." in
      { term_f  = identity;
        meta_term_f = term_of_meta_term;
        proof_f = (fun _ _ -> null_term);
        ctyp_f  = term_of_type;
        expr_f  = term_of_expr;
        item_f  = term_of_sig_item
      }

let convert_impl =
   let convert_proof _ = function
      Primitive t ->
         t
    | Derived expr ->
         term_of_expr expr
    | Incomplete ->
         mk_var_term "#"
    | Interactive proof ->
         match status_of_proof proof with
            Proof.Bad ->
               mk_var_term "-"
          | Proof.Partial ->
               mk_var_term "#"
          | Proof.Asserted ->
               mk_var_term "!"
          | Proof.Complete ->
               mk_var_term "*"
   in
      { term_f  = identity;
        meta_term_f = term_of_meta_term;
        proof_f = convert_proof;
        ctyp_f  = term_of_type;
        expr_f  = term_of_expr;
        item_f  = term_of_str_item
      }

(*
 * Display the entire package.
 *)
let format_interface mode buf pack =
   let dbase = dforms pack in
   let db = get_mode_base dbase mode in
   let tl = term_list convert_intf (Package.sig_info pack) in
   let t = mk_simple_term interface_op [mk_xlist_term tl] in
      format_term db buf t;
      format_newline buf

(*
 * Display the entire package.
 *)
let format_implementation mode buf pack =
   let dbase = dforms pack in
   let db = get_mode_base dbase mode in
   let tl = term_list convert_impl (Package.info pack) in
   let t = mk_simple_term implementation_op [mk_xlist_term tl] in
      format_term db buf t;
      format_newline buf

(*
 * Display the package DAG, using indentition.
 * Each package is listed with its children indented.
 *)
let format_packages buf pack =
   let compare pack1 pack2 =
      Package.name pack1 <= Package.name pack2
   in
   let packs = Sort.list compare (Package.packages pack) in
   let format_package pack' =
      let children = Sort.list compare (Package.children pack pack') in
      let format_child child =
         format_newline buf;
         format_string buf (Package.name child)
      in
         format_pushm buf tabstop;
         format_string buf (Package.name pack');
         List.iter format_child children;
         format_popm buf;
         format_newline buf
   in
      List.iter format_package packs

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
