(*
 * Common utilities for filtering modules.
 *
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
open Lm_debug
open Lm_printf

open Opname
open Term_sig
open Refiner.Refiner
open Refiner.Refiner.TermMeta
open Filter_type

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_util%t"

(************************************************************************
 * UTILS								*
 ************************************************************************)

(*
 * Get the context vars from a list.
 *)
let rec context_vars_list = function
   h::t ->
      Lm_list_util.union (TermSubst.context_vars h) (context_vars_list t)
 | [] ->
      []

(*
 * Collect the arguments in a rewrite.
 *)
let unzip_rewrite name =
   let rec aux = function
      MetaImplies (MetaTheorem a, b) ->
         let args, redex, contractum = aux b in
            a::args, redex, contractum
    | MetaIff (MetaTheorem redex, MetaTheorem contractum) ->
         [], redex, contractum
    | _ ->
         raise (BadCommand (name ^ ": illegal specification"))
   in
      aux

(*
 * Split the function into var names and a simple mterm.
 *)
let split_mfunction =
   let collect (labels', ext, t) (i, labels, exts, terms) =
      let ext = match ext with
         Some ext -> ext
       | None -> Refine.make_wildcard_ext_arg i t
      in
         succ i, labels' :: labels, ext :: exts, t :: terms
   in fun mterm ->
      let subgoals, goal = unzip_mfunction mterm in
      let _, labels, exts, terms = List.fold_right collect subgoals (1, [], [], []) in
         labels, exts, zip_mimplies terms goal

(************************************************************************
 * OPNAMES                                                              *
 ************************************************************************)

(*
 * Opname printing.
 *)
let string_of_opname_list =
   let rec print_path path' = function
      h::t ->
         if path' = "" then
            print_path h t
         else
            print_path (path' ^ "!" ^ h) t
    | [] ->
         path'
   in
      print_path ""

let print_opname ofile name =
   output_string ofile (string_of_opname_list name)

(*
 * Get a string from an opname.
 *)
let translate_opname opname =
   let l = dest_opname opname in
   let rec aux = function
      [h] -> h
    | h::t -> h ^ "_" ^ (aux t)
    | [] -> ""   in
      aux l

(************************************************************************
 * MODULE PATHS                                                         *
 ************************************************************************)

let rec string_of_path = function
   [] ->
      (* This should never happen because paths always point to something *)
      raise (EmptyModulePath "string_of_path")
 | [h] -> String.capitalize h
 | h::t -> (String.capitalize h) ^ "/" ^ (string_of_path t)

(*
 * Output a path to an ml file.
 *)
let output_path oport =
   let rec aux = function
      [] -> raise (EmptyModulePath "output_path")
    | [h] -> output_string oport (String.capitalize h)
    | h::t ->
         output_string oport h;
         output_string oport ".";
         aux t
   in
      aux

(************************************************************************
 * BINDINGS IN STR ITEMS                                                *
 ************************************************************************)

let add_binding, get_unparsed_bindings =
   let loc = 0,0 in
   let decls = ref [] in
   let decl_var = ref 0 in
   let add_binding d =
      incr decl_var;
      let v = "_$item_bnd" ^ (string_of_int !decl_var) in
         decls := (v, d) :: !decls;
         <:expr< $lid:v$ >>
   and get_bindings () =
      let res = !decls in
         decls := [];
         res
   in add_binding, get_bindings

let conv = function
   v, BindTerm t -> v, BindTerm(term_of_parsed_term t)
 | bnd -> bnd

let get_bindings () =
   List.map conv (get_unparsed_bindings ())

let no_resources = {
   item_item = [];
   item_bindings = [];
}

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
