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
open Lexing
open Simple_print.SimplePrint

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_util%t"

(************************************************************************
 * UTILS								*
 ************************************************************************)

(*
 * Dummy MLast.loc value
 *)
let dummy_loc =
   Lexing.dummy_pos, Lexing.dummy_pos

(*
 * Construct a location.
 * XXX: TODO: This converts the old-style location data into modern one.
 * Ideally, we should be able to embed location data as comments (bug 256).
 *)
let mk_pos i = {
   pos_cnum = i;
   pos_lnum = 1;
   pos_bol = 0;
   pos_fname = "";
}

let mk_proper_loc i j =
   (mk_pos (Lm_num.int_of_num i), mk_pos (Lm_num.int_of_num j))

let shift_pos pos offset =
   {pos with pos_cnum = pos.pos_cnum + offset}

let adjust_pos globpos local_pos =
(*
 * XXX: TODO: Because of Camlp4 bugs (OCaml PR#2953 and PR#2954), we can only
 * trust pos_cnum for now.
 *
 *  {
 *     globpos with
 *     pos_lnum = if local_pos.pos_lnum > 0 then globpos.pos_lnum + local_pos.pos_lnum - 1 else globpos.pos_lnum;
 *     pos_bol = if local_pos.pos_lnum <= 1 then globpos.pos_bol else local_pos.pos_bol + globpos.pos_cnum;
 *     pos_cnum = globpos.pos_cnum + local_pos.pos_cnum;
 *  }
 *)
   mk_pos (globpos.pos_cnum + local_pos.pos_cnum)

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
    | mt ->
         raise (BadCommand (name ^ ": illegal specification: " ^ string_of_mterm mt))
   in
      aux

(*
 * Split the function into var names and a simple mterm.
 *)
let split_mfunction =
   let collect (labels', ext, t) (labels, exts, terms) =
      let ext = match ext with
         Some ext -> ext
       | None -> Refine.dummy_ext
      in
         labels' :: labels, ext :: exts, t :: terms
   in fun mterm ->
      let subgoals, goal = unzip_mfunction mterm in
      let labels, exts, terms = List.fold_right collect subgoals ([], [], []) in
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
 * THEORY GROUPS AND DESCRIPTIONS                                       *
 ************************************************************************)

let make_groupdsc_opts () =
   let set s r v =
      match !r with
         Some v' when v <> v' ->
            raise (Invalid_argument ("Option \"" ^ s ^ "\" was set twice!"))
       | _ ->
            r := Some v
   in
   let get short long =
      let ref = Env_arg.string short None long set in
         fun () ->
            match !ref with
               Some v -> v
             | None -> raise (Invalid_argument ("Option \"" ^ short ^ "\" was not provided!"))
   in
      get "group" "The short name of the group of modules",
      get "descr" "The description of the group of modules"


(************************************************************************
 * BINDINGS IN STR ITEMS                                                *
 ************************************************************************)

let add_binding, get_bindings =
   let loc = dummy_loc in
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
   in
      add_binding, get_bindings

let no_resources =
   { item_item = [];
     item_bindings = [];
   }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
