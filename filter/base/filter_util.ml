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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_debug
open Lm_printf

open Opname
open Term_sig
open Refiner.Refiner.TermMan
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
let make_dummy_loc name =
   Ploc.make_loc name 1 0 (0, 0) ""

let dummy_loc = make_dummy_loc "<dummy>"

(*
 * Construct a location.
 * XXX: TODO: This converts the old-style location data into modern one.
 * Ideally, we should be able to embed location data as comments (bug 256).
 *)
let mk_pos i j =
   Ploc.make_unlined (i, j)

let mk_proper_loc i j =
   mk_pos (Lm_num.int_of_num i) (Lm_num.int_of_num j)

let shift_pos pos offset =
   Ploc.shift offset pos

let adjust_pos globpos local_pos =
   let local_pos_lnum = Ploc.line_nb local_pos in
   let local_pos_bol = Ploc.bol_pos local_pos in
   let local_pos_cnum = Ploc.first_pos local_pos in
   let local_pos_enum = Ploc.last_pos local_pos in
   let glob_pos_lnum = Ploc.line_nb globpos in
   let glob_pos_bol = Ploc.bol_pos globpos in
   let glob_pos_cnum = Ploc.first_pos globpos in
   let glob_pos_enum = Ploc.last_pos globpos in
   let glob_comm = Ploc.comment globpos in
   let glob_name = Ploc.file_name globpos in
      Ploc.make_loc glob_name (**)
         (if local_pos_lnum > 0 then glob_pos_lnum + local_pos_lnum - 1 else glob_pos_lnum)
         (if local_pos_lnum <= 1 then glob_pos_bol else local_pos_bol + glob_pos_cnum)
         (glob_pos_cnum + local_pos_cnum, glob_pos_enum + local_pos_enum)
         glob_comm

let ploc_of_lexing (l1, l2) =
   Ploc.make_loc l1.pos_fname l1.pos_lnum l1.pos_bol (l1.pos_cnum, l2.pos_cnum) ""

let grammar_parse gram st =
   let old_input = !Pcaml.input_file in
   let bol_ref, lnum_ref, name_ref = Plexing.bol_pos, Plexing.line_nb, Plexing.input_file in
   let old_bol, old_lnum, old_name = !bol_ref, !lnum_ref, !name_ref in
   let restore () =
      bol_ref := old_bol;
      lnum_ref := old_lnum;
      name_ref := old_name;
      Pcaml.input_file := old_input
    in
      bol_ref := ref 0;
      lnum_ref := ref 1;
      try
         let items = Grammar.Entry.parse gram st in
            restore ();
            items
      with exn ->
         restore ();
         raise exn

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
   let dummy = "__@@dummy_extract@@__" in
   let collect (labels', ext, t) (labels, exts, terms, i) =
      let ext = match ext with
         Some ext -> ext
       | None -> mk_so_var_term (Lm_symbol.make dummy i) [] []
      in
         labels' :: labels, ext :: exts, t :: terms, succ i
   in fun mterm ->
      let subgoals, goal = unzip_mfunction mterm in
      let labels, exts, terms, _ = List.fold_right collect subgoals ([], [], [], 0) in
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

(* unused
let print_opname ofile name =
   output_string ofile (string_of_opname_list name)
*)

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

(* TODO: LDB: fix dummy loc *)
let add_binding, get_bindings =
   let _loc = dummy_loc in
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

let intro_resources _loc =
   let intro =
      { res_loc  = _loc;
        res_name = "intro";
        res_flag = Mp_resource.Public;
        res_args = []
      }
   in
      { item_item = [intro];
        item_bindings = []
      }

let elim_resources _loc =
   let elim =
      { res_loc  = _loc;
        res_name = "elim";
        res_flag = Mp_resource.Public;
        res_args = []
      }
   in
      { item_item = [elim];
        item_bindings = []
      }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
