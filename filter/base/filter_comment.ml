(*
 * This module is used to provide an association between
 * terms in an ML file, and their comments.
 *
 * The algorithm is best match.  We parse the comments from
 * the file, the iterate through all the terms in the
 * program.  The closest, largest program block after the comment
 * is associated with the comment through a table.
 * The comments can then be queried through the table.
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

open Printf
open Mp_debug
open Mp_pervasives

open MLast
open MLast_util

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_comment%t"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The assocation is a hastable that maps MLast.loc to comments.
 *)
type t = (MLast.loc, int * string) Hashtbl.t

(************************************************************************
 * PARSING                                                              *
 ************************************************************************)

(*
 * Read the comments from the file.
 * No fancy stuff, just use stdio.
 *
 * raises Sys_error if file can't be opened.
 *)
let parse name =
   (*
    * Output:
    *   buf: buffer for collecting chars in a comment
    *   comments: list of all the comments.
    *   store: put another comment in the list.
    *)
   let buf = Filter_buffer.create () in
   let comments = ref [] in
   let store loc s =
      comments := (loc, s) :: !comments
   in

   (*
    * Input:
    *    inx: the input file
    *    getc: get a char from the file (may raise End_of_file)
    *    index: the position of the next char getc will return.
    *)
   let inx = open_in name in
   let index = ref 0 in
   let getc () =
      let c = input_char inx in
         incr index;
         c
   in
   let rec read = function
      '(' ->
         maybe_comment1 (getc ())
    | _ ->
         read (getc ())
   and maybe_comment1 = function
      '*' ->
         (* This might be a comment or it may be Nuprl *)
         maybe_comment2 (getc ())
    | _ ->
         (* Wasn't a comment after all *)
         read (getc ())
   and maybe_comment2 = function
      '*' ->
         (* It is a comment, but it may also be a terminator *)
         Filter_buffer.puts buf "(**";
         maybe_comment_end 1 (getc ())
    | ':' ->
         (* Nuprl comment leader is ignored *)
         read (getc ())
    | c ->
         (* It is certainly a comment *)
         Filter_buffer.puts buf "(*";
         Filter_buffer.putc buf c;
         comment 0 (getc ())
   and comment level c =
      Filter_buffer.putc buf c;
      match c with
         '*' ->
            maybe_comment_end level (getc ())
       | '(' ->
            maybe_nested_comment level (getc ())
       | _ ->
            comment level (getc ())
   and maybe_nested_comment level c =
      Filter_buffer.putc buf c;
      match c with
         '*' ->
            (* Comment is nested *)
            comment (level + 1) (getc ())
       | _ ->
            comment level (getc ())
   and maybe_comment_end level c =
      Filter_buffer.putc buf c;
      match c with
         ')' ->
            if level = 1 then
               (* Comment is terminated *)
               let s = Filter_buffer.gets buf in
                  store (!index - (String.length s)) s;
                  Filter_buffer.clear buf;
                  read (getc ())
            else
               comment (level - 1) (getc ())
       | _ ->
            comment level (getc ())
   in

   (* Execution will always produce End_of_file *)
   let _ =
      try read (getc ()) with
         End_of_file -> ()
   in
      close_in inx;
      List.rev !comments

(************************************************************************
 * ASSOCIATIONS                                                         *
 ************************************************************************)

(*
 * Program interators.
 *)
let fold_expr locs expr =
   loc_of_expr expr :: locs

let fold_patt locs patt =
   loc_of_patt patt :: locs

let fold_type locs ctyp =
   loc_of_ctyp ctyp :: locs

let fold_sig_item locs item =
   loc_of_sig_item item :: locs

let fold_str_item locs item =
   loc_of_str_item item :: locs

let fold_module_expr locs me =
   loc_of_module_expr me :: locs

let fold_module_type locs mt =
   loc_of_module_type mt :: locs

let fold_with_constr locs = function
   WcTyp (loc, _, _, _)
 | WcMod (loc, _, _) ->
      loc :: locs

let fold_class_any_infos locs { ciLoc = loc } =
   loc :: locs

let fold_class_expr locs = function
   MLast.CeApp (loc, _, _)
 | MLast.CeCon (loc, _, _)
 | MLast.CeFun (loc, _, _)
 | MLast.CeLet (loc, _, _, _)
 | MLast.CeStr (loc, _, _)
 | MLast.CeTyc (loc, _, _) ->
(* 3.02
 | MLast.CeXnd (loc, _, _)
 *)
      loc :: locs

let fold_class_type locs = function
   MLast.CtCon (loc, _, _)
 | MLast.CtFun (loc, _, _)
 | MLast.CtSig (loc, _, _) ->
(* 3.02
 | MLast.CtXnd (loc, _, _) ->
 *)
    loc :: locs

let fold_class_sig_item locs = function
   CgCtr (loc, _, _)
 | CgDcl (loc, _)
 | CgInh (loc, _)
 | CgMth (loc, _, _, _)
 | CgVal (loc, _, _, _)
 | CgVir (loc, _, _, _) ->
      loc :: locs

let fold_class_str_item locs = function
   CrCtr (loc, _, _)
 | CrDcl (loc, _)
 | CrInh (loc, _, _)
 | CrIni (loc, _)
 | CrMth (loc, _, _, _, _)
 | CrVal (loc, _, _, _)
 | CrVir (loc, _, _, _) ->
      loc :: locs

(*
 * Fold record.
 *)
let folder =
   { fold_expr = fold_expr;
     fold_patt = fold_patt;
     fold_type = fold_type;
     fold_sig_item = fold_sig_item;
     fold_str_item = fold_str_item;
     fold_module_expr = fold_module_expr;
     fold_module_type = fold_module_type;
     fold_with_constr = fold_with_constr;
     fold_class_type_infos = fold_class_any_infos;
     fold_class_expr_infos = fold_class_any_infos;
     fold_class_expr = fold_class_expr;
     fold_class_str_item = fold_class_str_item;
     fold_class_type = fold_class_type;
     fold_class_sig_item = fold_class_sig_item
   }

(*
 * Create an association.
 * Each comment gets associated with the closest,
 * largest block following the comment, except for the final
 * comment, which is associated with the last block.
 *
 * We assume the comments are sorted by location.
 * We have to sort the locations.
 *)
let create comments locs =
   let table = Hashtbl.create 97 in
   let rec insert locs = function
      comment :: comments ->
         let start, contents = comment in
         let rec search locs =
            match locs with
               [loc] ->
                  Hashtbl.add table loc comment;
                  insert locs comments
             | loc :: locs' ->
                  let start', _ = loc in
                     if start' >= start then
                        begin
                           (* This is the closest term *)
                           Hashtbl.add table loc comment;
                           insert locs comments
                        end
                     else
                        search locs'
             | [] ->
               (* No associations are possible *)
                  ()
         in
            search locs
    | [] ->
         ()
   in

   (* Sort the list by starting position, longest length going first *)
   let compare (start1, len1) (start2, len2) =
      (start1 < start2) || (start1 = start2 && len1 < len2)
   in
   let locs = Sort.list compare locs in
      insert (Sort.list compare locs) comments;
      table

(*
 * Type specific creators.
 *)
let create_sig comments interf =
   create comments (List.fold_left (MLast_util.fold_sig_item folder) [] interf)

let create_str comments implem =
   create comments (List.fold_left (MLast_util.fold_str_item folder) [] implem)

(*
 * Get an association from the table.
 * The location has to match exactly.
 *)
let get = Hashtbl.find

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
