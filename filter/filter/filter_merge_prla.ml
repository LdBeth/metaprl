(*
 * Merge two .prla files (creating a file that contains an "append" of the two).
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
 * Copyright (C) 2005 MetaPRL Group, California Institute of Technology
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
 * Author: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Format
open Lm_debug
open Lm_file_util

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermMan

open Ascii_io.AsciiIO

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_merge_prla%t"

let summary_opname  = mk_opname "Summary"      nil_opname
let incomplete_op   = mk_opname "incomplete"   summary_opname
let rewrite_op      = mk_opname "rewrite"      summary_opname
let cond_rewrite_op = mk_opname "cond_rewrite" summary_opname
let rule_op         = mk_opname "rule"         summary_opname

let incomplete_pf  = mk_simple_term incomplete_op []

(*
 * XXX: HACK: nogin: the proper way would be to decode the actual module items and then
 * do a proper filtering and merging. However I do not want to write this at the time as
 * I am hoping that it would become much easiere to do this one the new_io branch is
 * finished and merged.
 *)
let not_incomplete_proof t =
   try
      let _, _, t = dest_number_number_dep0_any_term t in
      let opname = opname_of_term t in
         if (Opname.eq opname rewrite_op) || (Opname.eq opname rule_op) then
            let params, stmt, proof, res = four_subterms t in
               not(alpha_equal incomplete_pf proof)
         else if Opname.eq opname cond_rewrite_op then
            let params, args, redex, contractum, proof, res = six_subterms t in
               not(alpha_equal incomplete_pf proof)
         else
            true
   with _ ->
      true

let merge_prla (inp1, inp2, outp) =
   let tbl1 = with_input_file inp1 read_table in
   let t1 = get_term tbl1 in
   let tbl2 = with_input_file inp2 read_table in
   let t2 = get_term tbl2 in
   let t1 = List.filter not_incomplete_proof (dest_xlist t1) in
   let t2 = List.filter not_incomplete_proof (dest_xlist t2) in
   let t = mk_xlist_term (t1 @ t2) in
   let output_term out_chan =
      let major, minor, rev = File_type_base.unpack_version (List.hd Filter_magic.ascii_versions) in Printf.fprintf out_chan "#PRL version %d.%d.%d ASCII term\n#Warning: this file was created by using the merge_prla binary\n#\tThis means that it might not actually have the above version and should not be ever committed to SVN\n" major minor rev;
         output_term tbl1 (make_simple_control out_chan) t
   in
      with_output_file outp output_term;
      printf "Output file was created successfully.@.";
      printf "However there may be some strange issues if you commit this file as is.@.";
      printf "Please recompite MetaPRL and then re-export the theory before committing the file.@."

(*
 * Everything standard is taken care of.
 * There should be a single file argument, so
 * process it.
 *)
let _ =
   if Array.length Sys.argv <> 4 then begin
      eprintf "Usage: %s input1.prla input2.prla output.prla@." Sys.argv.(0);
      exit 1
   end else begin
      Filter_exn.print_exn Dform.null_base (Some "Filter_merge_prla") merge_prla (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3));
      exit 0
   end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
