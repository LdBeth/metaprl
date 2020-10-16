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
open MLast

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermMan

open Filter_util

open Ascii_io.AsciiIO
open Proof_boot.Proof
open Tacticals_boot.Tacticals

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_merge_prla%t"

type pf_kind =
   Empty
 | Primitive
 | Real

(*
 * XXX: HACK: nogin: the proper way would be to decode the actual module items and then
 * do a proper filtering and merging. However I do not want to write this at the time as
 * I am hoping that it would become much easiere to do this one the new_io branch is
 * finished and merged.
 *)
let summary_opname  = mk_opname "Summary"      nil_opname
let prim_op         = mk_opname "prim"         summary_opname
let incomplete_op   = mk_opname "incomplete"   summary_opname
let interactive_op  = mk_opname "interactive"  summary_opname
let rewrite_op      = mk_opname "rewrite"      summary_opname
let cond_rewrite_op = mk_opname "cond_rewrite" summary_opname
let rule_op         = mk_opname "rule"         summary_opname

let incomplete_pf  = mk_simple_term incomplete_op []

let trivial_proof t =
   let op = opname_of_term t in
      if Opname.eq op incomplete_op && alpha_equal incomplete_pf t then
         Empty
      else if Opname.eq op prim_op then
         Primitive
      else if
            Opname.eq op interactive_op &&
            let t = one_subterm t in
            let pf = io_proof_of_term (fun _ -> ExUid(dummy_loc, Ploc.VaVal "()")) (fun _ -> idT) t in
               fst (node_count_of_io_proof pf) = 0
         then
            Empty
      else
         Real

let extract_proof term =
   try
      let _, _, t = dest_number_number_dep0_any_term term in
      let opname = opname_of_term t in
         if (Opname.eq opname rewrite_op) || (Opname.eq opname rule_op) then
            let name = dest_string_param t in
            let params, stmt, proof, res = four_subterms t in
               Some(name, proof)
         else if Opname.eq opname cond_rewrite_op then
            let name = dest_string_param t in
            let params, args, redex, contractum, proof, res = six_subterms t in
               Some(name, proof)
         else
            None
   with _ ->
      None

let merge_prla (inp1, inp2, outp) =
   let tbl1 = with_input_file inp1 read_table in
   let t1 = dest_xlist (get_term tbl1) in
   let tbl2 = with_input_file inp2 read_table in
   let t2 = dest_xlist (get_term tbl2) in
   let proofs2 = Hashtbl.create 19 in
   let add_proof term =
      match extract_proof term with
         Some(name, pf) ->
            Hashtbl.add proofs2 name (term, trivial_proof pf)
       | None ->
            ()
   in
   let () = List.iter add_proof t2 in
   let process1 t =
      match extract_proof t with
         Some (name, pf) ->
            eprintf "Rule/Rewrite %s:\n\t" name;
            begin match trivial_proof pf, Hashtbl.find_all proofs2 name with
               (Real|Primitive), [] ->
                  eprintf "%s has a non-trivial proof,\n\t%s has none,\n\tkeeping %s one@." inp1 inp2 inp1;
                  Some t
             | Empty, [] ->
                  eprintf "%s has an empty proof,\n\t%s has none,\n\tdropping it@." inp1 inp2;
                  None
             | Empty, (t, (Real|Primitive)) :: _ ->
                  eprintf "%s has an empty proof,\n\t%s has a non-trivial one,\n\tkeeping %s one@." inp1 inp2 inp2;
                  Hashtbl.remove proofs2 name;
                  Some t
             | Primitive, (t, Real) :: _ ->
                  eprintf "%s has a primitive axiom,\n\t%s has a non-trivial proof,\n\tkeeping %s one@." inp1 inp2 inp2;
                  Hashtbl.remove proofs2 name;
                  Some t
             | (Real|Primitive), (_, Empty) :: _ ->
                  eprintf "%s has a non-trivial proof,\n\t%s has an empty one,\n\tkeeping %s one@." inp1 inp2 inp1;
                  Hashtbl.remove proofs2 name;
                  Some t
             | Real, (_, Primitive) :: _ ->
                  eprintf "%s has a non-trivial proof,\n\t%s has a primitive axiom,\n\tkeeping %s one@." inp1 inp2 inp1;
                  Hashtbl.remove proofs2 name;
                  Some t
             | Real, (t, Real) :: _
             | Primitive, (t, Primitive) :: _ ->
                  eprintf "both %s\n\tand %s have non-trivial proofs,\n\tkeeping %s one@." inp1 inp2 inp2;
                  Hashtbl.remove proofs2 name;
                  Some t
             | Empty, (t, Empty) :: _ ->
                  eprintf "both %s\n\tand %s have empty proofs,\n\tdropping both@." inp1 inp2;
                  Hashtbl.remove proofs2 name;
                  None
            end
       | None ->
            Some t
   in
   let t1 = Lm_list_util.some_map process1 t1 in
   let process2 t =
      match extract_proof t with
         Some (name, pf) ->
            begin match Hashtbl.find_all proofs2 name with
               [] ->
                  (* already covered by process1 *)
                  None
             | (_, triv) :: _ ->
                  eprintf "Rule/Rewrite %s:\n\t" name;
                     if triv = Empty then begin
                        eprintf "%s has no proof,\n\t%s has an empty one,\n\tdropping it@." inp1 inp2;
                        None
                     end else begin
                        eprintf "%s has no proof,\n\t%s has a non-trivial one,\n\tkeeping it@." inp1 inp2;
                        Some t
                     end
            end
       | None ->
            Some t
   in
   let t2 = Lm_list_util.some_map process2 t2 in
   let t = mk_xlist_term (t1 @ t2) in
   let output_term out_chan =
      let major, minor, rev = File_type_base.unpack_version (List.hd Filter_magic.ascii_versions) in Printf.fprintf out_chan "#PRL version %d.%d.%d ASCII term\n#Warning: this file was created by using the merge_prla binary\n#\tThis means that it might not actually have the above version and should not be ever committed to SVN\n" major minor rev;
         output_term tbl1 (make_simple_control out_chan) t
   in
      with_output_file outp output_term;
      printf "\nOutput file %s was created successfully.@." outp;
      printf "However there may be some strange issues if you commit this file as is.@.";
      printf "Please recompite MetaPRL and then re-export the theory before committing the file.@."

(*
 * Everything standard is taken care of.
 * There should be a single file argument, so
 * process it.
 *)
let _ =
   if Array.length Sys.argv <> 4 then begin
      eprintf "Usage: %s input1.prla input2.prla output.prla\n\tIf both inputs have proofs of a theorem, the second one \"wins\"@." Sys.argv.(0);
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
