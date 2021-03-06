(*
 * Merge two .prla files (creating a file that contains an "append" of the two),
 * or inspect proofs in one .prla file.
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

open Lm_debug
open Format
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
 * I am hoping that it would become much easiere to do this once the new_io branch is
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

let string_of_term = Dform.string_of_term Dform.null_base

type proof = Proof of int list * string * proof list * proof list
           | Goal

let ext_rule_op = mk_opname "ext_rule" summary_opname

let dest_proof term =
   let term = one_subterm term in
   let rec dest_rule nums num term =
      let opname = opname_of_term term in
         if Opname.eq opname ext_rule_op then
            let tac = dest_string_param term in
            let _, _, subgoals, extras = four_subterms term in
            let subgoals = (dest_xlist subgoals) in
            let extras = (dest_xlist extras) in
            let nums = num :: nums in
            let ns = ref 1 in
            (* Encode numbering into proofs *)
            let dest_goals g =
               let n = !ns in
                  ns := 1 + n;
                  dest_rule nums n g
            in
            let subgoals = List.map dest_goals subgoals in
            let extras = List.map dest_goals extras in
               Proof (nums, tac, subgoals, extras)
         else Goal
   in dest_rule [] 0 term

let format_num num =
   String.concat "." (List.map Int.to_string (List.rev num))

let format_proof =
   let rec format_list f ppf = function
      [x] -> f ppf x
    | (x :: xs) -> fprintf ppf "%a@,%a" f x (format_list f) xs
    | [] -> ()
   in
   let format_tac ppf (nums, tac) = fprintf ppf "@[<hv>%s by@ %s@]" (format_num nums) tac in
   let rec format_aux ppf = function
      (Proof (nums, tac, subgoals, extras)) ->
         let format_goals ppf = function
            [] -> fprintf ppf "No Subgoals"
          | subgoals -> format_list format_aux ppf subgoals in
         let format_extras ppf = function
            (num, []) -> fprintf ppf "Close %s" (format_num nums)
          | (num, extras) -> fprintf ppf "%s Has Xtra: @[<v>%a@]" (format_num nums)
                             (format_list format_aux) extras in
            fprintf ppf "@[<v 0>@[<v 2>%a@ %a@]@,%a@]"
            format_tac (nums, tac) format_goals subgoals format_extras (nums, extras)
    | Goal -> fprintf ppf "<< Goal >>"
   in format_aux

let print_prla input =
   let tbl = with_input_file input read_table in
   let t = dest_xlist (get_term tbl) in
   let print_proof term =
      match extract_proof term with
         Some (name, pf) ->
            let fmt ppf () = match trivial_proof pf with
                                Empty -> fprintf ppf "No proof"
                              | Real -> fprintf ppf "@[<3>Proof:@ %a@]" format_proof (dest_proof pf)
                              | Primitive -> fprintf ppf "Prim" in
               printf "@[<hov 3>Rule/Rewrite %s:@ %a@]@." name fmt ()
       | None -> ()
   in
      List.iter print_proof t


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
   let t1 = List.filter_map process1 t1 in
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
   let t2 = List.filter_map process2 t2 in
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
   match Array.length Sys.argv with
      4 -> Filter_exn.print_exn Dform.null_base (Some "Filter_merge_prla") merge_prla (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3));
           exit 0
    | 2 -> print_prla Sys.argv.(1)
    | _ -> eprintf "@[<2>Usage: %s input1.prla [input2.prla output.prla]@ %s@ %s@]@." Sys.argv.(0)
           "If both inputs have proofs of a theorem, the second one \"wins\"."
           "If only one file is supplied, it prints out proofs in that .prla file.";
           exit 1

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
