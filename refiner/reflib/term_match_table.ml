(*
 * Make a hashtable for terms based on patterns.
 * Essentially, we want to be able to construct tables of pairs
 *    (pattern, 'a)
 * where pattern is a pattern that matches terms.  The lookup
 * function:
 *    lookup : table -> term -> 'a
 * should retrieve the value with the most specific pattern match.
 *
 * This implementation uses a discrimination tree, and term templates
 * are used as discrimination keys.
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
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_symbol
open Lm_printf
open Opname

open Term_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermSubst
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError

open Simple_print.SimplePrint
open Mp_resource

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_match_table%t"

let debug_term_table =
   create_debug (**)
      { debug_name = "term_table";
        debug_description = "Display Term_table (term hashtable) operations";
        debug_value = false
      }

let debug_rewrite = load_debug "rewrite"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

module ShapeTable = Lm_map.LmMake (
struct
   type t = shape
   let compare = Pervasives.compare
end)

(*
 * Entries contain the pattern/value pair.
 *)
type ('a, 'b) info_entry = {
   info_redex : 'b;
   info_value : 'a;
}

(*
 * XXX: TODO: the # of concls selector should allow for conclusion meta-vars,
 * not just a fixed number of conclusions (nogin 2004/06/30).
 *)
type seq_hyps =
   Right of int
 | Left of int
 | Both of int * int
 | Exact of int

(*
 * The discrimination program.  The tree is modeled as a stack machine
 * in the following language:
 *    DtreeAccept info: accept the term if the stack is empty
 *    DtreePop prog: pop the top entry from the stack (wildcard entry)
 *    DtreeFlatten prog: place all the subterms of the current term on the stack
 *                       in case of a sequent, push the arg and keep the term in
 *                       stack
 *    DtreeChoice progs: choose one of the programs (first one that applies)
 *    DtreeSeqHyps*: replace the sequent term on top of the stack with the
 *                   specified number of hypotheses and conclusions.
 *
 * Fail if the end of the program is reached with accepting.
 *
 * Invariants:
 *  (a) a DtreeChoice list can only contain 0 or 1 DtreeFlatten, followed by 0
 *      or 1 DtreePop, followed by 0 or 1 DtreeAccept.
 *  (b) a DtreeSeq list can contain any number of Exact entries in strictly
 *      decreasing order, followed by any number or Both entries with positive
 *      numbers in decreasing order, followed by any number of Left entries
 *      with positive numbers in strictly decreasing order, followed by Right
 *      entries with non-negative numbers in strictly decreasing order.
 *)
type ('a, 'b) internal_term_table =
   DtreeAccept of ('a, 'b) info_entry list
 | DtreePop of ('a, 'b) internal_term_table
 | DtreeFlatten of ('a, 'b) internal_term_table ShapeTable.t
 | DtreeChoice of ('a, 'b) internal_term_table list
 | DtreeSeq of (seq_hyps * ('a, 'b) internal_term_table) list

type ('a, 'b) prog_step =
   ProgAccept of ('a, 'b) info_entry
 | ProgPop
 | ProgFlatten of shape
 | ProgSeq of seq_hyps

(* Invariant: (c) has exactly one ProgAccept step - at the very end *)
type ('a, 'b) term_prog = ('a, 'b) prog_step list

type 'a lazy_lookup = unit -> ('a * 'a lazy_lookup)

type 'a term_table = ('a, rewrite_redex) internal_term_table
type 'a term_map_table = ('a, rewrite_rule) internal_term_table

(************************************************************************
 * Utilities.
 *)

(*
 * XXX HACK (nogin 10/13/2003): We remove all arguments from SO
 * variables to avoid getting an "AllSOInstances" error from the rewriter.
 *)
let simplify_term =
   let nmem vars v = not (SymbolSet.mem vars v) in
   let simplify_var vars t =
      if is_so_var_term t then
         let v, conts, _ = dest_so_var t in mk_so_var_term v (List.filter (nmem vars) conts) []
      else
         t
   in
      fun t -> map_up (simplify_var (free_vars_set t)) t

(************************************************************************
 * Compiling a single pattern.
 *)

(* If we are ready to accept, there is no need to clean up the stack first *)
let rec strip_pops = function
   ProgPop :: l -> strip_pops l
 | l -> l

(* A hack during sequent compilation *)
let sequent_marker =
   mk_simple_term (make_opname ["marker"; "Term_match_table"]) []

let bad_sequent_patt =
   Invalid_argument("Term_match_table.compile_sequent: improper sequent pattern\nA sequent pattern can have at most one context in the hypothesis list")

let compseq_bug = Invalid_argument("Term_match_table.compile_sequent: internal error")

let rec compile_sequent prog stack = function
   [] -> ProgSeq(Exact 0) :: prog, stack
 | Hypothesis (_, t) :: hyps ->
      begin match compile_sequent prog stack hyps with
         ProgSeq(Right 0) :: prog, stack      -> ProgSeq(Left 1) :: prog, t :: stack
       | ProgSeq(Right i) :: _, _             -> ProgSeq(Both(1,i)) :: prog, t :: stack
       | ProgSeq(Exact i) :: prog, stack      -> ProgSeq(Exact (i+1)) :: prog, t :: stack
       | ProgSeq(Left i) :: prog, stack       -> ProgSeq(Left (i+1)) :: prog, t :: stack
       | ProgSeq(Both (i, j)) :: prog, stack  -> ProgSeq(Both (i+1, j)) :: prog, t :: stack
       | _                                       -> raise compseq_bug
      end
 | Context _ :: hyps ->
      begin match compile_sequent prog stack hyps with
         ProgSeq(Exact i) :: prog, stack -> ProgSeq(Right i) :: prog, stack
       | ProgSeq _ :: _, _                   -> raise bad_sequent_patt
       | _                                   -> raise compseq_bug
      end

let rec compile info prog = function
   [] ->
      List.rev (ProgAccept info :: (strip_pops prog))
 | mk :: t :: ts when mk == sequent_marker ->
      let eseq = explode_sequent t in
      let hyps = SeqHyp.to_list eseq.sequent_hyps in
      let prog, stack = compile_sequent prog (eseq.sequent_concl :: ts) hyps in
         compile info prog stack
 | mk :: _ when mk == sequent_marker -> raise(Invalid_argument "Term_match_table.compile")
 | t :: ts when is_so_var_term t ->
      compile info (ProgPop :: prog) ts
 | t :: ts ->
      let s = shape_of_term t in
         if s == sequent_shape then
            let eseq = explode_sequent t in
            let stack = eseq.sequent_args :: sequent_marker :: t :: ts in
               compile info (ProgFlatten s :: prog) stack
         else
            compile info (ProgFlatten s :: prog) ((subterms_of_term t) @ ts)

(************************************************************************
 * Combining multiple patterns.
 *)

let empty_table = DtreeChoice []
let empty_map_table = empty_table

let tbl_list_of_tbl = function
   DtreeChoice l -> l
 | tbl -> [tbl]

let choice tbl = function
   [] -> tbl
 | tbls -> DtreeChoice (tbl::tbls)

let choice2 tbl1 tbl2 =
   DtreeChoice (tbl1 :: (tbl_list_of_tbl tbl2))

(*
 * Add a new program to an existing table.
 * The new entry is considered to be the "freshest".
 *)
let rec add_prog tbl prog =
   add_to_choice (tbl_list_of_tbl tbl) prog

and add_to_choice choices prog =
   match choices, prog with
      _, [] ->
         raise (Invalid_argument "Term_match_table.add_to_choice: bug")
    | [], ProgAccept i :: _ -> (* invariant: _ matches an empty list here *)
         DtreeAccept [i]
    | DtreeAccept is :: _, ProgAccept i :: _ -> (* invariant: both _ match an empty list here *)
         DtreeAccept (i::is)
    | DtreePop tbl :: tl, ProgPop :: prog ->
         choice (DtreePop (add_prog tbl prog)) tl
    | ((DtreeAccept _ :: _) | []) , ProgPop :: prog ->
         choice (DtreePop (add_to_choice [] prog)) choices
    | hd :: tl, (ProgPop | ProgAccept _) :: _ ->
         choice2 hd (add_to_choice tl prog)
    | DtreeFlatten stbl :: tl, ProgFlatten s :: prog ->
         choice (DtreeFlatten (ShapeTable.filter_add stbl s (add_option prog))) tl
    | _, ProgFlatten s :: prog ->
         choice (DtreeFlatten (ShapeTable.filter_add ShapeTable.empty s (add_option prog))) choices
    | [], ProgSeq knd :: prog ->
         DtreeSeq [knd, add_to_choice [] prog]
    | [DtreeSeq(lst)], (ProgSeq knd :: prog) ->
         DtreeSeq (seq_add_to_choice prog knd lst)
    | ((DtreeSeq _ :: _), _) | (_, ProgSeq _ :: _) ->
         raise (Invalid_argument "Term_match_table.add_to_choice: internal error - sequent mismatch")

and add_option prog = function
   None -> add_to_choice [] prog
 | Some tbl -> add_prog tbl prog

and seq_add_to_choice prog knd lst =
   match lst, knd with
      (knd', _) as hd :: tl, _ when knd < knd' ->
         hd :: (seq_add_to_choice prog knd tl)
    | (knd', tbl) :: rest, _ when knd = knd' ->
         (knd, add_prog tbl prog) :: rest
    | _ , _ ->
         (knd, add_to_choice [] prog) :: lst

(*
 * Add an entry.
 *)
let add_item tbl t v =
   let t = simplify_term t in
   let item = compile { info_redex = compile_redex Relaxed [||] t; info_value = v } [] [t] in
      add_prog tbl item

let add_map tbl t1 t2 v =
   let t1 = simplify_term t1 in
   let t2 = List.map simplify_term t2 in
   let item = compile { info_redex = term_rewrite Relaxed empty_args_spec [t1] t2 ; info_value = v } [] [t1] in
      add_prog tbl item

let table_resource_info extract =
   Functional {
      fp_empty = empty_table;
      fp_add = (fun tbl (t, v) -> add_item tbl t v);
      fp_retr = extract
   }

let rmap_table_resource_info extract =
   Functional {
      fp_empty = empty_map_table;
      fp_add = (fun tbl (t1, t2, v) -> add_map tbl t1 t2 v);
      fp_retr = extract
   }

(************************************************************************
 * DTREE EXECUTION                                                      *
 ************************************************************************)

let rec execute search fallbacks stack = function
   DtreePop prog ->
      if !debug_term_table then
         eprintf "Term_table.execute: DtreePop: %d@." (List.length stack);
      begin match stack with
         h :: t ->
            execute search fallbacks t prog
       | [] ->
            raise (Invalid_argument "Term_match_table.execute: bug")
      end
 | DtreeFlatten stbl ->
      begin match stack with
         t :: ts ->
            if !debug_term_table then
               eprintf "Term_table.execute: DtreeFlatten %s@." (string_of_term t);
            let s = shape_of_term t in
            if ShapeTable.mem stbl s then
               let prog = ShapeTable.find stbl s in
               if s == sequent_shape then
                  execute search fallbacks ((explode_sequent t).sequent_args :: stack) prog
               else
                  execute search fallbacks ((subterms_of_term t) @ ts) prog
            else
               execute_fallback search fallbacks
       | [] ->
            raise (Invalid_argument "Term_match_table.execute: bug")
      end
 | DtreeChoice progs ->
      if !debug_term_table then
         eprintf "Term_table.execute: DtreeChoice@.";
      execute_fallback search ((progs, stack) :: fallbacks)
 | DtreeAccept infos ->
      if !debug_term_table then begin
         eprintf "Term_table.execute: DtreeAccept: %d@." (List.length stack);
      end;
      search fallbacks infos
 | DtreeSeq(lst) ->
      begin match stack with
         t :: ts ->
            if !debug_term_table then
               eprintf "Term_table.execute: DtreeSeq %s@." (string_of_term t);
            let eseq = explode_sequent t in
            let l = SeqHyp.length eseq.sequent_hyps in
               execute_sequent search fallbacks stack eseq.sequent_hyps l (eseq.sequent_concl::ts) lst
       | [] ->
            raise (Invalid_argument "Term_match_table.execute: bug")
      end

and execute_fallback search = function
   ([prog], stack) :: progs ->
      execute search progs stack prog
 | ([], _) :: progs ->
      execute_fallback search progs
 | ((prog::progs), stack) :: fallbacks ->
      execute search ((progs, stack) :: fallbacks) stack prog
 | [] ->
      raise Not_found

and execute_sequent search fallbacks old_stack hyps l stack = function
   [] ->
      execute_fallback search fallbacks
 | ((Exact i|Left i|Right i), _) :: lst when i > l ->
      execute_sequent search fallbacks old_stack hyps l stack lst
 | (Exact i, _) :: lst when i <> l ->
      execute_sequent search fallbacks old_stack hyps l stack lst
 | (Both (i1,i2), _) :: lst when (i1 + i2) > l ->
      execute_sequent search fallbacks old_stack hyps l stack lst
 | (knd, tbl) :: lst ->
      let fallbacks =
         if lst = [] then fallbacks else (([DtreeSeq lst], old_stack)::fallbacks)
      in
         match knd with
            Exact i | Right i ->
               execute_hyps search fallbacks tbl hyps stack i (l-1)
          | Left i ->
               execute_hyps search fallbacks tbl hyps stack i (i - 1)
          | Both (i1, i2) ->
               execute_hyps_both search fallbacks tbl hyps stack i1 i2 (l-1)

and execute_hyps search fallbacks tbl hyps stack num last =
   if num = 0 then
      execute search fallbacks stack tbl
   else match SeqHyp.get hyps last with
      Hypothesis (_, t) ->
         execute_hyps search fallbacks tbl hyps (t::stack) (num-1) (last-1)
    | Context _ ->
         execute_fallback search fallbacks

and execute_hyps_both search fallbacks tbl hyps stack lnum num last =
   if num = 0 then
      execute_hyps search fallbacks tbl hyps stack lnum (lnum-1)
   else match SeqHyp.get hyps last with
      Hypothesis (_, t) ->
         execute_hyps_both search fallbacks tbl hyps (t::stack) lnum (num-1) (last-1)
    | Context _ ->
         execute_fallback search fallbacks

(*
 * Lookup an entry.
 *)
let rec search_infos get f t fallbacks = function
   info :: tl when f info.info_value ->
      begin try
         get info f t tl fallbacks
      with RefineError _ | Not_found ->
         search_infos get f t fallbacks tl
      end
 | _ :: tl ->
      search_infos get f t fallbacks tl
 | [] ->
      execute_fallback (search_infos get f t) fallbacks

let lookup_aux get tbl f t =
   execute (search_infos get f t) [] [t] tbl

let get_val info _ t _ _ =
   ignore(apply_redex info.info_redex [||] t []);
   info.info_value

let get_val_rwi info _ t _ _ =
   (apply_redex info.info_redex [||] t []), info.info_value

let rec get_val_all info f t tl fallbacks =
   (get_val info f t tl fallbacks), get_val_all_aux f t tl fallbacks
and get_val_all_aux f t tl fallbacks () = search_infos get_val_all f t fallbacks tl

let rec get_val_bucket =
   let rec aux f t = function
      [] -> []
    | info :: tl when f info.info_value ->
         let tl = aux f t tl in
         begin try
            ignore(apply_redex info.info_redex [||] t []);
            info.info_value :: tl
         with RefineError _ ->
            tl
         end
    | _ :: tl ->
      aux f t tl
   in (fun info f t tl fallbacks ->
      match aux f t (info :: tl) with
         [] -> execute_fallback (search_infos get_val_bucket f t) fallbacks
       | l -> l)

let get_val_rmap info _ t _ _ =
   apply_rewrite info.info_redex empty_args t [], info.info_value

let lookup tbl f t = lookup_aux get_val tbl f t
let lookup_rwi tbl f t = lookup_aux get_val_rwi tbl f t
let lookup_all tbl f t () = lookup_aux get_val_all tbl f t
let lookup_bucket tbl f t = lookup_aux get_val_bucket tbl f t
let lookup_rmap tbl f t = lookup_aux get_val_rmap tbl f t

let select_all _ = true

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
