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
open Format

open Lm_symbol
open Lm_debug
open Opname

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermSubst
open Refiner.Refiner.Rewrite

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

(*
 * We override the shapes to include shapes of sequents.
 *)
type shape =
   TermShape of TermShape.shape
 | SequentShapeFirst of int
 | SequentShapeLast of int
 | SequentShapeAll of int

(*
 * The discrimination tree.  The tree is modeled as a nondeterministic
 * stack machine in the following language:
 *    DtreeAccept info: accept the term if the stack is empty
 *    DtreeEnd prog: current term should be finished
 *    DtreePop prog: pop the top entry from the stack
 *    DtreeFlatten prog: place all the subterms of the current term on the stack
 *    DtreeChoice progs: choose one of the programs
 *
 * Fail if the end of the program is reached with accepting.
 *)
type 'a dtree_prog =
   DtreeAccept of 'a info_entry list
 | DtreeEnd of 'a dtree_prog
 | DtreePop of 'a dtree_prog
 | DtreeFlatten of shape * 'a dtree_prog
 | DtreeChoice of 'a dtree_prog list

(*
 * Entries contain the pattern/value pair.
 *)
and 'a info_entry =
   { info_term  : term;
     info_redex : rewrite_redex;
     info_value : 'a
   }

(*
 * Real tree uses a hastable to perform initial selection.
 *)
type 'a term_table = (TermShape.shape, 'a dtree_prog) Hashtbl.t

(*
 * Shape equality exception.
 *)
exception MatchError

(*
 * We need a particular term to represent a term conclusion.
 *)
let end_marker = mk_xstring_term "end_marker"

(************************************************************************
 * Printing.
 *)

(*
 * Print the flatten arg.
 *)
let pp_print_shape out shape =
   match shape with
      TermShape shape ->
         TermShape.pp_print_shape out shape
    | SequentShapeFirst len ->
         fprintf out "SequentShapeFirst(%d)" len
    | SequentShapeLast len ->
         fprintf out "SequentShapeLast(%d)" len
    | SequentShapeAll len ->
         fprintf out "SequentShapeAll(%d)" len

(*
 * Print out a program.
 *)
let pp_print_prog out prog =
   let rec print out = function
      DtreeAccept l ->
         let print_info { info_term = t } =
            fprintf out "@ %s" (string_of_term t)
         in
            fprintf out "@[<v 3>Accept:";
            List.iter print_info l;
            fprintf out "@]"
    | DtreeEnd prog ->
         fprintf out "DtreeEnd:@ %a" print prog
    | DtreePop prog ->
         fprintf out "DtreePop:@ %a" print prog
    | DtreeFlatten (shape, prog) ->
         fprintf out "DtreeFlatten %a:@ %a" pp_print_shape shape print prog
    | DtreeChoice progs ->
         fprintf out "@[<v 3>DtreeChoice:";
         List.iter (fun prog -> fprintf out "@ --@ %a" print prog) progs;
         fprintf out "@]"
   in
      fprintf out "@[<v 0>%a@]@." print prog

(************************************************************************
 * Shapes.
 *)

(*
 * Equality of shapes.
 *)
let shape_eq shape1 shape2 =
   match shape1, shape2 with
      TermShape shape1, TermShape shape2 ->
         TermShape.eq shape1 shape2
    | SequentShapeFirst i1, SequentShapeFirst i2
    | SequentShapeLast i1, SequentShapeLast i2
    | SequentShapeAll i1, SequentShapeAll i2 ->
         i1 = i2
    | _ ->
         false

(*
 * Override shape_of_term so that it will work on sequents too.
 * We distinguish between prefix and suffix matches.
 *)
let is_context_hyp hyps i =
   match SeqHyp.get hyps i with
      Context _ ->
         true
    | HypBinding _
    | Hypothesis _ ->
         false

let shape_of_sequent_pattern t =
   let { sequent_hyps = hyps;
         sequent_goals = goals
       } = explode_sequent t
   in
   let hyp_count = SeqHyp.length hyps in
      if hyp_count = 0 then
         SequentShapeAll 0
      else if is_context_hyp hyps 0 then
         SequentShapeLast (hyp_count - 1)
      else if is_context_hyp hyps (hyp_count - 1) then
         SequentShapeFirst (hyp_count - 1)
      else
         SequentShapeAll hyp_count

let shape_of_pattern t =
   if is_sequent_term t then
      shape_of_sequent_pattern t
   else
      TermShape (TermShape.shape_of_term t)

(*
 * Toplevel shape.
 *)
let shape_of_term_toplevel =
   (* Make an arbitrary shape for all sequents *)
   let opname = mk_opname "sequent" nil_opname in
   let sequent_term = mk_simple_term opname [] in
   let sequent_shape = TermShape.shape_of_term sequent_term in
      (fun t ->
            if is_sequent_term t then
               sequent_shape
            else
               TermShape.shape_of_term t)

(************************************************************************
 * Term flattening.
 *)

(*
 * We allow explicit matching on sequent contexts.
 *)
let context_term =
   let opname = mk_opname "context" nil_opname in
      mk_simple_term opname []

(*
 * Override subterms_of_term so that it works on sequents also,
 * using the flatten_arg to tell between sequents and other terms.
 *)
let term_of_hyp = function
   HypBinding (_, t)
 | Hypothesis t ->
      t
 | Context _ ->
      context_term

(*
 * Split the hyp list.
 *)
let flatten_hyps_sub hyps goals off len =
   let rec collect l i =
      let i = pred i in
         if i < off then
            l
         else
            collect (term_of_hyp (SeqHyp.get hyps i) :: l) i
   in
      collect goals (off + len)

(*
 * Flatten the sequent list.
 *)
let flatten_hyps t skip len =
   if is_sequent_term t then
      let { sequent_hyps  = hyps;
            sequent_goals = goals;
            sequent_args  = arg
          } = explode_sequent t
      in
      let length = SeqHyp.length hyps in
      let () =
         if length < len then
            raise MatchError
      in
      let terms = SeqGoal.to_list goals in
      let terms = flatten_hyps_sub hyps terms (skip length) len in
         arg :: terms
   else
      raise MatchError

(*
 * Flatten all the hyps.
 *)
let flatten_all_hyps t len =
   if is_sequent_term t then
      let { sequent_hyps  = hyps;
            sequent_goals = goals;
            sequent_args  = arg
          } = explode_sequent t
      in
         if SeqHyp.length hyps < len then
            raise MatchError;
         arg :: (List.map term_of_hyp (SeqHyp.to_list hyps)) @ SeqGoal.to_list goals
   else
      raise MatchError

(*
 * Get the subterms of the term, according to the filter_arg
 * directive.
 *)
let flatten_term shape t =
   match shape with
      TermShape shape ->
         if not (TermShape.eq shape (TermShape.shape_of_term t)) then
            raise MatchError;
         subterms_of_term t
    | SequentShapeFirst len ->
         flatten_hyps t (fun length -> 0) len
    | SequentShapeLast len ->
         flatten_hyps t (fun length -> length - len) len
    | SequentShapeAll len ->
         flatten_all_hyps t len

(************************************************************************
 * Utilities.
 *)

(*
 * XXX HACK (nogin 10/13/2003): When a term is inserted, we have to simplify
 * it to erase the distinction between different kinds of external variables.
 * We especially want to make sure that terms that differ only by contexts
 * they are in end up placed in the same cell of the table.
 *
 * We should probably be able to get rid of this once a proper fall-back mechanism
 * (beyond the current single-cell one) is implemented.
 *
 * XXX HACK (nogin 10/13/2003): In addition, we remove all arguments from SO
 * variables to avoid getting an "AllSOInstances" error from the rewriter.
 *)
let simplify_term t =
   let simplify_var vars t =
      if is_var_term t then
         let v = dest_var t in
            if SymbolSet.mem vars v then mk_so_var_term v [] [] else t
      else if is_so_var_term t then
         let v, _, _ = dest_so_var t in mk_so_var_term v [] []
      else
         t
   in
      map_up (simplify_var (free_vars_set t)) t

(*
 * Add an entry.
 *)
let make_info (t, v) =
   let t = simplify_term t in
   let redex = compile_redex Relaxed [||] t in
      { info_term = t;
        info_redex = redex;
        info_value = v
      }

(************************************************************************
 * Compiling a pattern.
 *)

(*
 * Sort a list of programs based on a flatten argument.
 *)
let sort_progs progs =
   (* Compare programs *)
   let compare prog1 prog2 =
      match prog1, prog2 with
         DtreeFlatten (shape1, _), DtreeFlatten (shape2, _) ->
            (match shape1, shape2 with
                SequentShapeAll i1, SequentShapeAll i2
              | SequentShapeLast i1, SequentShapeLast i2
              | SequentShapeFirst i1, SequentShapeLast i2
              | SequentShapeLast i1, SequentShapeFirst i2
              | SequentShapeFirst i1, SequentShapeFirst i2 ->
                   i2 - i1
              | TermShape _, TermShape _ ->
                   0
              | SequentShapeAll _, SequentShapeFirst _
              | SequentShapeAll _, SequentShapeLast _
              | SequentShapeAll _, TermShape _
              | SequentShapeFirst _, TermShape _
              | SequentShapeLast _, TermShape _ ->
                   -1
              | SequentShapeFirst _, SequentShapeAll _
              | SequentShapeLast _, SequentShapeAll _
              | TermShape _, SequentShapeAll _
              | TermShape _, SequentShapeFirst _
              | TermShape _, SequentShapeLast _ ->
                   1)
       | DtreeFlatten _, _ ->
            -1
       | _, DtreeFlatten _ ->
            1
       | _ ->
            0
   in

   (* Only sort if one of the choices is a sequent *)
   let is_sequent_prog = function
      DtreeFlatten (SequentShapeAll _, _)
    | DtreeFlatten (SequentShapeFirst _, _)
    | DtreeFlatten (SequentShapeLast _, _) ->
         true
    | _ ->
         false
   in

      if List.exists is_sequent_prog progs then
         List.sort compare progs
      else
         progs

(*
 * Collect subterms entries into three kinds:
 *   complete: there should be no more data to collect
 *   now: current term should be finished
 *   skip: skip this particular subterm
 *   select: need a particular subterm
 *)
let collect_stacks compact stacks =
   let rec collect complete now skip select stacks =
      match stacks with
         [] ->
            compact complete, now, skip, select
       | ([], info) :: t ->
            collect (info :: complete) now skip select t
       | (term :: terms, info) :: t ->
            if term == end_marker then
               collect complete ((terms, info) :: now) skip select t
            else if is_so_var_term term then
               collect complete now ((terms, info) :: skip) select t
            else
               let shape = shape_of_pattern term in
               let rec merge = function
                  h' :: t' ->
                     let shape', select = h' in
                        if shape_eq shape shape' then
                           (shape', (term, terms, info) :: select) :: t'
                        else
                           h' :: merge t'
                | [] ->
                     [shape, [term, terms, info]]
               in
                  collect complete now skip (merge select) t
   in
      collect [] [] [] [] stacks

(*
 * Compile a list of term stacks.
 * Select from the head terms.
 * We assume each stack has a head term.
 *)
let rec compile_select compact (shape, selections) =
   let expand_term (term, stack, info) =
      (flatten_term shape term) @ (end_marker :: stack), info
   in
   let stacks = List.map expand_term selections in
      DtreeFlatten (shape, compile_stacks compact stacks)

and compile_stacks compact stacks =
   let complete, now, skip, select = collect_stacks compact (List.rev stacks) in
   let progs =
      if skip <> [] then
         [DtreePop (compile_stacks compact skip)]
      else
         []
   in
   let progs =
      if select <> [] then
         List.map (compile_select compact) select @ progs
      else
         progs
   in
   let progs =
      if now <> [] then
         DtreeEnd (compile_stacks compact now) :: progs
      else
         progs
   in
   let progs =
      if complete <> [] then
         DtreeAccept complete :: progs
      else
         progs
   in
      match progs with
         [prog] ->
            prog
       | _ ->
            DtreeChoice (sort_progs progs)

(*
 * The raw interface assumes the term has already been flattened.
 *)
let compile_prog compact terms =
   let mk_stack info =
      [info.info_term; end_marker], info
   in
   let stacks = List.rev_map mk_stack terms in
      compile_stacks compact stacks

(*
 * Compile the table from the list of entries.
 * First collect all the entries into those that have the
 * same term template, then join all the common ones into
 * programs.
 *)
let create_table items compact =
   (* Create the base of entries with the same shape *)
   let base = Hashtbl.create 97 in
   let insert_entry item =
      let info = make_info item in
      let t = info.info_term in
      let shape' = shape_of_term_toplevel t in
      let entries =
         try Hashtbl.find base shape' with
            Not_found ->
               let entries = ref [] in
                  Hashtbl.add base shape' entries;
                  entries
      in
         Lm_ref_util.push info entries
   in
   let _ = List.iter insert_entry items in

   (* Compile the hastable into a collection of programs *)
   let base' = Hashtbl.create 97 in
   let compile_entries shape entries =
      Hashtbl.add base' shape (compile_prog compact !entries)
   in
      Hashtbl.iter compile_entries base;
      base'

let table_resource_info compact extract =
   let add datas data = data :: datas in
   let retr datas = extract (create_table datas compact) in
      Functional (**)
         { fp_empty = [];
           fp_add = add;
           fp_retr = retr
         }

(************************************************************************
 * DTREE EXECUTION                                                      *
 ************************************************************************)

(*
 * Execute a program, and return the info block.
 * The stack is a list of terms to promote sharing of stacks.
 *)
let split = function
   h::t ->
      if h == end_marker then
         raise Not_found
      else
         h, t
 | [] ->
      raise Not_found

let split_end = function
   h::t ->
      if h == end_marker then
         t
      else
         raise Not_found
 | [] ->
      raise Not_found

let rec execute (search : 'a list -> 'b) stack = function
   DtreePop prog ->
      if !debug_term_table then
         eprintf "Term_table.execute: DtreePop: %d@." (List.length stack);
      execute search (snd (split stack)) prog
 | DtreeFlatten (shape, prog) ->
      let h, t = split stack in
         if !debug_term_table then
            eprintf "Term_table.execute: DtreeFlatten %s@." (string_of_term h);
         execute search ((flatten_term shape h) @ (end_marker :: t)) prog
 | DtreeEnd prog ->
      let t = split_end stack in
         if !debug_term_table then
            eprintf "Term_table.execute: DtreeEnd@.";
         execute search t prog
 | DtreeChoice progs ->
      if !debug_term_table then
         eprintf "Term_table.execute: DtreeChoice@.";
      execute_many search stack progs
 | DtreeAccept info ->
      if !debug_term_table then
         eprintf "Term_table.execute: DtreeAccept: %d@." (List.length stack);
      if stack = [] then
         search info
      else
         raise Not_found

and execute_many search stack = function
   prog :: progs ->
      begin
         try
            execute search stack prog
         with
            Not_found
          | MatchError ->
               execute_many search stack progs
      end
 | [] ->
      raise Not_found

(*
 * Lookup an entry.
 *)
let rec search_infos t = function
   { info_term = t'; info_redex = redex; info_value = v } :: tl ->
      begin
         if !debug_term_table then
            eprintf "Term_table.lookup: try %b:%s@." (is_sequent_term t') (string_of_term t');
         try
            let debug = !debug_rewrite in
            let _ = debug_rewrite := false in
            let items = apply_redex redex [||] t [] in
               debug_rewrite := debug;
               items, v
         with
            exn ->
               if !debug_term_table then
                  eprintf "Term_table.lookup: %s failed@." (string_of_term t');
               search_infos t tl
      end
 | [] ->
      raise Not_found

let lookup base t =
   let shape = shape_of_term_toplevel t in
   let () =
      if !debug_term_table then
         eprintf "Term_table.lookup: %b: %s@." (is_sequent_term t) (string_of_term t)
   in
   let prog = Hashtbl.find base shape in
   let stack = [t; end_marker] in
   let _ =
      if !debug_term_table then
         let print_term t =
            eprintf "  %s@." (string_of_term t)
         in
            eprintf "Term_table.lookup: program@.";
            eprintf "%a@." pp_print_prog prog;
            eprintf "Term_table.lookup: against:@.";
            List.iter print_term stack
   in
   let result =
      try execute (search_infos t) stack prog with
         MatchError ->
            raise Not_found
   in
      if !debug_term_table then
         eprintf "Term_table.lookup: %s found@." (string_of_term t);
      result

(************************************************************************
 * Debugging.
 *)

(*
 * For top-level debugging.
 *)
let save_prog = ref (DtreeAccept [])

let print_term_match terms =
   let info, _ =
      List.fold_left (fun (info, i) t ->
            let info = make_info (t, i) :: info in
               info, succ i) ([], 0) terms
   in
   let info = List.rev info in
   let prog = compile_prog (fun x -> x) info in
      pp_print_prog Format.std_formatter prog;
      save_prog := prog

let eval_term_match t =
   let prog = !save_prog in
   let stack = [t; end_marker] in
   let matches =
      try execute (fun l -> Some l) stack prog with
         Not_found
       | MatchError ->
            None
   in
   let () =
      match matches with
         Some l ->
            printf "@[<b 3>Term match:";
            List.iter (fun info -> printf "@ %d" info.info_value) l;
            printf "@]@."
       | None ->
            printf "Term match: none@."
   in
   let matches =
      try Some (execute (search_infos t) stack prog) with
         Not_found
       | MatchError ->
            None
   in
      match matches with
         Some (_, i) ->
            printf "Term match: %d@." i
       | None ->
            printf "Term match: none@."

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
