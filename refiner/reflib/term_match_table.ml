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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Printf
open Mp_debug
open Opname

open Refiner.Refiner
open Term
open TermOp
open TermMan
open TermMeta
open TermShape
open Rewrite

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
 * The discrimination tree.  The tree is modeled as a nondeterministic
 * stack machine in the following language:
 *    DtreeAccept info: accept the term if the stack is empty
 *    DtreeTerm prog: current term should be finished
 *    DtreePop prog: pop the top entry from the stack
 *    DtreeFlatten prog: place all the subterms of the current term on the stack
 *    DtreeMatch (template, prog): match the current term with the template, fail on no match
 *    DtreeChoice progs: choose one of the programs
 *
 * Fail if the end of the program is reached with accepting.
 *)
type 'a dtree_prog =
   DtreeAccept of 'a info_entry list
 | DtreeTerm of 'a dtree_prog
 | DtreePop of 'a dtree_prog
 | DtreeFlatten of 'a dtree_prog
 | DtreeMatch of shape * 'a dtree_prog
 | DtreeChoice of 'a dtree_prog list

(*
 * Entries contain the pattern/value pair.
 *)
and 'a info_entry =
   { info_term : term;
     info_redex : rewrite_redex;
     info_value : 'a
   }

(*
 * Real tree uses a hastable to perform initial selection.
 *)
type 'a term_table = (shape, 'a dtree_prog) Hashtbl.t

(*
 * We need a particular term to represent a term conclusion.
 *)
let end_marker = mk_xstring_term "end_marker"

(*
 * Sequents are handled specially.
 *)
let sequent_term =
   let opname = mk_opname "sequent" nil_opname in
      mk_simple_term opname [mk_var_term "ext"; mk_var_term "hyps"]

let shape_of_term t =
   shape_of_term (if is_sequent_term t then sequent_term else t)

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * When a term is inserted, we have to simplify it so that the rewriter does
 * not complain.
 *)
let simplify_term t =
   let simplify_var t =
      if is_so_var_term t then
         mk_var_term (fst (dest_so_var t))
      else
         t
   in
      map_up simplify_var t

(*
 * Add an entry.
 *)
let make_info (t,v) =
   let t = simplify_term t in
   let redex, _ = compile_redex Relaxed [||] t in
   { info_term = t;
     info_redex = redex;
     info_value = v
   }

(************************************************************************
 * DTREE COMPILATION                                                    *
 ************************************************************************)

(*
 * Print out a program.
 *)
let tab out stop =
   for i = 1 to stop do
      output_char out ' '
   done

let print_prog out prog =
   let rec print tabstop = function
      DtreeAccept l ->
         let print_info { info_term = t } =
            fprintf out "%a%s\n" tab (tabstop + 2) (string_of_term t)
         in
            fprintf out "%aAccept:\n" tab tabstop;
            List.iter print_info l
    | DtreeTerm prog ->
         fprintf out "%aDtreeTerm\n" tab tabstop;
         print tabstop prog
    | DtreePop prog ->
         fprintf out "%aDtreePop\n" tab tabstop;
         print tabstop prog
    | DtreeFlatten prog ->
         fprintf out "%aDtreeFlatten\n" tab tabstop;
         print tabstop prog
    | DtreeMatch (shape, prog) ->
         fprintf out "%aDtreeShape: %a\n" tab tabstop print_shape shape;
         print tabstop prog
    | DtreeChoice progs ->
         fprintf out "%aDtreeChoice:\n" tab tabstop;
         List.iter (print (tabstop + 2)) progs
   in
      print 0 prog;
      flush out

(*
 * Collect subterms entries into three kinds:
 *   complete: there should be no more data to collect
 *   now: current term should be finished
 *   skip: skip this particular subterm
 *   select: need a particular subterm
 *)
let collect_stacks compact stacks =
   let rec collect complete now skip select = function
      [] ->
         compact complete, now, skip, select
    | ([], info) :: t ->
         collect (info :: complete) now skip select t
    | (term :: terms, info) :: t ->
         if term == end_marker then
            collect complete ((terms, info) :: now) skip select t
         else if is_var_term term then
            collect complete now ((terms, info) :: skip) select t
         else
            let shape = shape_of_term term in
            let rec merge = function
               h' :: t' ->
                  let shape', select = h' in
                     if shape = shape' then
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
      (subterms_of_term term) @ (end_marker :: stack), info
   in
   let stacks = List.map expand_term selections in
      DtreeMatch (shape, DtreeFlatten (compile_stacks compact stacks))

and compile_stacks compact stacks =
   match collect_stacks compact (List.rev stacks) with
      complete, [], [], [] ->
         DtreeAccept complete
    | [], now, [], [] ->
         DtreeTerm (compile_stacks compact now)
    | [], [], skip, [] ->
         DtreePop (compile_stacks compact skip)
    | complete, now, skip, select ->
         DtreeChoice ((DtreeAccept complete
                       :: (DtreeTerm (compile_stacks compact now))
                       :: (List.map (compile_select compact) select))
                      @ [DtreePop (compile_stacks compact skip)])

(*
 * The raw interface assumes the term has already been falttened.
 *)
let compile_prog compact terms =
   let mk_stack info =
      (subterms_of_term info.info_term) @ [end_marker], info
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
      let shape' = shape_of_term t in
      let entries =
         try Hashtbl.find base shape' with
            Not_found ->
               let entries = ref [] in
                  Hashtbl.add base shape' entries;
                  entries
      in
         Ref_util.push info entries
   in
   let _ = List.iter insert_entry items in

   (* Compile the hastable into a collection of programs *)
   let base' = Hashtbl.create 97 in
   let compile_entries template entries =
      Hashtbl.add base' template (compile_prog compact !entries)
   in
      Hashtbl.iter compile_entries base;
      base'

let table_resource_info compact extract =
   let add datas data = data :: datas in
   let retr datas = extract (create_table datas compact) in
   Functional {
      fp_empty = [];
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

let rec execute prog stack =
   match prog with
      DtreePop prog ->
         if !debug_term_table then
            eprintf "Term_table.execute: DtreePop: %d%t" (List.length stack) eflush;
         execute prog (snd (split stack))
    | DtreeMatch (shape, prog) ->
         let h, _ = split stack in
         let shape' = shape_of_term h in
            if !debug_term_table then
               eprintf "Term_table.execute: DtreeMatch: %a vs %a%t" (**)
                  print_shape shape print_shape shape' eflush;
            if TermShape.eq shape shape' then
               begin
                  if !debug_term_table then
                     eprintf "Term_table.execute: DtreeMatch: succeeded%t" eflush;
                  execute prog stack
               end
            else
               begin
                  if !debug_term_table then
                     eprintf "Term_table.execute: DtreeMatch: failed%t" eflush;
                  raise Not_found
               end
    | DtreeFlatten prog ->
         let h, t = split stack in
            if !debug_term_table then
               eprintf "Term_table.execute: DtreeFlatten %s%t" (string_of_term h) eflush;
            execute prog ((subterms_of_term h) @ (end_marker :: t))
    | DtreeTerm prog ->
         let t = split_end stack in
            if !debug_term_table then
               eprintf "Term_table.execute: DtreeTerm%t" eflush;
            execute prog t
    | DtreeChoice progs ->
         if !debug_term_table then
            eprintf "Term_table.execute: DtreeChoice%t" eflush;
         let rec search = function
            prog::progs ->
               begin
                  try execute prog stack with
                     Not_found ->
                        search progs
               end
          | [] ->
               raise Not_found
         in
            search progs
    | DtreeAccept info ->
         if !debug_term_table then
            eprintf "Term_table.execute: DtreeAccept: %d%t" (List.length stack) eflush;
         if stack = [] then
            info
         else
            raise Not_found

(*
 * Lookup an entry.
 *)
let lookup base t =
   let shape = shape_of_term t in
   let _ =
      if !debug_term_table then
         eprintf "Term_table.lookup: %s%t" (string_of_term t) eflush
   in
   let prog = Hashtbl.find base shape in
   let stack = (subterms_of_term t) @ [end_marker] in
   let _ =
      if !debug_term_table then
         let print_term t =
            eprintf "  %s%t" (string_of_term t) eflush
         in
            eprintf "Term_table.lookup: program\n";
            print_prog stderr prog;
            eprintf "Term_table.lookup: against:\n";
            List.iter print_term stack
   in
   let items = execute prog stack in
   let rec search = function
      { info_term = t'; info_redex = redex; info_value = v } :: tl ->
         begin
            if !debug_term_table then
               eprintf "Term_table.lookup: try %s%t" (string_of_term t') eflush;
            try
               let _, items =
                  let debug = !debug_rewrite in
                  let _ = debug_rewrite := false in
                  let x = apply_redex' redex [||] t [] in
                     debug_rewrite := debug;
                     x
               in
                  items, v
            with
               _ ->
                  if !debug_term_table then
                     eprintf "Term_table.lookup: %s failed%t" (string_of_term t') eflush;
                  search tl
         end
    | [] ->
         raise Not_found
   in
   let triple = search items in
      if !debug_term_table then
         eprintf "Term_table.lookup: %s found%t" (string_of_term t) eflush;
      triple

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
