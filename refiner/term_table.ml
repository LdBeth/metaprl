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
 *)

open Printf
open Debug
open Opname
open Term
open Term_util
open Rewrite

open Simple_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_table%t" eflush

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
type 'a dtree = (shape, 'a dtree_prog) Hashtbl.t

(*
 * We can add entries, as well as other tables.
 * When the tables are compiled, common ancestors
 * are merged.
 *)
type 'a table_item =
   Entry of 'a info_entry
 | Table of 'a table_item list

(*
 * A table has a list of pairs, plus an position for a compute
 * table.
 *)
type 'a term_table =
   { tbl_items : 'a table_item list;
     mutable tbl_base : 'a dtree option
   }

(*
 * Destruction.
 *)
type 'a table_entry =
   TableEntry of term * 'a
 | TableTable of 'a term_table

(*
 * We need a particular term to represent a term conclusion.
 *)
let end_marker = mk_xstring_term "end_marker"

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty table contains nothing.
 *)
let new_table () =
   { tbl_items = [];
     tbl_base = None
   }

(*
 * Destruction.
 *)
let is_empty_table = function
   { tbl_items = [] } ->
      true
 | _ ->
      false

let equal_tables { tbl_items = items1 } { tbl_items = items2 } =
   items1 == items2

let dest_table { tbl_items = items } =
   match items with
      [] ->
         raise (Invalid_argument "dest_table")
    | x::tl ->
         let entry =
            match x with
               Entry { info_term = t; info_value = x } ->
                  TableEntry (t, x)
             | Table t ->
                  TableTable { tbl_items = t; tbl_base = None }
         in
            entry, { tbl_items = tl; tbl_base = None }

(*
 * Add an entry.
 *)
let insert { tbl_items = items } t v =
   let redex = compile_redex [||] t in
   let entry =
      { info_term = t;
        info_redex = redex;
        info_value = v
      }
   in
      if !debug_term_table then
         eprintf "Term_table.insert: %s%t" (string_of_term t) eflush;
      { tbl_items = Entry entry :: items; tbl_base = None }

(*
 * Join another table.
 *)
let join_tables { tbl_items = entries } { tbl_items = items } =
   { tbl_items = Table items :: entries; tbl_base = None }

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
 * Collect the entries into a single list.
 * Keep a list of tables that have been inserted so that each table is only inserted
 * once.  Tables are just lists of items; compare them with physical equality.
 *)
let collect_entries { tbl_items = items } =
   let rec insert_item tables_entries = function
      [] ->
         tables_entries
    | h::t ->
         let tables, entries = tables_entries in
            match h with
               Entry info ->
                  insert_item (tables, info :: entries) t
             | Table table ->
                  if List.memq table tables then
                     insert_item tables_entries t
                  else
                     insert_item (insert_item (table :: tables, entries) table) t
   in
      snd (insert_item ([], []) items)

(*
 * Collect subterms entries into three kinds:
 *   complete: there should be no more data to collect
 *   now: current term should be finished
 *   skip: skip this particular subterm
 *   select: need a particular subterm
 *)
let collect_stacks stacks =
   let rec collect complete now skip select = function
      [] ->
         complete, now, skip, select
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
let rec compile_select (shape, selections) =
   let expand_term (term, stack, info) =
      (subterms_of_term term) @ (end_marker :: stack), info
   in
   let stacks = List.map expand_term selections in
      DtreeMatch (shape, DtreeFlatten (compile_stacks stacks))

and compile_stacks stacks =
   match collect_stacks stacks with
      complete, [], [], [] ->
         DtreeAccept complete
    | [], now, [], [] ->
         DtreeTerm (compile_stacks now)
    | [], [], skip, [] ->
         DtreePop (compile_stacks skip)
    | complete, now, skip, select ->
         DtreeChoice ((DtreeAccept complete
                       :: (DtreeTerm (compile_stacks now))
                       :: (List.map compile_select select))
                      @ [DtreePop (compile_stacks skip)])

(*
 * The raw interface assumes the term has already been falttened.
 *)
let compile_prog terms =
   let mk_stack info =
      (subterms_of_term info.info_term) @ [end_marker], info
   in
   let stacks = List.map mk_stack terms in
      compile_stacks stacks

(*
 * Compile the table from the list of entries.
 * First collect all the entries into those that have the
 * same term template, then join all the common ones into
 * programs.
 *)
let compute_dtree table =
   (* Create the base of common entries *)
   let base = Hashtbl.create 97 in

   (* Insert a new entry into the table *)
   let insert_entry info =
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
   let _ = List.iter insert_entry (collect_entries table) in
   
   (* Compile the hastable into a collection of programs *)
   let base' = Hashtbl.create 97 in
   let compile_entries template entries =
      Hashtbl.add base' template (compile_prog (List.rev !entries))
   in
      Hashtbl.iter compile_entries base;
      base'

(*
 * Lookup the base.
 *)
let get_base = function
   { tbl_base = Some base } ->
      base
 | { tbl_base = None } as tbl ->
      let base = compute_dtree tbl in
         tbl.tbl_base <- Some base;
         base

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
            if shape = shape' then
               execute prog stack
            else
               raise Not_found
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
let lookup table t =
   let base = get_base table in
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
               let stack, items = apply_redex' redex [||] [t] in
                  stack, items, v
            with
               _ ->
                  search tl
         end
    | [] ->
         raise Not_found
   in
      search items

(*
 * $Log$
 * Revision 1.9  1998/05/04 13:01:21  jyh
 * Ocaml display without let rec.
 *
 * Revision 1.8  1998/05/01 14:59:42  jyh
 * Updating display forms.
 *
 * Revision 1.6  1998/04/29 20:53:38  jyh
 * Initial working display forms.
 *
 * Revision 1.5  1998/04/29 14:48:30  jyh
 * Added ocaml_sos.
 *
 * Revision 1.4  1998/04/28 21:38:11  jyh
 * Adjusted uppercasing.
 *
 * Revision 1.3  1998/04/28 18:30:49  jyh
 * ls() works, adding display.
 *
 * Revision 1.2  1998/04/24 02:43:03  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/04/28 15:51:46  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.3  1996/11/13 22:59:01  jyh
 * Initial version of forward/backward chaining cache.
 *
 * Revision 1.2  1996/05/21 02:14:25  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/04/07 18:27:09  jyh
 * Intermediate checking while updating dform commands.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
