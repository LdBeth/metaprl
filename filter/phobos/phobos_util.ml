(*
 * Basic utilities.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)

open Phobos_type
open Phobos_constants
open Phobos_exn
open Phobos_parse_state

(*
 * Source position.
 *)
let bogus_pos = "bogus", 0, 0, 0, 0

let source_pos = ref ("null", 1, 0, 1, 0)

let source_position () =
   !source_pos

let set_source_position pos =
   source_pos := pos

(*
 * Utilities for symbols.
 *)
let print_psymbol = function
   Terminal s ->
      Format.print_string s
 | NonTerminal s ->
      Format.print_string (string_add ["<"; s; ">"])
 | Empty ->
      Format.print_string "{Epsilon}"
 | Eof ->
      Format.print_string "$"

let print_psymbol_set set =
   let psymbol_list = PSymbolSet.to_list set in
   List.iter (fun psym ->
      Format.print_string " ";
      print_psymbol psym) psymbol_list

let psymbol_equal ps1 ps2 =
   if psymbol_compare ps1 ps2 = 0 then
      true
   else
      false

let psymbol_set_empty = PSymbolSet.empty
let psymbol_set_mem = PSymbolSet.mem
let psymbol_set_add = PSymbolSet.add
let psymbol_set_of_list = PSymbolSet.of_list
let psymbol_set_to_list = PSymbolSet.to_list
let psymbol_set_union = PSymbolSet.union
let psymbol_set_equal = PSymbolSet.equal
let psymbol_set_diff = PSymbolSet.diff
let psymbol_set_size = PSymbolSet.cardinal
let psymbol_set_subset = PSymbolSet.subset

let psymbol_list_equal lst1 lst2 =
   PSymbolSet.equal (PSymbolSet.of_list lst1) (PSymbolSet.of_list lst2)

let psymbol_list_union lst1 lst2 =
   let set1 = psymbol_set_of_list lst1 in
   let set2 = psymbol_set_of_list lst2 in
   let sets = psymbol_set_union set1 set2 in
      psymbol_set_to_list sets

(*
 * String set utilities.
 *)
let string_set_mem = StringSet.mem

let string_set_union = StringSet.union

let string_set_of_list l =
   let s = StringSet.empty in
   List.fold_left (fun set s ->
      StringSet.add set s) s l

(*
 * Grammar utilities.
 *)
let find_productions mtable key =
   try
      PSymbolMTable.find_all mtable key
   with
      Not_found ->
         []

(*
 * Dealing with the nullables set.
 *)
let nullables_add = PSymbolSet.add

let is_nullable nullables = function
   Empty ->
      true
 | key ->
      PSymbolSet.mem nullables key

let rec is_nullable_list nullables = function
   head :: rest ->
      if is_nullable nullables head then
         is_nullable_list nullables rest
      else
         false
 | [] ->
      true

(*
 * Dealing with FIRST sets.
 *)
let first_set_find_as_set (first_set: PSymbolSet.t PSymbolTable.t) key =
   try
      PSymbolTable.find first_set key
   with
      Not_found ->
         PSymbolSet.empty

let first_set_find first_set key =
   try
      psymbol_set_to_list (PSymbolTable.find first_set key)
   with
      Not_found ->
         []

let first_set_add first_set key data =
   let (old_set: PSymbolSet.t) = first_set_find_as_set first_set key in
   let new_set = psymbol_set_add old_set data in
      PSymbolTable.add first_set key new_set

let first_set_add_list first_set key lst =
   let old_set = first_set_find_as_set first_set key in
   let to_be_added = psymbol_set_of_list lst in
   let new_set = psymbol_set_union old_set to_be_added in
      PSymbolTable.add first_set key new_set

(*
 * Dealing with FOLLOW sets.
 *)
let follow_set_find_as_set (follow_set: PSymbolSet.t PSymbolTable.t) key =
   try
      PSymbolTable.find follow_set key
   with
      Not_found ->
         PSymbolSet.empty

let follow_set_find follow_set key =
   try
      psymbol_set_to_list (PSymbolTable.find follow_set key)
   with
      Not_found ->
         []

let follow_set_add follow_set key data =
   let (old_set: PSymbolSet.t) = follow_set_find_as_set follow_set key in
   let new_set = psymbol_set_add old_set data in
      PSymbolTable.add follow_set key new_set

let follow_set_add_list follow_set key lst =
   let old_set = follow_set_find_as_set follow_set key in
   let to_be_added = psymbol_set_of_list lst in
   let new_set = psymbol_set_union old_set to_be_added in
      PSymbolTable.add follow_set key new_set

let grammar_table_add = PSymbolMTable.add
let grammar_table_find = PSymbolMTable.find_all

let grammar_table_add_once table key data =
   try
      let data_list = grammar_table_find table key in
      if List.mem data data_list then
         table
      else
         grammar_table_add table key data
   with
      Not_found ->
         grammar_table_add table key data



(*
 * Items, productions.
 *)
let prod_id_empty = ProductionIdTable.empty
(*let prod_id_find_unsafe = ProductionIdTable.find*)
let prod_id_find_unsafe pridenv prod_id =
   try
      ProductionIdTable.find pridenv prod_id
   with
      Not_found ->
         raise (Invalid_argument (Printf.sprintf "production id %d not found" prod_id))

let prod_id_add = ProductionIdTable.add

let prod_id_replace table key data =
   let table = ProductionIdTable.remove table key in
      prod_id_add table key data

let print_psym = function
   NonTerminal s ->
      Format.print_string "<";
      Format.print_string s;
      Format.print_string ">"
 | Terminal s ->
      Format.print_string s
 | Eof ->
      Format.print_string "EOF"
 | Empty ->
      Format.print_string "{epsilon}"

let print_psym_list lst =
   List.iter (fun psym ->
      Format.print_string " ";
      print_psym psym) lst

let prod_empty = ProductionTable.empty
let prod_find_unsafe = ProductionTable.find
let prod_find prenv (head, prods) =
   try
      ProductionTable.find prenv (head, prods)
   with
      Not_found ->
         Format.print_string "PROD not found :";
         print_psym head;
         Format.print_string " ->";
         print_psym_list prods;
         exit 1

let prod_add = ProductionTable.add

(*
 * For each production, we have a list of rewrite rules.
 * These routines will also be used to keep the compiled
 * rewrite rules.
 *)
let rewrite_empty = ProductionIdMTable.empty
let rewrite_find_unsafe rewrites key =
   try
      ProductionIdMTable.find_all rewrites key
   with
      Not_found ->
         raise (RewriteException (source_position (), "No rewrite rule to apply"))

let rewrite_add = ProductionIdMTable.add
let rewrite_add_list table key lst =
   List.fold_left (fun table entry ->
      rewrite_add table key entry) table lst

let rewrite_replace_list table key lst =
   let table = ProductionIdMTable.remove table key in
      rewrite_add_list table key lst

(*
 * For each token, we have a list of rewrite rules.
 * These routines will also be used to keep the compiled
 * rewrite rules.
 *)
let lex_rewrite_empty = PSymbolMTable.empty

let lex_rewrite_find_unsafe rewrites key =
   try
      PSymbolMTable.find_all rewrites key
   with
      Not_found ->
         raise (RewriteException (source_position (), "No rewrite rule to apply"))

let lex_rewrite_add = PSymbolMTable.add

let lex_rewrite_add_list table key lst =
   List.fold_left (fun table entry ->
      lex_rewrite_add table key entry) table lst

(*
 * Don't add more than one set of rewrites for the same symbol.
 * Invariant: we add the newer set of rewrites first.
 *)
let lex_rewrite_add_or_replace_list table key lst =
   try
      let rewrites = PSymbolMTable.find_all table key in
         table
   with
      Not_found ->
         lex_rewrite_add_list table key lst

(*
 * State manipulation.
 *)
let state_empty = { state_map = Parser_state.empty; state_cache = IntSet.empty }
let state_find_unsafe state item = Parser_state.find state.state_map item

let state_find_safe state item =
   try
      state_find_unsafe state item
   with
      Not_found ->
         psymbol_set_empty

let state_add state item psym =
   try
      let psym_set = (*state_find_unsafe*)Parser_state.find state.state_map item in
      if psymbol_set_mem psym_set psym then
         state, false
      else
         let state_map' = Parser_state.add state.state_map item (psymbol_set_add psym_set psym) in
         { state_map = state_map'; state_cache = state.state_cache }, true
   with
      Not_found ->
         let state_map' = Parser_state.add state.state_map item (psymbol_set_of_list [psym]) in
         let state_cache' = IntSet.add state.state_cache (comp_item_of_item item) in
         { state_map = state_map'; state_cache = state_cache' }, true

(*
 * Returns a new state with the given lookahead symbol set
 * added to item. If this resulted in no change, (false, {})
 * is returned as the second component of the resulting tuple,
 * otherwise it is (true, {new_symbols}).
 *)
let state_add_set state item set =
   let res =
   try
      let psym_set = (*state_find_unsafe*)Parser_state.find state.state_map item in
      let psym_diff = psymbol_set_diff set psym_set in
         if not (psymbol_set_subset set psym_set) then
            let state_map' = Parser_state.add state.state_map item (psymbol_set_union psym_set set) in
            { state_map = state_map'; state_cache = state.state_cache }, (true, psym_diff)
         else
            state, (false, psymbol_set_empty)
   with
      Not_found ->
         let state_map' = Parser_state.add state.state_map item set in
         let state_cache' = IntSet.add state.state_cache (comp_item_of_item item) in
         { state_map = state_map'; state_cache = state_cache' }, (true, set)
   in
      res

let state_union (st1: state_struct) (st2: state_struct) =
   let len1 = Parser_state.cardinal st1.state_map in
   let len2 = Parser_state.cardinal st2.state_map in
      Parser_state.fold (fun (st1, changed) item looks2 ->
         let looks1 = state_find_safe st1 item in
         let changed' = not (psymbol_set_subset looks2 looks1) in
         let st1, _ = state_add_set st1 item (psymbol_set_union looks1 looks2) in
            st1, changed || changed') (st1, false) st2.state_map

(*
 * Two states are equal if they have the same
 * items, but the items' associated symbols are ignored.
 * (Because we are aiming to produce an LALR table.)
 *)
let state_equal st1 st2 =
(*   let len1 = Parser_state.cardinal st1.state_map in
   let len2 = Parser_state.cardinal st2.state_map in
   let res =
   if len1 <> len2 then
      false
   else
   try
      Parser_state.iter (fun item _ ->
         if Parser_state.mem st2 item then
            ()
         else
            raise Not_found) st1;
         true
   with
      Not_found ->
         false
   in*)
   let res = IntSet.equal st1.state_cache st2.state_cache in
      res

(*
 * Manipulating a list of states.
 *)
let states_empty = { states_list = []; states_cache = IntSetMap.empty }
(*let states_with_id_empty = { states_and_id_list = []; states_and_id_cache = IntSetMap.empty }*)
let states_nth lst n = List.nth lst (n-start_state_id)

let rec states_contain_aux states state count =
(*   match states with
      head :: rest ->
         if state_equal head state then
            true, count
         else
            states_contain_aux rest state (count+1)
    | [] ->
         false, -1
*)
   let target = state.state_cache in
   try
      true, IntSetMap.find states.states_cache target
   with
      Not_found -> false, -1

let states_contain states state =
   fst (states_contain_aux states state start_state_id)

let states_id_of states state =
   snd (states_contain_aux states state start_state_id)

let states_contain_and_id states state =
   states_contain_aux states state start_state_id

(*
 * Replace the "id"th state in "states" with
 * its and another state's union.
 *)
(*let state_cache = ref Parser_state.empty*)

exception IntSetRetrieve of IntSetBase.t

let int_set_retrieve_key map res =
   try
      IntSetMap.fold (fun () key i ->
         if i = res then
            raise (IntSetRetrieve key)) () map;
      raise (Invalid_argument "int_set_retrieve_key: Key not found")
   with
      IntSetRetrieve key -> key

let states_union_symbols states id state' =
   if id < 1 then
      raise (Invalid_argument "states_union_symbols");
   let prev = list_first_n states.states_list (id - 1) in
   let tail = list_from_nth states.states_list (id + 1) in
   let old_state = list_nth states.states_list id in
   let old_state_cache = int_set_retrieve_key states.states_cache id in
   let updated_state, changed = state_union old_state state' in
   let old_states_cache = IntSetMap.remove states.states_cache old_state_cache in
   let states_list' = prev @ [updated_state] @ tail in
   let states_cache' = IntSetMap.add old_states_cache updated_state.state_cache id in
   let states' = { states_list = states_list'; states_cache = states_cache' } in
      states', updated_state, changed

let obtain_id states state =
   let contains, id = states_contain_and_id states state in
   if contains then
      begin
         let states, updated_state, changed = states_union_symbols states id state in
            states, id, false, changed, updated_state
      end
   else
      let len = List.length states.states_list + 1 in
      let states_cache' = IntSetMap.add states.states_cache state.state_cache len in
      let states_list' = states.states_list @ [state] in
      let states = { states_list = states_list'; states_cache = states_cache' } in
         states, len, true, true, state

(*
 * Parsing actions.
 *)
let actions_empty = Action_edges.empty
let actions_union = Action_edges.union
let actions_add = Action_edges.add
let actions_equal = Action_edges.equal

(*
 * Accepting states.
 *)
let accepts_empty = Accepts.empty
let accepts_add = Accepts.add
let accepts_equal = Accepts.equal

(*
 * Parsing table utilities.
 *)
let parsing_table_empty = ParserFA.empty
let parsing_table_find_unsafe = ParserFA.find_all

let parsing_table_find ptable key =
   try
      ParserFA.find_all ptable key
   with
      Not_found ->
         []

let parsing_table_add ptable ptable_errors key data =
   try
      let _ = parsing_table_find_unsafe ptable key in
         ParserFA.add ptable key data, ptable_errors @ [key]
   with
        Not_found ->
         ParserFA.add ptable key data, ptable_errors

let parsing_table_remove = ParserFA.remove
   
(*
 * Parsing errors.
 *)
let parsing_errors_remove errors (i, psym) =
   List.filter (fun (i2, psym2) ->
      if compare i i2 = 0 && psymbol_compare psym psym2 = 0 then
         false
      else
         true) errors

(*
 * File and filename routines.
 *)
let chop_extension fname =
   try
      let index = String.rindex fname '.' in
         String.sub fname 0 index
   with
      Not_found ->
         fname

let filename_of_compiled_grammar fname =
   let name = chop_extension fname in
      string_add [name; ".cph"]

let filename_of_grammar_output fname =
   let name =
      try
         let index = String.rindex fname '.' in
            String.sub fname 0 index
      with
         Not_found ->
            fname
   in
      string_add [name; ".output"]

let rec find_file paths name =
   match paths with
      head :: tail ->
         let fname = (head ^ "/" ^ name) in
         if Sys.file_exists fname then
            fname
         else
            find_file tail name
    | [] ->
         name

(*
 * Printing warning messages.
 *)
let print_warning pos s =
   Printf.printf "%s: warning - %s\n" (string_of_pos pos) s
