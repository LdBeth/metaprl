(*
 * Generic parser.
 *
 * We have utilities for
 *     - generic parser operations (closure, goto)
 *     - computing parser states
 *     - computing parser actions
 *     - parsing
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2001 Adam Granicz, Caltech
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

open Phobos_debug
open Phobos_type
open Phobos_parse_state
open Phobos_parser_internals
open Phobos_constants
open Phobos_exn
open Phobos_util
open Phobos_print
open Phobos_report
open Phobos_debug
open Phobos_rewrite
open Phobos_grammar

(*****************************************************
 * Generic parser routines.
 *****************************************************)
(*
 * Serialize productions.
 *)
let serialize_productions gst =
(***********************************************)
(*
 * TEMP: Compile local term rewrites.
 *)
   if !debug_phobos then
      begin
         print_string "Compiling local rewrites....\n";
         flush stdout
      end;
   let new_term_rewrites =
      List.map (fun (term1, term2) ->
         compile_pattern [term1] term2) gst.grammar_local_rewrites
   in
   if !debug_phobos then
      begin
         print_string "Finished....\n";
         flush stdout
      end;
(***********************************************)
(* end of TEMP *)
   let prenv = prod_empty in
   let pridenv = prod_id_empty in
   let rewrite_table = rewrite_empty in
   let prenv, pridenv, rewrite_table, total =
      List.fold_left (fun (prenv, pridenv, rewrite_table, total) (psym, pos, prods, prec_opt, rewrites) ->
         try
            let prods = List.map fst prods in
            let index = prod_find_unsafe prenv (psym, prods) in
            if !debug_phobos then
               begin
                  print_string "warning: ";
                  print_string (string_of_pos pos);
                  print_string ": production redeclared: \"";
                  print_psymbol psym;
                  print_string " ::= ";
                  print_psymbol_list prods;
                  print_string "\"\n";
                  flush stdout;
               end;
            (* Update production with optional precision *)
            let pridenv = prod_id_replace pridenv index (psym, prods, first_of_option prec_opt) in
            (* Replace old rewrites with new ones associated with the production *)
            let rewrite_table = rewrite_replace_list rewrite_table index rewrites in
               prenv, pridenv, rewrite_table, index
(*               raise (PhobosException (pos, "production redefined"))*)
         with
            Not_found ->
               let prods = List.map fst prods in
               let new_index = total + 1 in
               let prenv = prod_add prenv (psym, prods) new_index in
               let pridenv = prod_id_add pridenv new_index (psym, prods, first_of_option prec_opt) in
               let rewrite_table = rewrite_add_list rewrite_table new_index rewrites in
                  prenv, pridenv, rewrite_table, new_index) (prenv, pridenv, rewrite_table, 0) gst.grammar_grammar
   in
      prenv, pridenv, rewrite_table

(*
 * Calculate the FIRST and FOLLOW sets based
 * on a given grammar.
 * The FIRST set is a set of nonterminals that
 * can start any string derived from a symbol.
 * The FOLLOW set is a set of nonterminals that
 * can follow a given symbol.
 *)

let rec calculate_first_and_follow_sets grammar grammar_table first_set follow_set nullables =
   let changed, first_set', follow_set', nullables' =
      List.fold_left (fun (changed, first, follow, null) (psym, pos, prod, _, _) ->
         let prod = List.map fst prod in
         let changed = ref changed in
         let first_set' = ref first in
         let follow_set' = ref follow in
         let nullables' = ref null in
         let prod_a = Array.of_list prod in
         let k = Array.length prod_a - 1 in
            if is_nullable_list !nullables' prod then
               if not (is_nullable !nullables' psym) then
                  begin
                     changed := true;
                     nullables' := nullables_add !nullables' psym
                  end;
         for i = 0 to k do
            let y_i = prod_a.(i) in
            let first_x = first_set_find !first_set' psym in
            let first_y_i = first_set_find !first_set' y_i in
            let follow_x = follow_set_find !follow_set' psym in
            let follow_y_i = follow_set_find !follow_set' y_i in
               if i=0 || is_nullable_list !nullables' (Array.to_list (array_sub prod_a 0 (i-1))) then
                  begin
                     let new_first_x = psymbol_list_union first_x first_y_i in
                        if not (psymbol_list_equal new_first_x first_x) then
                           changed := true;
                        first_set' := first_set_add_list !first_set' psym new_first_x;
               end;
         done;
         for i = 0 to k do
            let y_i = prod_a.(i) in
            let first_x = first_set_find !first_set' psym in
            let first_y_i = first_set_find !first_set' y_i in
            let follow_x = follow_set_find !follow_set' psym in
            let follow_y_i = follow_set_find !follow_set' y_i in
               if i=k || is_nullable_list !nullables' (Array.to_list (array_sub prod_a (i+1) k)) then
                  begin
                     let new_follow_y_i = psymbol_list_union follow_y_i follow_x in
                        if not (psymbol_list_equal new_follow_y_i follow_y_i) then
                           changed := true;
                        follow_set' := follow_set_add_list !follow_set' y_i new_follow_y_i;
                  end;
         done;
         for i = 0 to k do
            for j = i+1 to k do
               let y_i = prod_a.(i) in
               let first_x = first_set_find !first_set' psym in
               let first_y_i = first_set_find !first_set' y_i in
               let follow_x = follow_set_find !follow_set' psym in
               let follow_y_i = follow_set_find !follow_set' y_i in
               let y_j = prod_a.(j) in
               let first_y_j = first_set_find !first_set' y_j in
               let follow_y_j = follow_set_find !follow_set' y_j in
                  if i+1=j || is_nullable_list !nullables' (Array.to_list (array_sub prod_a (i+1) (j-1))) then
                     begin
                        let new_follow_y_i = psymbol_list_union follow_y_i first_y_j in
                           if not (psymbol_list_equal new_follow_y_i follow_y_i) then
                              changed := true;
                           follow_set' := follow_set_add_list !follow_set' y_i new_follow_y_i;
                     end
            done
         done;
            !changed, !first_set', !follow_set', !nullables') (false, first_set, follow_set, nullables) grammar
   in
   if changed then
      calculate_first_and_follow_sets grammar grammar_table first_set' follow_set' nullables'
   else
      first_set', follow_set', nullables'

let initialize_first_set terminals =
   let first_set = PSymbolTable.empty in
   let first_set =
      StringSet.fold (fun first_set str ->
         first_set_add first_set (Terminal str) (Terminal str)) first_set terminals
   in
   let first_set = first_set_add first_set Eof Eof in
   let first_set = first_set_add first_set Empty Empty in
      first_set

let calculate_first_and_follow_sets grammar grammar_table first_set =
   let follow_set = PSymbolTable.empty in
   let nullables = PSymbolSet.empty in
      calculate_first_and_follow_sets grammar grammar_table first_set follow_set nullables

let rec is_nullable_string penv = function
   head :: rest ->
      let first_of_head = first_set_find_as_set penv.parser_first_set head in
         if is_nullable penv.parser_nullables head then
            begin
               let is_nullable, first_set = is_nullable_string penv rest in
                  is_nullable, psymbol_set_union first_of_head first_set
            end
         else
            false, first_set_find_as_set penv.parser_first_set head
 | [] ->
      true, psymbol_set_empty

(*
 * Calculate the closure of a state.
 * Returns another state.
 *)
let rec closure penv all_items new_items =
   let all_items, new_items, changed =
      (* Iterate through all items *)
      Parser_state.fold (fun (all_items, new_items, changed) (prod_id, i) lookaheads ->
         let psym, prods, _ = prod_id_find_unsafe penv.parser_prod_ids prod_id in
         let len = List.length prods in
         (* there is at least one symbol after dot *)
         if i+1 <= len then
            begin
               let x = List.nth prods i in
               let beta = list_from_nth prods (i+2) in
               let w_set =
                  match is_nullable_string penv beta with
                     (* Beta was nullable *)
                     true, w_set ->
                        psymbol_set_union lookaheads w_set
                     (* Beta is not nullable, and its FIRST set is w_set *)
                   | false, w_set ->
                        w_set
               in
               let x_prods = find_productions penv.parser_grammar x in
               let all_items, new_items, changed =
                  List.fold_left (fun (all_items, new_items, changed) prods ->
                     let prod_id = prod_find penv.parser_prods (x, prods) in
                     let all_items, (changed', changed_lookaheads) =
                        state_add_set all_items (prod_id, 0) w_set
                     in
                        if changed' then
                           all_items, fst (state_add_set new_items (prod_id, 0) changed_lookaheads), true
                        else
                           all_items, new_items, changed) (all_items, new_items, changed) x_prods
               in
                  all_items, new_items, changed
            end else
               all_items, new_items, changed) (all_items, state_empty, false) new_items.state_map
   in
   if changed then
      let state, _ = state_union all_items (closure penv all_items new_items) in
         state
   else
      all_items

let closure penv state =
   let closure = closure penv state state in
      closure

(*
 * Returns another state.
 *)
let goto penv state key =
   let result = state_empty in
   let result =
      Parser_state.fold (fun result (prod_id, i) lookaheads ->
         let psym, prods, _ = prod_id_find_unsafe penv.parser_prod_ids prod_id in
         let plen = List.length prods in
         if i >= plen then
            result
         else begin
            let x = List.nth prods i in
            if psymbol_equal x key then
               begin
                  if i = plen then
                     result
                  else
                     fst (state_add_set result (prod_id, (i+1)) lookaheads)
               end else
                  result
         end) result state.state_map
   in
      closure penv result

(*****************************************************
 * Parser environments.
 *****************************************************)
let create_penv module_name gst =
   let prenv, pridenv, rewrite_table = serialize_productions gst in
   debug_grammar "\nProcessing grammar:\n" gst.grammar_grammar;
   let grammar_table = grammar_table_of_grammar gst.grammar_grammar in
   let first_set = initialize_first_set gst.grammar_terminals in
   let follow_empty = PSymbolMTable.empty in
   let first_set, follow_set, nullables =
      calculate_first_and_follow_sets gst.grammar_grammar grammar_table first_set
   in
   debug_parser_sets "Parser internals:\n" nullables first_set follow_set;
   let penv =
      { parser_module = module_name;
        parser_grammar = grammar_table;
        parser_prod_ids = pridenv;
        parser_prods = prenv;
        parser_nullables = nullables;
        parser_first_set = first_set;
        parser_follow_set = follow_set;
        parser_rewrites = rewrite_table
      }
   in
   debug_symbol "\nStart symbol:" gst.grammar_start_symbol;
      penv

let closures_empty = ParserFA.empty
let closures_find = ParserFA.find
let closures_add = ParserFA.add

(*
 * Iterate only if new states had been found.
 * Note: new_states also carries an additional integer per state.
 *)
let print_state_temp state =
   Parser_state.iter (fun (prod_id, index) set ->
      print_string (Lm_printf.sprintf "(%d, %d), " prod_id index)) state

let rec iterate penv (all_states: state_list_struct) (new_states: state_with_id_list_struct) actions accepts =
   let all_states, new_states, actions', accepts, changed =
      List.fold_left (fun (all_states, new_states, actions', accepts, changed) (state, id1) ->
         let all_states, new_states, actions', accepts, changed =
            Parser_state.fold (fun (all_states, new_states, actions', accepts, changed) (prod_id, i) lookaheads ->
               let psym, prods, _ = prod_id_find_unsafe penv.parser_prod_ids prod_id in
               if i >= List.length prods then
                  all_states, new_states, actions', accepts, changed
               else
               begin
                  let x = List.nth prods i in
                     if psymbol_equal x Eof then
                        begin
                           let accepts' = accepts_add accepts id1 in
                              all_states, new_states, actions', accepts', changed
                        end
                     else
                     begin
                        let new_state = goto penv state x in
                        let all_states, id2, changed', changed'', new_state = obtain_id all_states new_state in
                        let new_states =
                           if changed'' then
                              new_states @ [new_state, id2]
                           else
                              new_states
                        in
                        let actions'' = actions_add actions' (id1, id2, x) in
                        let changed = changed || changed' in
                           all_states, new_states, actions'', accepts, changed
                     end
               end) (all_states, new_states, actions', accepts, changed) state.state_map
         in
            all_states, new_states, actions', accepts, changed) (all_states, [], actions, accepts, false) new_states
   in
      if changed then begin
         if !debug_phobos then
            print_string "iterate: changed\n";
         iterate penv all_states new_states actions' accepts
      end
      else
         all_states, actions', accepts

let create_parser_states penv =
   let start_prod = grammar_table_find penv.parser_grammar global_start_symbol in
   (* There should be one and only one start production *)
   if List.length start_prod <> 1 then
      raise (Invalid_argument (Lm_printf.sprintf (**)
         "%d start productions found" (List.length start_prod)));
   let start_prod = List.hd start_prod in
   let start = state_empty in
   let start_prod_id = prod_find penv.parser_prods (global_start_symbol, start_prod) in
   let start_item, psym = (start_prod_id, 0), bogus_symbol in
   let start, _ = state_add start start_item psym in
   let start_state = closure penv start in
(*   let states = [] in*)
   let actions = actions_empty in
   let accepts = accepts_empty in
(*   let start_state_cache = IntSet.add IntSet.empty (comp_item_of_item start_item) in*)
   let start_states_cache = IntSetMap.add IntSetMap.empty start_state.state_cache start_state_id in
   let start_all_states = { states_list = [start_state]; states_cache = start_states_cache } in
      iterate penv start_all_states [(start_state, start_state_id)] actions accepts

let fill_parsing_table penv states ptable actions accepts =
   (* Insert shift and goto actions *)
   let ptable, ptable_errors =
      Action_edges.fold (fun (ptable, ptable_errors) (i, j, x) ->
         match x with
            NonTerminal _ ->
               parsing_table_add ptable ptable_errors (i, x) (Goto j)
          | Terminal _ ->
               parsing_table_add ptable ptable_errors (i, x) (Shift j)
          | Eof ->
               parsing_table_add ptable ptable_errors (i, x) (Shift j)
          | Empty ->
               parsing_table_add ptable ptable_errors (i, x) (Goto j)) (ptable, []) actions
   in
   (* Insert accept actions *)
   let ptable, ptable_errors =
      Accepts.fold (fun (ptable, ptable_errors) state_num ->
         parsing_table_add ptable ptable_errors (state_num, eof_symbol) Accept) (ptable, ptable_errors) accepts
   in
   (* Insert reduce actions *)
   let ptable, ptable_errors =
      List.fold_left (fun (ptable, ptable_errors) state ->
         let _, id, check, _, _ = obtain_id states state in
            if check then
               raise (Invalid_argument "fill_parsing_table:reduce")
            else
            begin
               let ptable, ptable_errors =
                  Parser_state.fold (fun (ptable, ptable_errors) (prod_id, i) lookaheads ->
                     let ptable, ptable_errors =
                        PSymbolSet.fold (fun (ptable, ptable_errors) lookahead ->
                           let _, prods, opt_prec = prod_id_find_unsafe penv.parser_prod_ids prod_id in
                              if List.length prods = i then
                                 begin
                                    let ptable, ptable_errors =
                                       parsing_table_add ptable ptable_errors (id, lookahead) (Reduce prod_id)
                                    in
                                       ptable, ptable_errors
                                 end
                                 else
                                    ptable, ptable_errors) (ptable, ptable_errors) lookaheads
                     in
                        ptable, ptable_errors) (ptable, ptable_errors) state.state_map
               in
                  ptable, ptable_errors
            end) (ptable, ptable_errors) states.states_list
   in
      states, ptable, ptable_errors

(***********************************************************
 * Disambiguating grammar by applying the associativity
 * and precedence rules specified.
 ***********************************************************)
let rec first_token_right_aux = function
   Terminal s :: rest ->
      Some s
 | head :: rest ->
      first_token_right_aux rest
 | [] ->
      None

let first_token_right penv prod_id =
   let _, prods, _ = prod_id_find_unsafe penv.parser_prod_ids prod_id in
      first_token_right_aux (List.rev prods)

let replace_entry ptable ptable_errors (i, psym) action =
   let ptable_errors = parsing_errors_remove ptable_errors (i, psym) in
   let ptable = parsing_table_remove ptable (i, psym) in
   let ptable, ptable_errors = parsing_table_add ptable ptable_errors (i, psym) action in
      ptable, ptable_errors

let apply_disambiguating_rules penv ptable ptable_errors assocs =
   let rr_probs, sr_probs, ptable, ptable_errors =
      List.fold_left (fun (rr_probs, sr_probs, ptable, ptable_errors) (i, state_sym) ->
         let errors = parsing_table_find ptable (i, state_sym) in
         let rr_prob = is_rr_problem errors in
         let sr_prob = is_sr_problem errors in
         let rr_probs, sr_probs, ptable, ptable_errors =
            (match List.length errors, rr_prob, sr_prob with
               (* Reduce/Reduce conflict detected *)
               2, true, (false, _) ->
                  rr_probs, sr_probs, ptable, ptable_errors
               (* Shift/Reduce conflict detected *)
             | 2, false, (true, (Some (prod_id, shift))) ->
                  let _, _, opt_prec = prod_id_find_unsafe penv.parser_prod_ids prod_id in
                  (*
                   * If rule has a precedence symbol, get it, otherwise its precedence is
                   * determined by its last terminal symbol.
                   *)
                  let tok1 =
                     (match opt_prec with
                        Some s ->
                           Some s
                      | None ->
                           first_token_right penv prod_id)
                  in
                  let rr_probs, sr_probs, ptable, ptable_errors =
                     (match tok1, string_of_psymbol state_sym with
                        Some s1, s2 ->
                           let pri1, _ = priority_of s1 assocs in
                           let pri2, assoc = priority_of s2 assocs in
                           let rr_probs, sr_probs, ptable, ptable_errors =
                              (match pri1 > pri2, pri1 < pri2 with
                                 true, false when pri1 > 0 && pri2 > 0 ->
                                    let ptable, ptable_errors = replace_entry ptable ptable_errors (i, state_sym) (Reduce prod_id) in
                                       rr_probs, (sr_probs+1), ptable, ptable_errors
                               | false, true when pri1 > 0 && pri2 > 0 ->
                                    let ptable, ptable_errors = replace_entry ptable ptable_errors (i, state_sym) (Shift shift) in
                                       rr_probs, (sr_probs+1), ptable, ptable_errors
                               | false, false when pri1 > 0 && pri2 > 0 ->
                                    (match assoc with
                                       Some NonAssoc ->
                                          let ptable, ptable_errors = replace_entry ptable ptable_errors (i, state_sym) Error in
                                             rr_probs, (sr_probs+1), ptable, ptable_errors
                                     | Some LeftAssoc ->
                                          let ptable, ptable_errors = replace_entry ptable ptable_errors (i, state_sym) (Reduce prod_id) in
                                             rr_probs, (sr_probs+1), ptable, ptable_errors
                                     | Some RightAssoc ->
                                          let ptable, ptable_errors = replace_entry ptable ptable_errors (i, state_sym) (Shift shift) in
                                             rr_probs, (sr_probs+1), ptable, ptable_errors
                                     | None ->
                                          rr_probs, sr_probs, ptable, ptable_errors)
                               | _ ->
                                    rr_probs, sr_probs, ptable, ptable_errors)
                           in
                                 rr_probs, sr_probs, ptable, ptable_errors
                      | _ ->
                                 rr_probs, sr_probs, ptable, ptable_errors)
                  in
                     rr_probs, sr_probs, ptable, ptable_errors
               (* Something else, we will just leave things as they are *)
             | _ ->
                  rr_probs, sr_probs, ptable, ptable_errors)
         in
            rr_probs, sr_probs, ptable, ptable_errors) (0, 0, ptable, ptable_errors) ptable_errors
   in
      rr_probs, sr_probs, ptable, ptable_errors

(***********************************************************
 * Create the parsing table.
 ***********************************************************)
let create_parsing_table name gst penv =
   let states, actions, accepts = create_parser_states penv in
   debug_states "\nStates:\n" penv states.states_list accepts;
   let ptable = parsing_table_empty in
   let states, ptable, ptable_errors = fill_parsing_table penv states ptable actions accepts in
   debug_ptable "\nParsing table:\n" penv ptable ptable_errors;
   debug_string "\nApplying disambiguating rules...\n";
   let rr_probs, sr_probs, ptable, ptable_errors =
      apply_disambiguating_rules penv ptable ptable_errors gst.grammar_assocs
   in
   debug_string "Reduce/Reduce conflicts fixed (should be zero) = ";
   debug_int rr_probs;
   debug_string "\n";
   debug_string "Shift/Reduce conflicts fixed = ";
   debug_int sr_probs;
   debug_string "\n";
   debug_string "\nAll entries:\n";
   let total =
      ParserFA.fold (fun total key _ ->
         let entries = parsing_table_find ptable key in
            total + List.length entries) 0 ptable
   in
   debug_string "\nTotal entries = ";
   debug_int total;
   debug_string "\n\nError locations:\n";
   debug_ploc_list penv ptable ptable_errors;
   debug_string "\n";
   if List.length ptable_errors > 0 then
      print_string "There were errors.\n";
   if !Phobos_state.debug_grammar then
      save_parser_report gst penv states ptable ptable_errors (**)
         (Phobos_util.filename_of_grammar_output name);
      ptable

(***********************************************************
 * Parsing.
 ***********************************************************)
(*
 * Perform indicated action.
 * Return new stack, and new position in the input string.
 *)

let perform_action clenv penv ptable rewrites stack sym matched_string input_pos pos action conv =
   match action with
      Shift i ->
         if !debug_phobos then begin
            print_string "Performing SHIFT(";
            print_int i;
            print_string ")\n"
         end;
         let term = term_of_token_string pos matched_string in

         if !debug_phobos then begin
            print_string "token on the line is =";
            print_term term;
            print_string "\n"
         end;

         let lex_pre_rewrites = lex_rewrite_find_unsafe clenv.clexer_rewrites sym in

         if !debug_phobos then begin
            print_string "Its matching rewrite rules =\n";
            print_pre_rewrites lex_pre_rewrites;
            print_string "\n"
         end;

         (* Find all lexical rewrites for sym *)
         let lrw_rules = lex_rewrite_find_unsafe rewrites.rw_lexer sym in
         (* Choose the first rule that matches and apply it *)
         let res_term = apply_first_rewrite pos lrw_rules [term] conv in
         let stack = stack_push stack (Sta_term res_term) in
         let stack = stack_push stack (Sta_state i) in
            stack, input_pos + 1
    | Goto i ->
         if !debug_phobos then begin
            print_string "Performing GOTO(";
            print_int i;
            print_string ")\n"
         end;
         (* Go to another state. *)
         (* We will push an "empty" term on the stack. *)
         let stack = stack_push stack (Sta_term empty_term) in
         let stack = stack_push stack (Sta_state i) in
            stack, input_pos
    | Reduce prod_id ->
         if !debug_phobos then begin
            print_string "Performing REDUCE(";
            print_int prod_id;
            print_string ")\n"
         end;
         let psym, psyml, _ = prod_id_find_unsafe penv.parser_prod_ids prod_id in

         if !debug_phobos then begin
            print_string "   REDUCE -> Stack =";
            print_stack stack;
            print_string "\n";
            flush stdout
         end;

         let term = term_of_token_string pos matched_string in

         if !debug_phobos then begin
            print_string "token on the line is =";
            print_term term;
            print_string "\n"
         end;

         let parser_pre_rewrites = rewrite_find_unsafe penv.parser_rewrites prod_id in

         if !debug_phobos then begin
            print_string "Production's matching rewrite rules =\n";
            print_pre_rewrites parser_pre_rewrites;
            print_string "\n"
         end;

         (* Terms on the stack to be reduced *)
         let stack, _, terms = stack_pop_list stack psyml in

         if !debug_phobos then begin
            print_string "the stack terms that this production applies to =\n";
            print_terms terms;
            print_string "\n"
         end;

         (* Fetch the rewrites that apply *)
         let rw_rules = rewrite_find_unsafe rewrites.rw_parser prod_id in
         (* Choose the first rule that matches and apply it *)
         let res_term = apply_first_rewrite pos rw_rules terms conv in

         let last_state = current_state_of_stack stack in
         (* Push result term on stack *)
         let stack = stack_push stack (Sta_term res_term) in
         let actions = parsing_table_find ptable (last_state, psym) in
         let new_state =
            match select_action actions with
               Goto i ->
                  i
             | _ ->
                  raise (Invalid_argument "perform_action: Reduce")
            in
         (* Push new state on stack *)
         let stack = stack_push stack (Sta_state new_state) in

         if !debug_phobos then begin
            print_string "   after REDUCE -> Stack =";
            print_stack stack;
            print_string "\n";
            flush stdout
         end;

         stack, input_pos
    | Accept ->
         if !debug_phobos then
            print_string "ACCEPT!\n";
         raise SourceAccepted
    | Error ->
         if !debug_phobos then
            print_string "ERROR!\n";
         raise (SyntaxError (source_position ()))

let parse_source gst (clenv: clexer_env) penv ptable source =
   let stack = ref (Stack.create ()) in
   if !debug_phobos then
      begin
         print_string "Local rewrites:\n";
         print_pre_term_rewrites gst.grammar_local_rewrites
      end;
   (* Get the conversional that applies all local rewrites *)
   let conv = iforms_conv gst.grammar_local_rewrites in
   (* We compile each terms->term pattern in penv.lexer/parser_rewrites *)
   let lexer_crewrites = compile_lexer_rewrites clenv.clexer_rewrites in
   let parser_crewrites = compile_parser_rewrites penv.parser_rewrites in
   let rewrites =
      { rw_parser = parser_crewrites;
        rw_lexer  = lexer_crewrites
      }
   in
   (* Initially, the stack contains the start state *)
      stack := stack_push !stack stack_start_state;
   if List.length source = 0 then
      raise (Invalid_argument "Empty source file");
   (* Append EOF symbol to token list *)
   let (_, _, eof_pos) = List.hd (List.rev source) in
   let source = list_add source (Eof, "", eof_pos) in
   let input_size = List.length source - 1 in
   let input_pos = ref 0 in
      try
         while !input_pos <= input_size do
            let (psym, matched_string, pos) = List.nth source !input_pos in
               set_source_position pos;

               if !debug_phobos then begin
                  print_string "Stack =";
                  print_stack !stack;
                  print_string "\n";
                  print_string "Processing symbol =";
                  print_psymbol psym;
                  print_string " @(";
                  print_int !input_pos;
                  print_string ")\n";
                  flush stdout
               end;

            let cur_state = current_state_of_stack !stack in
            let actions = parsing_table_find ptable (cur_state, psym) in
            let action =
               match List.length actions with
                  0 ->
                     (* Check to see if there is an e-transition *)
                     let actions = parsing_table_find ptable (cur_state, Empty) in
		        (match List.length actions with
			    0 ->
				(* Maybe an Eof? *)
				let actions = parsing_table_find ptable (cur_state, Eof) in
	                            select_action actions
			 | _ ->
			    select_action actions)
                | _ ->
                     select_action actions
            in
            let result = perform_action clenv penv ptable rewrites !stack psym matched_string !input_pos pos action conv in
               stack := fst result;
               input_pos := snd result
         done;
            raise (Invalid_argument "parse_source: parsed, but not accepted")
      with
         SourceAccepted ->
            (* Source accepted, return top (hopefully only!) term on stack. *)
            result_term_of_stack !stack
       | SyntaxError _ as exn ->
            raise exn

