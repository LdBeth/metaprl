(*
 * Print the grammar summary file.
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

open Phobos_type
open Phobos_print
open Phobos_util
open Phobos_parser_internals

let save_parser_report gst penv (states: state_list_struct) ptable ptable_errors name =
   let print_opt_prec = function
      Some s ->
         Lm_format.print_string (Lm_printf.sprintf " %%prec %s" s)
    | None ->
         ()
   in
   let rec num_of_conflicts = function
        (state_id, psym) :: rest ->
         let actions = parsing_table_find ptable (state_id, psym) in
            (match actions with
               [] ->
                  raise (Invalid_argument "num_of_conflicts [empty]")
             | [a] ->
                  num_of_conflicts rest
             | a :: [b] ->
                  (match a, b with
                     Shift _, Reduce _
                   | Reduce _, Shift _ ->
                        let sr, rr = num_of_conflicts rest in
                           sr+1, rr
                   | Reduce _, Reduce _ ->
                        let sr, rr = num_of_conflicts rest in
                           sr, rr+1
                   | _ ->
                        num_of_conflicts rest)
             | _ ->
                  num_of_conflicts rest)
    | [] ->
         0, 0
   in
   let outx = open_out name in
      Lm_format.set_formatter_out_channel outx;
   let total_productions, _ =
      (* Print all productions *)
      ProductionIdTable.fold (fun (prod_num, last) prod_id (head, prods, opt_prec) ->
         if psymbol_compare last head = 0 then
            begin
               Lm_format.print_string (Lm_printf.sprintf "%4d      |" prod_num);
               print_psymbol_list prods;
               print_opt_prec opt_prec;
               Lm_format.print_string "\n"
            end else
            begin
               Lm_format.print_string (Lm_printf.sprintf "\n%4d  " prod_num);
               print_psymbol head;
               Lm_format.print_string " :";
               print_psymbol_list prods;
               print_opt_prec opt_prec;
               Lm_format.print_string "\n"
         end;
            prod_num+1, head) (1, Empty) penv.parser_prod_ids
   in
   let _ =
      ParserFA.fold_all (fun old_state_num (state_id, lookahead) actions ->
         if old_state_num <> state_id then
            begin
               let state = states_nth states.states_list state_id in
               Lm_format.print_string (Lm_printf.sprintf "\nState %d\n" state_id);
               (* Print all non-trivival rules *)
               Parser_state.fold (fun () (prod_id, i) _ ->
                  if i <> 0 then
                     begin
                        Lm_format.print_string "        ";
                        print_rule_loc penv.parser_prod_ids (prod_id, i);
                        Lm_format.print_string (Lm_printf.sprintf " (%d)\n" prod_id)
                     end) () state.state_map;
               Lm_format.print_string "\n";
            end;
         (* Print all lookaheads and the associated action *)
         let action = select_action actions in
         if List.length actions > 1 then
            Lm_format.print_string "   **   "
         else
            Lm_format.print_string "        ";
         print_psymbol lookahead;
         Lm_format.print_string "  ";
         print_pentry_simple action;
         if List.length actions > 1 then
            begin
               Lm_format.print_string "  [";
               print_pentry_simple_list actions;
               Lm_format.print_string " ]"
            end;
         Lm_format.print_string "\n";
         state_id) (-1) ptable
   in
   let sr, rr = num_of_conflicts ptable_errors in
      Lm_format.print_string (**)
         (Lm_printf.sprintf "\n%d terminals, %d nonterminals\n%d grammar rules, %d states\n%d shift-reduce, %d reduce-reduce conflicts\n%d entries in parsing table\n" (**)
            (StringSet.cardinal gst.grammar_terminals) (StringSet.cardinal gst.grammar_nonterminals) (**)
            (total_productions-1) (List.length states.states_list) sr rr (ParserFA.cardinal ptable));
      Lm_format.set_formatter_out_channel stdout;
      close_out outx

