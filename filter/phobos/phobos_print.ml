(*
 * Printing utilities.
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
open Phobos_util
open Simple_print.SimplePrint

(****************************************************
 * Printing functions.
 ****************************************************)
let print_string_list lst =
   List.iter (fun s ->
      Lm_format.print_string " ";
      Lm_format.print_string s) lst

let print_string_set sset =
   StringSet.iter (fun s ->
      Lm_format.print_string " ";
      Lm_format.print_string s) sset;
   Lm_format.print_string "\n"


(*
 * Print psymbol's and its relatives.
 *)
let string_of_psymbol = function
   Terminal s
 | NonTerminal s ->
      s
 | Empty ->
      "{Epsilon}"
 | Eof ->
      "$"

let print_psymbol = function
   Terminal s ->
      Lm_format.print_string s
 | NonTerminal s ->
      Lm_format.print_string (string_add ["<"; s; ">"])
 | Empty ->
      Lm_format.print_string "{Epsilon}"
 | Eof ->
      Lm_format.print_string "$"

let print_psymbol_set set =
   let psymbol_list = PSymbolSet.to_list set in
   List.iter (fun psym ->
      Lm_format.print_string " ";
      print_psymbol psym) psymbol_list

let print_psymbol_list lst =
   List.iter (fun psym ->
      Lm_format.print_string " ";
      print_psymbol psym) lst

let print_psymbol_set_of_sets set =
   PSymbolTable.iter (fun psym psym_set ->
      print_psymbol psym;
      Lm_format.print_string ":";
      print_psymbol_set psym_set;
      Lm_format.print_string "\n") set

let print_first_set first_set =
   print_psymbol_set_of_sets first_set

let print_follow_set follow_set =
   print_psymbol_set_of_sets follow_set

(*
 * Print grammar.
 *)
let rec print_grammar_productions = function
   (NonTerminal s, pos) :: rest ->
      Lm_format.print_string " <";
      Lm_format.print_string s;
      Lm_format.print_string ">";
      print_grammar_productions rest
 | (Terminal s, pos) :: rest ->
      Lm_format.print_string " ";
      Lm_format.print_string s;
      print_grammar_productions rest
 | (Empty, _) :: rest ->
      Lm_format.print_string " {Epsilon}";
      print_grammar_productions rest
 | (Eof, _) :: rest ->
      Lm_format.print_string " $";
      print_grammar_productions rest
 | [] ->
      ()

and print_opt_prec = function
   Some (s, _) ->
      Lm_format.print_string (Lm_printf.sprintf "%%prec %s" s)
 | None ->
      ()

and print_grammar = function
   (ps1, pos, prods, opt_prec, _) :: rest ->
      Lm_format.print_string "<";
      Lm_format.print_string (string_of_psymbol ps1);
      Lm_format.print_string "> ->";
      print_grammar_productions prods;
      print_opt_prec opt_prec;
      Lm_format.print_string "\n";
      print_grammar rest
 | [] ->
      ()

(*
 * Print rule_loc's.
 *)
let print_rule_loc prod_ids (prod_id, pos) =
   let head, prods, opt_pred =
      try
         prod_id_find_unsafe prod_ids prod_id
      with
         _ ->
            raise (Invalid_argument "print_rule_loc: production id not found")
   in
      print_psymbol head;
      Lm_format.print_string " -> ";
      for i = 0 to pos-1 do
         let sym = List.nth prods i in
         Lm_format.print_string " ";
         print_psymbol sym
      done;
      Lm_format.print_string ".";
      for i = pos to List.length prods - 1 do
         let sym = List.nth prods i in
         Lm_format.print_string " ";
         print_psymbol sym
      done;
      match opt_pred with
           Some s ->
            Lm_format.print_string " %prec ";
            Lm_format.print_string s
         | None ->
            ()

let print_rule_loc_list prod_ids lst =
   List.iter (fun rule_loc ->
      print_rule_loc prod_ids rule_loc;
      Lm_format.print_string "\n") lst

(*
 * Print an item.
 *)
let print_item prod_ids rule_loc looks =
   Lm_format.print_string "(";
   print_rule_loc prod_ids rule_loc;
   Lm_format.print_string ", [";
   print_psymbol_set looks;
   Lm_format.print_string "])"

let print_productions head lst =
   List.iter (fun prods ->
      print_psymbol head;
      Lm_format.print_string " -> ";
      print_psymbol_list prods;
      Lm_format.print_string "\n") lst

(*
 * Print a state.
 *)
let print_state penv state =
   Parser_state.iter (fun item looks ->
      print_item penv.parser_prod_ids item looks;
      Lm_format.print_string "\n") state

let print_states penv states =
   let _ =
      List.fold_left (fun num { state_map = state; state_cache = _ } ->
         Lm_format.print_string "State ";
         Lm_format.print_int num;
         Lm_format.print_string "\n";
         print_state penv state;
         Lm_format.print_string "\n";
            num+1) 1 states
   in
      ()

(*
 * Print parsing table entries.
 *)
let print_pentry penv = function
   Shift i ->
      Lm_format.print_string "s";
      Lm_format.print_int i
 | Goto i ->
      Lm_format.print_string "g";
      Lm_format.print_int i
 | Reduce prod_id ->
      let psym, prods, _ = prod_id_find_unsafe penv.parser_prod_ids prod_id in
         Lm_format.print_string "r[";
         print_psymbol psym;
         Lm_format.print_string " -> ";
         print_psymbol_list prods;
         Lm_format.print_string "]"
 | Accept ->
      Lm_format.print_string "accept"
 | Error ->
      Lm_format.print_string "error"

let print_pentry_simple = function
   Shift i ->
      Lm_format.print_string "shift ";
      Lm_format.print_int i
 | Goto i ->
      Lm_format.print_string "goto ";
      Lm_format.print_int i
 | Reduce prod_id ->
         Lm_format.print_string "reduce ";
         Lm_format.print_int prod_id
 | Accept ->
      Lm_format.print_string "%accept%"
 | Error ->
      Lm_format.print_string "error"

let print_pentry_list penv lst =
   List.iter (fun pentry ->
      Lm_format.print_string " ";
      print_pentry penv pentry) lst

let print_pentry_simple_list lst =
   List.iter (fun pentry ->
      Lm_format.print_string " ";
      print_pentry_simple pentry) lst

(*
 * Print an action line from the parsing table.
 *)
let print_ploc penv ptable (i, psym) =
   Lm_format.print_string "(";
   Lm_format.print_int i;
   Lm_format.print_string ", ";
   print_psymbol psym;
   Lm_format.print_string ") -> ";
   print_pentry_list penv (parsing_table_find ptable (i, psym));
   Lm_format.print_string "\n"

let print_ploc_list penv ptable lst =
   List.iter (fun ploc ->
      Lm_format.print_string " ";
      print_ploc penv ptable ploc) lst

(*
 * Print terms.
 *)

(* pre_terms *)
let print_pre_term (term, _) =
   Lm_format.print_string (string_of_term term)

let print_pre_terms lst =
   List.iter (fun term ->
      Lm_format.print_string " ";
      print_pre_term term) lst

let print_pre_term_rewrite term1 term2 =
   print_pre_term term1;
   Lm_format.print_string " --> ";
   print_pre_term term2

let print_pre_rewrite terms term =
   print_pre_terms terms;
   Lm_format.print_string " --> ";
   print_pre_term term

let print_pre_rewrites lst =
   List.iter (fun (terms, term) ->
      Lm_format.print_string "    | ";
      print_pre_rewrite terms term;
      Lm_format.print_string "\n") lst

let print_pre_term_rewrites lst =
   List.iter (fun (term1, term2) ->
      Lm_format.print_string "    | ";
      print_pre_term_rewrite term1 term2;
      Lm_format.print_string "\n") lst

let print_rewrite_table table =
   PSymbolMTable.iter_all (fun sym rewrites ->
      print_psymbol sym;
      Lm_format.print_string ":\n";
      print_pre_rewrites rewrites;
      Lm_format.print_string "\n") table

(* terms *)
let print_term term =
   Lm_format.print_string (string_of_term term)

let print_terms lst =
   List.iter (fun term ->
      Lm_format.print_string " ";
      print_term term) lst

let print_rewrite terms term =
   print_terms terms;
   Lm_format.print_string " --> ";
   print_term term

let print_rewrites lst =
   List.iter (fun (terms, term) ->
      Lm_format.print_string "    | ";
      print_rewrite terms term;
      Lm_format.print_string "\n") lst

(*
 * Print the parsing stack.
 *)
let print_stack stack =
   Lm_format.print_string "[";
   Stack.iter (fun el ->
      match el with
         Sta_state i ->
            Lm_format.print_string " {";
            Lm_format.print_int i;
            Lm_format.print_string "}"
       | Sta_term term ->
            Lm_format.print_string " ";
            print_term term) stack;
   Lm_format.print_string " ]"

