(*
 * Various debugging primitives.
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
 *
 *)

open Lm_debug
open Lm_printf

open Phobos_type
open Phobos_print
open Phobos_parse_state

(*
 * Basic MetaPRL debugging.
 *)
let debug_phobos =
   create_debug (**)
      { debug_name = "phobos";
        debug_description = "print Phobos debugging information";
        debug_value = false
      }

(*
 * Debugging shorthands.
 *)
let debug_string s =
   if !debug_phobos then
      Lm_format.print_string s

let debug_int i =
   if !debug_phobos then
      Lm_format.print_int i

let rec debug_list f sep = function
   [el] ->
      f el
 | el :: rest ->
      f el;
      debug_string sep;
      debug_list f sep rest
 | [] ->
      ()

(*
 * Miscellaneous.
 *)
let debug_tokens s tokens =
   if !debug_phobos then
      begin
         Lm_format.print_string s;
         List.iter (fun (symbol, matched_string, pos) ->
            Lm_format.print_string symbol;
            Lm_format.print_string "=[";
            Lm_format.print_string matched_string;
            Lm_format.print_string "]{";
            print_pos pos;
            Lm_format.print_string "}  ") tokens;
         Lm_format.print_string "\n"
      end

let debug_token_options options =
   debug_list (fun opt ->
      match opt with
         Token_remove rms ->
            Lm_format.print_string "-remove ";
            debug_list (fun (s, _) -> debug_string s) ", " rms
       | Token_extend opt ->
            (match opt with
               Some ids ->
                  Lm_format.print_string "-extend ";
                  debug_list (fun (s, _) -> debug_string s) ", " ids
             | None ->
                  Lm_format.print_string "-extend% ")
       | Token_override ids ->
            Lm_format.print_string "-override ";
            debug_list (fun (s, _) -> debug_string s) ", " ids) " " options

let debug_token_rules s token_rules =
   if !debug_phobos then
      begin
         Lm_format.print_string s;
         List.iter (fun (_, ((id, _), token_options), _, rewrites) ->
            Lm_format.print_string id;
            Lm_format.print_string " ";
            debug_token_options token_options;
            Lm_format.print_string " ==>";
            print_pre_rewrites rewrites;
            Lm_format.print_string "\n") token_rules;
         Lm_format.print_string "\n"
      end

let debug_symbols s nonterminals terminals =
   if !debug_phobos then
      begin
         Lm_format.print_string s;
         Lm_format.print_string "\nTerminal symbols:\n";
         print_string_set terminals;
         Lm_format.print_string "\n\nNonterminal symbols:\n";
         print_string_set nonterminals;
         Lm_format.print_string "\n";
      end

let debug_regexps regexps =
   if !debug_phobos then
      begin
         Lm_format.print_string "Recognizing the following tokens:\n";
         List.iter (fun (ignore, s, regexps) ->
            if ignore then
               Lm_format.print_string "{ignored} ";
            Lm_format.print_string s;
            Lm_format.print_string "=";
            List.iter (fun regexp ->
               Lm_format.print_string " | ";
               Lm_format.print_string regexp) regexps;
            Lm_format.print_string "\n") regexps;
         Lm_format.print_string "\n"
      end

let debug_pre_grammar pre_grammar =
   if !debug_phobos then
	begin
         Lm_format.print_string "Rewrite rules:\n";
         List.iter (fun ((sym, pos), (_, _, rewrites)) ->
         Lm_format.print_string sym;
         Lm_format.print_string " ::= ";
         print_pre_rewrites rewrites;
         Lm_format.print_string "\n") pre_grammar
	end

let debug_grammar s grammar =
   if !debug_phobos then
      begin
         Lm_format.print_string s;
         print_grammar grammar
      end

let debug_parser_sets s nullables first_set follow_set =
   if !debug_phobos then
      begin
         Lm_format.print_string s;
         Lm_format.print_string "\nNullables:";
         print_psymbol_set nullables;
         Lm_format.print_string "\n\nFirst_set:\n";
         print_first_set first_set;
         Lm_format.print_string "\nFollow_set:\n";
         print_follow_set follow_set;
      end

let debug_symbol s symbol =
   if !debug_phobos then
      begin
         Lm_format.print_string s;
         print_psymbol symbol;
         Lm_format.print_string "\n\n"
      end

let debug_states s penv states accepts =
   if !debug_phobos then
      begin
         Lm_format.print_string s;
         Lm_format.print_string "States Length:";
         Lm_format.print_int (List.length states);
         Lm_format.print_string "\n\nStates:\n";
         print_states penv states;
         Lm_format.print_string "\n\nAccepts Length:";
         Lm_format.print_int (Accepts.cardinal accepts);
         Lm_format.print_string "\n"
      end

let debug_ptable s penv ptable ptable_errors =
   if !debug_phobos then
      begin
         Lm_format.print_string s;
         Lm_format.print_string "\nErrors Length:";
         Lm_format.print_int (List.length ptable_errors);
         Lm_format.print_string "\nError locations:\n";
         print_ploc_list penv ptable ptable_errors;
         Lm_format.print_string "\nAll entries:\n";
         ParserFA.fold (fun () key _ ->
            print_ploc penv ptable key) () ptable;
      end

let debug_ploc_list penv ptable ptable_errors =
   if !debug_phobos then
      print_ploc_list penv ptable ptable_errors

