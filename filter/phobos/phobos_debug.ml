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
      print_string s

let debug_int i =
   if !debug_phobos then
      print_int i

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
         print_string s;
         List.iter (fun (symbol, matched_string, pos) ->
            print_string symbol;
            print_string "=[";
            print_string matched_string;
            print_string "]{";
            print_pos pos;
            print_string "}  ") tokens;
         print_string "\n"
      end

let debug_token_options options =
   debug_list (fun opt ->
      match opt with
         Token_remove rms ->
            print_string "-remove ";
            debug_list (fun (s, _) -> debug_string s) ", " rms
       | Token_extend opt ->
            (match opt with
               Some ids ->
                  print_string "-extend ";
                  debug_list (fun (s, _) -> debug_string s) ", " ids
             | None ->
                  print_string "-extend% ")
       | Token_override ids ->
            print_string "-override ";
            debug_list (fun (s, _) -> debug_string s) ", " ids) " " options

let debug_token_rules s token_rules =
   if !debug_phobos then
      begin
         print_string s;
         List.iter (fun (_, ((id, _), token_options), _, rewrites) ->
            print_string id;
            print_string " ";
            debug_token_options token_options;
            print_string " ==>";
            print_pre_rewrites rewrites;
            print_string "\n") token_rules;
         print_string "\n"
      end

let debug_symbols s nonterminals terminals =
   if !debug_phobos then
      begin
         print_string s;
         print_string "\nTerminal symbols:\n";
         print_string_set terminals;
         print_string "\n\nNonterminal symbols:\n";
         print_string_set nonterminals;
         print_string "\n";
      end

let debug_regexps regexps =
   if !debug_phobos then
      begin
         print_string "Recognizing the following tokens:\n";
         List.iter (fun (ignore, s, regexps) ->
            if ignore then
               print_string "{ignored} ";
            print_string s;
            print_string "=";
            List.iter (fun regexp ->
               print_string " | ";
               print_string regexp) regexps;
            print_string "\n") regexps;
         print_string "\n"
      end

let debug_pre_grammar pre_grammar =
   if !debug_phobos then
	begin
         print_string "Rewrite rules:\n";
         List.iter (fun ((sym, pos), (_, _, rewrites)) ->
         print_string sym;
         print_string " ::= ";
         print_pre_rewrites rewrites;
         print_string "\n") pre_grammar
	end

let debug_grammar s grammar =
   if !debug_phobos then
      begin
         print_string s;
         print_grammar grammar
      end

let debug_parser_sets s nullables first_set follow_set =
   if !debug_phobos then
      begin
         print_string s;
         print_string "\nNullables:";
         print_psymbol_set nullables;
         print_string "\n\nFirst_set:\n";
         print_first_set first_set;
         print_string "\nFollow_set:\n";
         print_follow_set follow_set;
      end

let debug_symbol s symbol =
   if !debug_phobos then
      begin
         print_string s;
         print_psymbol symbol;
         print_string "\n\n"
      end

let debug_states s penv states accepts =
   if !debug_phobos then
      begin
         print_string s;
         print_string "States Length:";
         print_int (List.length states);
         print_string "\n\nStates:\n";
         print_states penv states;
         print_string "\n\nAccepts Length:";
         print_int (Accepts.cardinal accepts);
         print_string "\n"
      end

let debug_ptable s penv ptable ptable_errors =
   if !debug_phobos then
      begin
         print_string s;
         print_string "\nErrors Length:";
         print_int (List.length ptable_errors);
         print_string "\nError locations:\n";
         print_ploc_list penv ptable ptable_errors;
         print_string "\nAll entries:\n";
         ParserFA.fold (fun () key _ ->
            print_ploc penv ptable key) () ptable;
      end

let debug_ploc_list penv ptable ptable_errors =
   if !debug_phobos then
      print_ploc_list penv ptable ptable_errors

