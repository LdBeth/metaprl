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

open Phobos_type
open Phobos_print
open Phobos_parse_state

(*
 * Basic debugging.
 *)
let debug_string s =
   if !Phobos_state.debug_phobos then
      Format.print_string s

let debug_int i =
   if !Phobos_state.debug_phobos then
      Format.print_int i

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
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string s;
         List.iter (fun (symbol, matched_string, pos) ->
            Format.print_string symbol;
            Format.print_string "=[";
            Format.print_string matched_string;
            Format.print_string "]{";
            print_pos pos;
            Format.print_string "}  ") tokens;
         Format.print_string "\n"
      end

let debug_token_options options =
   debug_list (fun opt ->
      match opt with
         Token_remove rms ->
            Format.print_string "-remove ";
            debug_list (fun (s, _) -> debug_string s) ", " rms
       | Token_extend opt ->
            (match opt with
               Some ids ->
                  Format.print_string "-extend ";
                  debug_list (fun (s, _) -> debug_string s) ", " ids
             | None ->
                  Format.print_string "-extend% ")
       | Token_override ids ->
            Format.print_string "-override ";
            debug_list (fun (s, _) -> debug_string s) ", " ids) " " options

let debug_token_rules s token_rules =
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string s;
         List.iter (fun (_, ((id, _), token_options), _, rewrites) ->
            Format.print_string id;
            Format.print_string " ";
            debug_token_options token_options;
            Format.print_string " ==>";
            print_pre_rewrites rewrites;
            Format.print_string "\n") token_rules;
         Format.print_string "\n"
      end

let debug_symbols s nonterminals terminals =
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string s;
         Format.print_string "\nTerminal symbols:\n";
         print_string_set terminals;
         Format.print_string "\n\nNonterminal symbols:\n";
         print_string_set nonterminals;
         Format.print_string "\n";
      end

let debug_regexps regexps =
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string "Recognizing the following tokens:\n";
         List.iter (fun (ignore, s, regexps) ->
            if ignore then
               Format.print_string "{ignored} ";
            Format.print_string s;
            Format.print_string "=";
            List.iter (fun regexp ->
               Format.print_string " | ";
               Format.print_string regexp) regexps;
            Format.print_string "\n") regexps;
         Format.print_string "\n"
      end

let debug_pre_grammar pre_grammar =
   if !Phobos_state.debug_phobos then
	begin
         Format.print_string "Rewrite rules:\n";
         List.iter (fun ((sym, pos), (_, _, rewrites)) ->
         Format.print_string sym;
         Format.print_string " ::= ";
         print_pre_rewrites rewrites;
         Format.print_string "\n") pre_grammar
	end

let debug_grammar s grammar =
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string s;
         print_grammar grammar
      end

let debug_parser_sets s nullables first_set follow_set =
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string s;
         Format.print_string "\nNullables:";
         print_psymbol_set nullables;
         Format.print_string "\n\nFirst_set:\n";
         print_first_set first_set;
         Format.print_string "\nFollow_set:\n";
         print_follow_set follow_set;
      end

let debug_symbol s symbol =
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string s;
         print_psymbol symbol;
         Format.print_string "\n\n"
      end

let debug_states s penv states accepts =
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string s;
         Format.print_string "States Length:";
         Format.print_int (List.length states);
         Format.print_string "\n\nStates:\n";
         print_states penv states;
         Format.print_string "\n\nAccepts Length:";
         Format.print_int (Accepts.cardinal accepts);
         Format.print_string "\n"
      end

let debug_ptable s penv ptable ptable_errors =
   if !Phobos_state.debug_phobos then
      begin
         Format.print_string s;
         Format.print_string "\nErrors Length:";
         Format.print_int (List.length ptable_errors);
         Format.print_string "\nError locations:\n";
         print_ploc_list penv ptable ptable_errors;
         Format.print_string "\nAll entries:\n";
         ParserFA.fold (fun () key _ ->
            print_ploc penv ptable key) () ptable;
      end

let debug_ploc_list penv ptable ptable_errors =
   if !Phobos_state.debug_phobos then
      print_ploc_list penv ptable ptable_errors

