(*
 * Grammar utilities.
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
open Phobos_constants
open Phobos_parse_state
open Phobos_exn
open Phobos_print
open Phobos_util
open Phobos_rewrite
open Phobos_marshal
open Phobos_tokenizer
open Phobos_token_inheritance

(*****************************************************
 * Associativity and precedence list.
 *
 * Check that it contains only valid terminals.
 *****************************************************)
let rec check_terminals terminals = function
   (s, pos) :: rest ->
      if string_set_mem terminals s then
         check_terminals terminals rest
      else
         begin
            print_warning pos (string_format "undeclared terminal %s" s);
            check_terminals terminals rest
         end
 | [] ->
      ()

let check_assocs gst =
   List.iter (fun assoc ->
      match assoc with
         Dir_nonassoc lst
       | Dir_leftassoc lst
       | Dir_rightassoc lst ->
            check_terminals gst.grammar_terminals lst) gst.grammar_assocs

let rec assoc_list_mem lst s =
   match lst with
      (s1, pos) :: rest ->
         if s1 = s then
            true
         else
            assoc_list_mem rest s
    | [] ->
         false

(*
 * Return priority (lowest being 1) of a token.
 * For non-terminals, return -1.
 *)
let rec priority_of_aux s level = function
   assoc :: rest ->
   (match assoc with
      Dir_nonassoc lst ->
         if assoc_list_mem lst s then
            level, Some NonAssoc
         else
            priority_of_aux s (level+1) rest
    | Dir_leftassoc lst ->
         if assoc_list_mem lst s then
            level, Some LeftAssoc
         else
            priority_of_aux s (level+1) rest
    | Dir_rightassoc lst ->
         if assoc_list_mem lst s then
            level, Some RightAssoc
         else
            priority_of_aux s (level+1) rest)
 | [] ->
   -1, None

let priority_of s assocs =
   priority_of_aux s 1 assocs

let rec get_start_symbol = function
   opt :: rest ->
      (match opt with
         Go_start s ->
         NonTerminal s
       | _ ->
         get_start_symbol rest)
 | [] ->
   raise (PhobosException (bogus_pos, "no start symbol declared"))

(*
 * Convert from pre-grammar to grammar, and do some
 * sanity checks.
 *)
let rec create_symbols terminals nonterminals = function
   (head, pos) :: rest ->
      let psym =
         if head = "_" then
            Empty, pos
         else if string_set_mem terminals head then
            Terminal head, pos
         else if string_set_mem nonterminals head then
            NonTerminal head, pos
         else
            raise (PhobosException (pos, (Printf.sprintf "undefined symbol [%s]" head)))
      in
         psym :: create_symbols terminals nonterminals rest
 | [] ->
      []

and create_grammar_aux gst = function
   ((sym, pos), (prods, prec_opt, rewrites)) :: rest ->
      (NonTerminal sym, pos, create_symbols gst.grammar_terminals gst.grammar_nonterminals prods,
      prec_opt, List.rev rewrites) :: create_grammar_aux gst rest
 | [] ->
      []

and create_grammar gst pre_grammar =
   List.rev (create_grammar_aux gst pre_grammar)

(*
 * Make sure that every symbol is either a variable (e.g.
 * it has been defined as the head of a production), or a
 * terminal symbol (e.g. there is a regexp for it).
 * We do this by searching the list of terminals and
 * non-terminals that we previously computed.
 *)
let rec check_productions terminals nonterminals = function
   [(Eof, pos)]
 | [(Empty, pos)] ->
      ()
 | (Eof, pos) :: rest ->
      raise (PhobosException (pos, "EOF can't be followed by any symbols"))
 | (Empty, pos) :: rest ->
      raise (PhobosException (pos, "'_' can't be followed by any symbols"))
 | (psym, pos) :: rest ->
      let sym = string_of_psymbol psym in
      if string_set_mem nonterminals sym then
         check_productions terminals nonterminals rest
      else
      if string_set_mem terminals sym then
         check_productions terminals nonterminals rest
      else
         raise (PhobosException (pos, string_add ["unbound symbol ["; sym; "]"]))
 | [] ->
      ()

and check_grammar gst =
   List.iter (fun (_, _, prods, _, _) ->
      check_productions gst.grammar_terminals gst.grammar_nonterminals prods) gst.grammar_grammar

let grammar_table_of_grammar (grammar: grammar) =
   List.fold_left (fun grammar_table (psym, _, prods, prec_opt, rewrites) ->
      let prods = List.map fst prods in
      (* Do not add duplicate productions *)
      let grammar_table = grammar_table_add_once grammar_table psym prods in
         grammar_table) PSymbolMTable.empty grammar

(*
 * Augment grammar with a new start production S' -> S$.
 *)
let augment_grammar gst =
   (* Leave if no start symbol is given *)
   if gst.grammar_start_symbol = bogus_symbol then
      gst
   else
      (* We make up a position for the new start symbol *)
      let new_production =
         (global_start_symbol, bogus_pos, [(gst.grammar_start_symbol, bogus_pos); (Eof, bogus_pos)], None, [])
      in
         { grammar_nonterminals  = gst.grammar_nonterminals;
           grammar_terminals     = gst.grammar_terminals;
           grammar_assocs        = gst.grammar_assocs;
           grammar_token_rules   = gst.grammar_token_rules;
           grammar_start_symbol  = gst.grammar_start_symbol;
           grammar_grammar       = new_production :: gst.grammar_grammar;
           grammar_termsets      = gst.grammar_termsets;
           grammar_local_rewrites= gst.grammar_local_rewrites;
           grammar_post_rewrites = gst.grammar_post_rewrites;
           grammar_inline_forms  = gst.grammar_inline_forms
         }

let remove_start_productions gst =
   (* Remove redefined start productions *)
   if gst.grammar_start_symbol <> bogus_symbol then begin
      let (grammar', _) = List.partition (fun (head_sym, _, _, _, _) ->
         match head_sym with
            NonTerminal s when s = global_start_string ->
               false
         | _ ->
               true) gst.grammar_grammar
      in
         grammar'
      end else
         gst.grammar_grammar

(*
 * Include other semantic modules.
 * For now, there are a couple bogus things in here.
 * [regexps; assocs; grammar; ..]
 *)
let include_grammars paths includes =
   let dummy_gst =
      { grammar_nonterminals     = StringSet.empty;
        grammar_terminals        = StringSet.empty;
        grammar_assocs           = [];
        grammar_token_rules      = [];
        grammar_start_symbol     = bogus_symbol;
        grammar_grammar          = [];
        grammar_termsets         = [];
        grammar_local_rewrites   = [];
        grammar_post_rewrites    = [];
        grammar_inline_forms     = []
      }
   in
   let gst =
      List.fold_left (fun gst incl ->
         let gst', _, _, _ = load_grammar (find_file paths incl) in
         (* Remove redefined start productions *)
         let old_grammar = remove_start_productions gst in
         (* Keep old start symbol if no new one is given *)
         let start_symbol =
            if gst'.grammar_start_symbol = bogus_symbol then
               gst.grammar_start_symbol
            else
               gst'.grammar_start_symbol
         in
         let nt_set = gst.grammar_nonterminals in
         let nt_set' = gst'.grammar_nonterminals in
         let t_set = gst.grammar_terminals in
         let t_set' = gst'.grammar_terminals in
            { grammar_nonterminals     = string_set_union nt_set nt_set';
              grammar_terminals        = string_set_union t_set t_set';
              grammar_assocs           = gst'.grammar_assocs;
              grammar_token_rules      = gst'.grammar_token_rules @ gst.grammar_token_rules;
              grammar_start_symbol     = start_symbol;
              grammar_grammar          = gst'.grammar_grammar @ old_grammar;
              grammar_termsets         = gst'.grammar_termsets @ gst.grammar_termsets;
              grammar_local_rewrites   = gst'.grammar_local_rewrites @ gst.grammar_local_rewrites;
              grammar_post_rewrites    = gst'.grammar_post_rewrites @ gst.grammar_post_rewrites;
              grammar_inline_forms     = gst'.grammar_inline_forms @ gst.grammar_inline_forms
            }) dummy_gst includes
      in
         if !debug_phobos then begin
            Format.print_string "After include_grammars: terminals = \n";
            print_string_set gst.grammar_terminals;
            Format.print_string "After include_grammars: nonterminals = \n";
            print_string_set gst.grammar_nonterminals
         end;
         gst

(*
 * Process fresh pre_grammar.
 *)
let compile paths
   { phobos_module_name = module_name;
     phobos_includes = includes;
     phobos_termsets = termsets;
     phobos_local_rewrites = local_pre_rewrites;
     phobos_lexer_info = (token_rules, loptions);
     phobos_assoc_info = assocs;
     phobos_grammar_info = (pre_grammar, grammar_options);
     phobos_post_rewrites = post_pre_rewrites;
     phobos_inline_forms = inline_forms
   } =
   debug_pre_grammar pre_grammar;

   (* Produce terminal and nonterminal symbols *)
   let terminals = List.map (fun (_, ((sym, _), _), _, _) -> sym) token_rules in
   let nonterminals = List.map (fun ((sym, _), _) -> sym) pre_grammar in

   (* Filter out duplicates *)
   let terminals = string_set_of_list (List.sort compare terminals) in
   let nonterminals = string_set_of_list (List.sort compare nonterminals) in
   debug_symbols "Symbols defined in this module (with no duplicates):\n" nonterminals terminals;

   (* Import necessary semantic modules. *)
   let gst = include_grammars paths includes in

   (* Do we redefine disambiguation rules? *)
   let assocs =
      match assocs with
         [] ->
            gst.grammar_assocs
       | _ ->
            assocs
   in

   (* Retrieve start symbol *)
   let start_symbol, ss_is_defined =
      try
         (* Do we have a new start symbol? *)
         get_start_symbol grammar_options, true
      with
         Phobos_exn.PhobosException _ ->
            let old_symbol = gst.grammar_start_symbol in
            if old_symbol <> bogus_symbol then
               old_symbol, true
            else
               bogus_symbol, false
   in

   debug_symbols "After joining with inherited grammars\n" 
      (string_set_union gst.grammar_nonterminals nonterminals)
      (string_set_union gst.grammar_terminals terminals);

   (* Add terminal and nonterminal symbols, and remove start production *)
   let gst =
      { grammar_nonterminals  = string_set_union gst.grammar_nonterminals nonterminals;
        grammar_terminals     = string_set_union gst.grammar_terminals terminals;
        grammar_assocs        = gst.grammar_assocs;
        grammar_token_rules   = gst.grammar_token_rules;
        grammar_start_symbol  = gst.grammar_start_symbol;
        grammar_grammar       = remove_start_productions gst;
        grammar_termsets      = gst.grammar_termsets;
        grammar_local_rewrites= gst.grammar_local_rewrites;
        grammar_post_rewrites = gst.grammar_post_rewrites;
        grammar_inline_forms  = gst.grammar_inline_forms
      }
   in

   (* Create grammar from pre_grammar *)
   let grammar = create_grammar gst pre_grammar in

   (* Update grammar information if necessary *)
   let gst =
      { grammar_nonterminals  = string_set_union gst.grammar_nonterminals nonterminals;
        grammar_terminals     = string_set_union gst.grammar_terminals terminals;
        grammar_assocs        = assocs;
        grammar_token_rules   = gst.grammar_token_rules @ token_rules;
        grammar_start_symbol  = start_symbol;
        grammar_grammar       = gst.grammar_grammar @ grammar;
        grammar_termsets      = gst.grammar_termsets @ termsets;
        grammar_local_rewrites= gst.grammar_local_rewrites @ local_pre_rewrites;
        grammar_post_rewrites = gst.grammar_post_rewrites @ post_pre_rewrites;
        grammar_inline_forms  = gst.grammar_inline_forms @ inline_forms
      }
   in

   (* Augment grammar with new start production *)
   let gst = augment_grammar gst in

   (* Check associativity list *)
   check_assocs gst;

   (* Do sanity-check on grammar *)
   check_grammar gst;

   (* Create lexer environment *)
   let lenv = create_lenv gst loptions in

   (* Apply token inheritance rules (-extend/-override/-remove) *)
   let gst, lenv = apply_token_inheritance gst lenv in

   (* Are we applying the token rules right? *)
   debug_regexps lenv.lexer_regexps;

   debug_token_rules "\nWe have the following lexer-rewrites:\n" gst.grammar_token_rules;
      gst, module_name, lenv, ss_is_defined

