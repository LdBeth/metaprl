(*
 * Generic parser.
 * Provides parser types and collections.
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

(*************************************************
 * Basics.
 *************************************************)
module StringBase =
struct
   type t = string
   let compare = compare
end

module StringSet = Lm_set.LmMake (StringBase)
module StringTable = Lm_map.LmMake (StringBase)

module IntBase =
struct
   type t = int
   let compare = compare
end

module IntSet = Lm_set.LmMake (IntBase)
module IntTable = Lm_map.LmMake (IntBase)
module IntMTable = Lm_map.LmMakeList (IntBase)

(*************************************************
 * Parsing Phobos files.
 *************************************************)
(*
 * Parse position.
 *)
type pos = string * int * int * int * int

type id = string * pos

type token_option =
   Token_extend of id list option
 | Token_remove of (string * pos) list
 | Token_override of id list

type term_option =
   Term_extend of string * (id * int) list

type loption =
   Lo_longest
 | Lo_first
 | Lo_naml
 | Lo_fc
 | Lo_pasqual
 | Lo_pascal

type directive =
   Dir_nonassoc of id list
 | Dir_leftassoc of id list
 | Dir_rightassoc of id list

type assoc =
     NonAssoc
   | LeftAssoc
   | RightAssoc

type goption =
   Go_start of string
 | Go_unknown

(*
 * Parse errors.
 *)
exception ParseError of pos * string

(*************************************************
 * Tokenizer.
 *************************************************)
(* A regexp has a flag (whether tokens matching the
 * regexp are ignored or not), a name, and a compiled
 * regexp.
 *)
type cregexp = bool * string * Str.regexp
type multi_regexp = bool * string * string list

(*************************************************
 * Parser symbols.
 *************************************************)
(* 
 * A psymbol can be a terminal (token),
 * a non-terminal, an empty symbol, or an EOF.
 *)
type psymbol =
   NonTerminal of string
 | Terminal of string
 | Empty
 | Eof

(*************************************************
 * Higher ADT's with psymbols.
 *************************************************)
 
let compare_strings = compare

(*
 * Relative order: Empty Eof Terminal NonTerminal.
 *)
let psymbol_compare ps1 ps2 =
   match ps1, ps2 with
	NonTerminal s1, NonTerminal s2 ->
	   compare_strings s1 s2
    | Terminal s1, Terminal s2 ->
	   compare_strings s1 s2
    | Empty, Empty ->
         0
    | Eof, Eof ->
         0
    | Terminal _, NonTerminal _ ->
         -1
    | NonTerminal _, Terminal _ ->
         1
    | Empty, _ ->
         -1
    | _, Empty ->
         1
    | Eof, _ ->
         -1
    | _, Eof ->
         1

let rec psymbol_list_compare psl1 psl2 =
   match psl1, psl2 with
      hd1 :: rest1, hd2 :: rest2 ->
         let tmp = psymbol_compare hd1 hd2 in
         if tmp <> 0 then
            tmp
         else
            psymbol_list_compare rest1 rest2
    | [], [] ->
         0
    | a, [] ->
         1
    | [], a ->
         -1

module PSymbolBase =
struct
   type t = psymbol
   let compare = psymbol_compare
end

module PSymbolSet = Lm_set.LmMake (PSymbolBase)
module PSymbolTable = Lm_map.LmMake (PSymbolBase)
module PSymbolMTable = Lm_map.LmMakeList (PSymbolBase)

let psymbol_set_compare pss1 pss2 =
   let len1 = PSymbolSet.cardinal pss1 in
   let len2 = PSymbolSet.cardinal pss2 in
   let tmp = PSymbolSet.equal pss1 pss2 in
   if tmp then
      0
   else
      compare len1 len2

(*************************************************
 * CFG's and their representation.
 *************************************************)
(*
 * A rule associates a non-terminal psymbol to a list of
 * psymbols. It also has an optional precedence symbol 
 * and its position.
 *)

type mp_pre_term = Refiner.Refiner.Term.term * pos
type mp_term = Refiner.Refiner.Term.term

type mp_pre_rewrite = mp_pre_term list * mp_pre_term
type mp_pre_term_rewrite = mp_pre_term * mp_pre_term

type mp_rewrite = Refiner.Refiner.Rewrite.rewrite_rule

(* Pre_rule: this is the type of the incoming productions *)
type pre_rule = id * (id list * id option * mp_pre_rewrite list)
type rule = psymbol * pos * (psymbol * pos) list * id option * mp_pre_rewrite list

(*
 * A grammar is a set of rules.
 *)
type pre_grammar = pre_rule list
type grammar = rule list
type grammar_table = psymbol list PSymbolMTable.t

(*
 * An item is a production and a position within.
 * We will serialize productions using two tables,
 * one mapping productions to unique integers,
 * and vice versa. Then we will represent an item
 * by a production_id and a position.
 *)   
type production_id = int

let production_id_compare = (-)

type production = psymbol * psymbol list

let production_compare (head1, prods1) (head2, prods2) =
   let tmp = psymbol_compare head1 head2 in
   if tmp <> 0 then
      tmp
   else
      psymbol_list_compare prods1 prods2

module ProductionBase =
struct
   type t = production
   let compare = production_compare
end

module ProductionIdBase =
struct
   type t = production_id
   let compare = production_id_compare
end

module ProductionTable = Lm_map.LmMake (ProductionBase)
module ProductionIdTable = Lm_map.LmMake (ProductionIdBase)
module ProductionIdMTable = Lm_map.LmMakeList (ProductionIdBase)

type production_table = production_id ProductionTable.t
(*
 * A production has an optional precedence symbol.
 *)
type production_id_table = (psymbol * psymbol list * string option) ProductionIdTable.t
(*
 * Rewrite rules are associated with a PrId.
 *)
type rewrite_table = mp_pre_rewrite ProductionIdMTable.t
type crewrite_table = mp_rewrite ProductionIdMTable.t
(*
 * Same with each token (which we store as psymbol).
 *)
type lexer_rewrite_table = mp_pre_rewrite PSymbolMTable.t
type lexer_crewrite_table = mp_rewrite PSymbolMTable.t

type crewrites =
   { rw_lexer  : lexer_crewrite_table;
     rw_parser : crewrite_table
   }

(*
 * A parser item.
 *)
type item = production_id * int

let item_compare (prod_id1, i1) (prod_id2, i2) =
   let tmp = production_id_compare prod_id1 prod_id2 in
   if tmp <> 0 then
      tmp
   else
      i1 - i2

module ItemBase =
struct
   type t = item
   let compare = item_compare
end

(*
 * Compressed parser item.
 * index gets 8 bits, prod_id gets the rest.
 *)
type comp_item = int

let item_of_comp_item ci =
   ci lsr 9, ci land 512

let comp_item_of_item (l, r) =
   l lsl 9 + r

let comp_item_compare = (-)

(*
 * Integer lists.
 *)
exception IntSetCompareResult of int

let int_set_compare is1 is2 =
   let il1 = IntSet.to_list is1 in
   let il2 = IntSet.to_list is2 in
   let size =
      if List.length il1 > List.length il2 then
         -1
      else if List.length il2 > List.length il1 then
         1
      else
         0
   in
   let shorter = ref size in
   try
      List.iter2 (fun a b ->
         if a > b then
            raise (IntSetCompareResult (-1))
         else if a < b then
            raise (IntSetCompareResult 1)) il1 il2;
      0
   with
        IntSetCompareResult i -> i
      | Invalid_argument _ -> !shorter
         
module IntSetBase =
struct
   type t = IntSet.t
   let compare = int_set_compare
end

module IntSetMap = Lm_map.LmMake (IntSetBase)

(*
 * A state is a set of items and their symbol sets.
 *)
module Parser_state = Lm_map.LmMake (ItemBase)

type parser_state = (PSymbolSet.t(* * bool*)) Parser_state.t

type state_struct = 
   { state_map : parser_state;
     state_cache : IntSet.t
   }

(*
 * A parser has a list of states.
 *)
type state_list = parser_state list


type state_list_struct =
   { states_list : state_struct list;
     states_cache : int IntSetMap.t;
   }

type state_with_id_list_struct = (state_struct * int) list
(*   { states_and_id_list : (state_struct * int) list;
     states_and_id_cache : int IntSetMap.t;
   }*)

(*
 * An action edge exists between two states via
 * a symbol. We serialize the states, so we can
 * deal with them easier.
 *)
type state_id = int

type action_edge = state_id * state_id * psymbol

let action_edge_compare (i1, i2, ps1) (j1, j2, ps2) =
   let tmp = compare i1 j1 in
   if tmp <> 0 then
      tmp
   else
   let tmp = compare i2 j2 in
   if tmp <> 0 then
      tmp
   else
      psymbol_compare ps1 ps2

module ActionEdgeBase =
struct
   type t = action_edge
   let compare = action_edge_compare
end

(*
 * In the "state-graph", we have a set of action edges.
 *)
module Action_edges = Lm_set.LmMake (ActionEdgeBase)

module AcceptBase =
struct
   type t = state_id
   let compare = compare
end

(*
 * The set of accepting states.
 *)
module Accepts = Lm_set.LmMake (AcceptBase)

(*************************************************
 * GRAMMAR/PARSER INTERNALS.
 *************************************************)
type token_info = id * token_option list

type grammar_state =
   { grammar_nonterminals     : StringSet.t;
     grammar_terminals        : StringSet.t;
     grammar_assocs           : directive list;
     grammar_token_rules      : (bool * token_info * string * mp_pre_rewrite list) list;
     grammar_start_symbol     : psymbol;
     grammar_grammar          : grammar;
     grammar_termsets         : term_option list list;
     grammar_local_rewrites   : mp_pre_term_rewrite list;
     grammar_post_rewrites    : mp_pre_term_rewrite list list;
     grammar_inline_forms     : mp_pre_term list
   }

type lexer_env =
   { lexer_regexps       : multi_regexp list;
     lexer_options       : loption list;
     lexer_rewrites      : lexer_rewrite_table;
   }

type clexer_env =
   { clexer_regexps      : cregexp list;
     clexer_options      : loption list;
     clexer_rewrites     : lexer_rewrite_table;
   }

type parser_env =
   { parser_module       : string;
     parser_grammar      : grammar_table;
     parser_prod_ids     : production_id_table;
     parser_prods        : production_table;
     parser_nullables    : PSymbolSet.t;
     parser_first_set    : PSymbolSet.t PSymbolTable.t;
     parser_follow_set   : PSymbolSet.t PSymbolTable.t;
     parser_rewrites     : rewrite_table
   }

(*
 * A location within a parsing table is determined by
 * a state and a psymbol.
 *)
type ploc = state_id * psymbol

let ploc_compare (i1, ps1) (i2, ps2) =
   let temp = compare i1 i2 in (**)
   if temp <> 0 then
      temp
   else
      psymbol_compare ps1 ps2
 
module FABase =
struct
   type t = ploc
   let compare = ploc_compare
end

(*
 * We will use an FA to figure out when to do certain
 * parsing actions.
 * Its transition table is likely to be sparse,
 * so we will store it as a map.
 *)
module ParserFA = Lm_map.LmMakeList (FABase)

(*
 * An entry in the parsing table is an action to
 * be taken.
 *)
type pentry =
   Shift of state_id
 | Goto of state_id
 | Reduce of production_id
 | Accept
 | Error

(*
 * A parsing table is a mapping from ploc to pentry's.
 *)
type parsing_table = pentry ParserFA.t

(*
 * We can track of errors in the parsing table
 * by keeping their positions.
 *)
type parsing_table_error = ploc list

(*************************************************
 * PDA's stack
 *************************************************)
(*
 * Stack elements.
 * They are either a state identifier or a term.
 *)
type stack_entry =
   Sta_state of state_id
 | Sta_term of mp_term

type stack = stack_entry Stack.t

(*************************************************
 * Return value from Phobos parser.
 *************************************************)
type phobos_parser_return_type =
   { phobos_module_name : string;
     phobos_includes : string list;
     phobos_lexer_info : (bool * token_info * string * mp_pre_rewrite list) list * loption list;
     phobos_assoc_info : directive list;
     phobos_grammar_info : pre_rule list * goption list;
     phobos_termsets : term_option list list;
     phobos_local_rewrites : mp_pre_term_rewrite list;
     phobos_post_rewrites : mp_pre_term_rewrite list list;
     phobos_inline_forms : mp_pre_term list
   }

(*************************************************
 * Source.
 *************************************************)
type source = (psymbol * string * pos) list

