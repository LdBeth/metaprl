open Lm_set_sig
open Lm_map_sig

module StringSet : LmSet with type elt = string
module StringTable : LmMap with type key = string
module IntSet : LmSet with type elt = int
module IntTable : LmMap with type key = int
module IntMTable : LmMapList with type key = int

type pos = string * int * int * int * int
and id = string * pos
and token_option =
    Token_extend of id list option
  | Token_remove of (string * pos) list
  | Token_override of id list
and term_option = Term_extend of string * (id * int) list
and loption =
    Lo_longest
  | Lo_first
  | Lo_naml
  | Lo_fc
  | Lo_pasqual
  | Lo_pascal
and directive =
    Dir_nonassoc of id list
  | Dir_leftassoc of id list
  | Dir_rightassoc of id list
and assoc = NonAssoc | LeftAssoc | RightAssoc
and goption = Go_start of string | Go_unknown
exception ParseError of pos * string
type cregexp = bool * string * Str.regexp
and multi_regexp = bool * string * string list
and psymbol = NonTerminal of string | Terminal of string | Empty | Eof

val compare_strings : 'a -> 'a -> int
val psymbol_compare : psymbol -> psymbol -> int
val psymbol_list_compare : psymbol list -> psymbol list -> int

module PSymbolSet : LmSet with type elt = psymbol
module PSymbolTable : LmMap with type key = psymbol
module PSymbolMTable : LmMapList with type key = psymbol

val psymbol_set_compare : PSymbolSet.t -> PSymbolSet.t -> int
type mp_pre_term = Refiner.Refiner.Term.term * pos
and mp_term = Refiner.Refiner.Term.term
and mp_pre_rewrite = mp_pre_term list * mp_pre_term
and mp_pre_term_rewrite = mp_pre_term * mp_pre_term
and mp_rewrite = Refiner.Refiner.Rewrite.rewrite_rule
and pre_rule = id * (id list * id option * mp_pre_rewrite list)
and rule = psymbol * pos * (psymbol * pos) list * id option *
           mp_pre_rewrite list
and pre_grammar = pre_rule list
and grammar = rule list
and grammar_table = psymbol list PSymbolMTable.t
and production_id = int
val production_id_compare : int -> int -> int
type production = psymbol * psymbol list
val production_compare :
  psymbol * psymbol list -> psymbol * psymbol list -> int

module ProductionTable : LmMap with type key = production
module ProductionIdTable : LmMap with type key = production_id
module ProductionIdMTable : LmMapList with type key = production_id

type production_table = production_id ProductionTable.t
and production_id_table = (psymbol * psymbol list * string option)
                          ProductionIdTable.t
and rewrite_table = mp_pre_rewrite ProductionIdMTable.t
and crewrite_table = mp_rewrite ProductionIdMTable.t
and lexer_rewrite_table = mp_pre_rewrite PSymbolMTable.t
and lexer_crewrite_table = mp_rewrite PSymbolMTable.t
and crewrites = {
  rw_lexer : lexer_crewrite_table;
  rw_parser : crewrite_table;
} 
and item = production_id * int

val item_compare : int * int -> int * int -> int

type comp_item = int
val item_of_comp_item : int -> int * int
val comp_item_of_item : int * int -> int
val comp_item_compare : int -> int -> int
exception IntSetCompareResult of int
val int_set_compare : IntSet.t -> IntSet.t -> int

module IntSetMap : LmMap with type key = IntSet.t
module Parser_state : LmMap with type key = item

type parser_state = PSymbolSet.t Parser_state.t
and state_struct = { state_map : parser_state; state_cache : IntSet.t; } 
and state_list = parser_state list
and state_list_struct = {
  states_list : state_struct list;
  states_cache : int IntSetMap.t;
} 
and state_with_id_list_struct = (state_struct * int) list
and state_id = int
and action_edge = state_id * state_id * psymbol
val action_edge_compare : 'a * 'b * psymbol -> 'a * 'b * psymbol -> int

module Action_edges : LmSet with type elt = action_edge
module Accepts : LmSet with type elt = state_id

type token_info = id * token_option list
and grammar_state = {
  grammar_nonterminals : StringSet.t;
  grammar_terminals : StringSet.t;
  grammar_assocs : directive list;
  grammar_token_rules :
    (bool * token_info * string * mp_pre_rewrite list) list;
  grammar_start_symbol : psymbol;
  grammar_grammar : grammar;
  grammar_termsets : term_option list list;
  grammar_local_rewrites : mp_pre_term_rewrite list;
  grammar_post_rewrites : mp_pre_term_rewrite list list;
  grammar_inline_forms : mp_pre_term list;
} 
and lexer_env = {
  lexer_regexps : multi_regexp list;
  lexer_options : loption list;
  lexer_rewrites : lexer_rewrite_table;
} 
and clexer_env = {
  clexer_regexps : cregexp list;
  clexer_options : loption list;
  clexer_rewrites : lexer_rewrite_table;
} 
and parser_env = {
  parser_module : string;
  parser_grammar : grammar_table;
  parser_prod_ids : production_id_table;
  parser_prods : production_table;
  parser_nullables : PSymbolSet.t;
  parser_first_set : PSymbolSet.t PSymbolTable.t;
  parser_follow_set : PSymbolSet.t PSymbolTable.t;
  parser_rewrites : rewrite_table;
} 
and ploc = state_id * psymbol
val ploc_compare : 'a * psymbol -> 'a * psymbol -> int

module ParserFA : LmMapList with type key = ploc

type pentry =
    Shift of state_id
  | Goto of state_id
  | Reduce of production_id
  | Accept
  | Error
and parsing_table = pentry ParserFA.t
and parsing_table_error = ploc list
and stack_entry = Sta_state of state_id | Sta_term of mp_term
and stack = stack_entry Stack.t
and phobos_parser_return_type = {
  phobos_module_name : string;
  phobos_includes : string list;
  phobos_lexer_info :
    (bool * token_info * string * mp_pre_rewrite list) list * loption list;
  phobos_assoc_info : directive list;
  phobos_grammar_info : pre_rule list * goption list;
  phobos_termsets : term_option list list;
  phobos_local_rewrites : mp_pre_term_rewrite list;
  phobos_post_rewrites : mp_pre_term_rewrite list list;
  phobos_inline_forms : mp_pre_term list;
} 
and source = (psymbol * string * pos) list
