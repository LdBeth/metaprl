open Phobos_type

val bogus_pos : string * int * int * int * int
val source_pos : pos ref
val source_position : unit -> pos
val set_source_position : pos -> unit
val print_psymbol : psymbol -> unit
val print_psymbol_set : PSymbolSet.t -> unit
val psymbol_equal : psymbol -> psymbol -> bool
val psymbol_set_empty : PSymbolSet.t
val psymbol_set_mem :
  PSymbolSet.t -> PSymbolSet.elt -> bool
val psymbol_set_add :
  PSymbolSet.t ->
  PSymbolSet.elt -> PSymbolSet.t
val psymbol_set_of_list :
  PSymbolSet.elt list -> PSymbolSet.t
val psymbol_set_to_list :
  PSymbolSet.t -> PSymbolSet.elt list
val psymbol_set_union :
  PSymbolSet.t ->
  PSymbolSet.t -> PSymbolSet.t
val psymbol_set_equal :
  PSymbolSet.t -> PSymbolSet.t -> bool
val psymbol_set_diff :
  PSymbolSet.t ->
  PSymbolSet.t -> PSymbolSet.t
val psymbol_set_size : PSymbolSet.t -> int
val psymbol_set_subset :
  PSymbolSet.t -> PSymbolSet.t -> bool
val psymbol_list_equal :
  PSymbolSet.elt list -> PSymbolSet.elt list -> bool
val psymbol_list_union :
  PSymbolSet.elt list ->
  PSymbolSet.elt list -> PSymbolSet.elt list
val string_set_add :
  StringSet.t -> StringSet.elt -> StringSet.t
val string_set_empty :
  StringSet.t
val string_set_mem :
  StringSet.t -> StringSet.elt -> bool
val string_set_union :
  StringSet.t -> StringSet.t -> StringSet.t
val string_set_of_list :
  StringSet.elt list -> StringSet.t
val find_productions :
  'a PSymbolMTable.t -> PSymbolMTable.key -> 'a list
val nullables_add :
  PSymbolSet.t ->
  PSymbolSet.elt -> PSymbolSet.t
val is_nullable :
  PSymbolSet.t -> PSymbolSet.elt -> bool
val is_nullable_list :
  PSymbolSet.t -> PSymbolSet.elt list -> bool
val first_set_find_as_set :
  PSymbolSet.t PSymbolTable.t ->
  PSymbolTable.key -> PSymbolSet.t
val first_set_find :
  PSymbolSet.t PSymbolTable.t ->
  PSymbolTable.key -> PSymbolSet.elt list
val first_set_add :
  PSymbolSet.t PSymbolTable.t ->
  PSymbolTable.key ->
  PSymbolSet.elt ->
  PSymbolSet.t PSymbolTable.t
val first_set_add_list :
  PSymbolSet.t PSymbolTable.t ->
  PSymbolTable.key ->
  PSymbolSet.elt list ->
  PSymbolSet.t PSymbolTable.t
val follow_set_find_as_set :
  PSymbolSet.t PSymbolTable.t ->
  PSymbolTable.key -> PSymbolSet.t
val follow_set_find :
  PSymbolSet.t PSymbolTable.t ->
  PSymbolTable.key -> PSymbolSet.elt list
val follow_set_add :
  PSymbolSet.t PSymbolTable.t ->
  PSymbolTable.key ->
  PSymbolSet.elt ->
  PSymbolSet.t PSymbolTable.t
val follow_set_add_list :
  PSymbolSet.t PSymbolTable.t ->
  PSymbolTable.key ->
  PSymbolSet.elt list ->
  PSymbolSet.t PSymbolTable.t
val grammar_table_add :
  'a PSymbolMTable.t ->
  PSymbolMTable.key -> 'a -> 'a PSymbolMTable.t
val grammar_table_find :
  'a PSymbolMTable.t -> PSymbolMTable.key -> 'a list
val grammar_table_add_once :
  'a PSymbolMTable.t ->
  PSymbolMTable.key -> 'a -> 'a PSymbolMTable.t
val prod_id_empty : 'a ProductionIdTable.t
val prod_id_find_unsafe :
  'a ProductionIdTable.t ->
  ProductionIdTable.key -> 'a
val prod_id_add :
  'a ProductionIdTable.t ->
  ProductionIdTable.key ->
  'a -> 'a ProductionIdTable.t
val prod_id_replace :
  'a ProductionIdTable.t ->
  ProductionIdTable.key ->
  'a -> 'a ProductionIdTable.t
val print_psym : psymbol -> unit
val print_psym_list : psymbol list -> unit
val prod_empty : 'a ProductionTable.t
val prod_find_unsafe :
  'a ProductionTable.t -> ProductionTable.key -> 'a
val prod_find :
  'a ProductionTable.t ->
  psymbol * psymbol list -> 'a
val prod_add :
  'a ProductionTable.t ->
  ProductionTable.key -> 'a -> 'a ProductionTable.t
val rewrite_empty : 'a ProductionIdMTable.t
val rewrite_find_unsafe :
  'a ProductionIdMTable.t ->
  ProductionIdMTable.key -> 'a list
val rewrite_add :
  'a ProductionIdMTable.t ->
  ProductionIdMTable.key ->
  'a -> 'a ProductionIdMTable.t
val rewrite_add_list :
  'a ProductionIdMTable.t ->
  ProductionIdMTable.key ->
  'a list -> 'a ProductionIdMTable.t
val rewrite_replace_list :
  'a ProductionIdMTable.t ->
  ProductionIdMTable.key ->
  'a list -> 'a ProductionIdMTable.t
val lex_rewrite_empty : 'a PSymbolMTable.t
val lex_rewrite_find_unsafe :
  'a PSymbolMTable.t -> PSymbolMTable.key -> 'a list
val lex_rewrite_add :
  'a PSymbolMTable.t ->
  PSymbolMTable.key -> 'a -> 'a PSymbolMTable.t
val lex_rewrite_add_list :
  'a PSymbolMTable.t ->
  PSymbolMTable.key -> 'a list -> 'a PSymbolMTable.t
val lex_rewrite_add_or_replace_list :
  'a PSymbolMTable.t ->
  PSymbolMTable.key -> 'a list -> 'a PSymbolMTable.t
val state_empty : state_struct
val state_find_unsafe :
  state_struct ->
  Parser_state.key -> PSymbolSet.t
val state_find_safe :
  state_struct ->
  Parser_state.key -> PSymbolSet.t
val state_add :
  state_struct ->
  Parser_state.key ->
  PSymbolSet.elt -> state_struct * bool
val state_add_set :
  state_struct ->
  Parser_state.key ->
  PSymbolSet.t ->
  state_struct * (bool * PSymbolSet.t)
val state_union :
  state_struct ->
  state_struct -> state_struct * bool
val state_equal :
  state_struct -> state_struct -> bool
val states_empty : state_list_struct
val states_nth : 'a list -> int -> 'a
val states_contain_aux :
  state_list_struct ->
  state_struct -> 'a -> bool * int
val states_contain :
  state_list_struct -> state_struct -> bool
val states_id_of :
  state_list_struct -> state_struct -> int
val states_contain_and_id :
  state_list_struct -> state_struct -> bool * int
exception IntSetRetrieve of IntSet.t
val int_set_retrieve_key :
  'a IntSetMap.t -> 'a -> IntSet.t
val states_union_symbols :
  state_list_struct ->
  int ->
  state_struct ->
  state_list_struct * state_struct * bool
val obtain_id :
  state_list_struct ->
  state_struct ->
  state_list_struct * int * bool * bool *
  state_struct
val actions_empty : Action_edges.t
val actions_union :
  Action_edges.t ->
  Action_edges.t -> Action_edges.t
val actions_add :
  Action_edges.t ->
  Action_edges.elt -> Action_edges.t
val actions_equal :
  Action_edges.t -> Action_edges.t -> bool
val accepts_empty : Accepts.t
val accepts_add :
  Accepts.t -> Accepts.elt -> Accepts.t
val accepts_equal : Accepts.t -> Accepts.t -> bool
val parsing_table_empty : 'a ParserFA.t
val parsing_table_find_unsafe :
  'a ParserFA.t -> ParserFA.key -> 'a list
val parsing_table_find :
  'a ParserFA.t -> ParserFA.key -> 'a list
val parsing_table_add :
  'a ParserFA.t ->
  ParserFA.key list ->
  ParserFA.key ->
  'a -> 'a ParserFA.t * ParserFA.key list
val parsing_table_remove :
  'a ParserFA.t ->
  ParserFA.key -> 'a ParserFA.t
val parsing_errors_remove :
  ('a * psymbol) list ->
  'a * psymbol -> ('a * psymbol) list
val chop_extension : string -> string
val filename_of_compiled_grammar : string -> string
val filename_of_grammar_output : string -> string
val find_file : string list -> string -> string
val print_warning : pos -> string -> unit
