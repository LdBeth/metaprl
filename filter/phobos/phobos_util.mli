val bogus_pos : string * int * int * int * int
val source_pos : Phobos_type.pos ref
val source_position : unit -> Phobos_type.pos
val set_source_position : Phobos_type.pos -> unit
val print_psymbol : Phobos_type.psymbol -> unit
val print_psymbol_set : Phobos_type.PSymbolSet.t -> unit
val psymbol_equal : Phobos_type.psymbol -> Phobos_type.psymbol -> bool
val psymbol_set_empty : Phobos_type.PSymbolSet.t
val psymbol_set_mem :
  Phobos_type.PSymbolSet.t -> Phobos_type.PSymbolSet.elt -> bool
val psymbol_set_add :
  Phobos_type.PSymbolSet.t ->
  Phobos_type.PSymbolSet.elt -> Phobos_type.PSymbolSet.t
val psymbol_set_of_list :
  Phobos_type.PSymbolSet.elt list -> Phobos_type.PSymbolSet.t
val psymbol_set_to_list :
  Phobos_type.PSymbolSet.t -> Phobos_type.PSymbolSet.elt list
val psymbol_set_union :
  Phobos_type.PSymbolSet.t ->
  Phobos_type.PSymbolSet.t -> Phobos_type.PSymbolSet.t
val psymbol_set_equal :
  Phobos_type.PSymbolSet.t -> Phobos_type.PSymbolSet.t -> bool
val psymbol_set_diff :
  Phobos_type.PSymbolSet.t ->
  Phobos_type.PSymbolSet.t -> Phobos_type.PSymbolSet.t
val psymbol_set_size : Phobos_type.PSymbolSet.t -> int
val psymbol_set_subset :
  Phobos_type.PSymbolSet.t -> Phobos_type.PSymbolSet.t -> bool
val psymbol_list_equal :
  Phobos_type.PSymbolSet.elt list -> Phobos_type.PSymbolSet.elt list -> bool
val psymbol_list_union :
  Phobos_type.PSymbolSet.elt list ->
  Phobos_type.PSymbolSet.elt list -> Phobos_type.PSymbolSet.elt list
val string_set_mem :
  Phobos_type.StringSet.t -> Phobos_type.StringSet.elt -> bool
val string_set_union :
  Phobos_type.StringSet.t -> Phobos_type.StringSet.t -> Phobos_type.StringSet.t
val string_set_of_list :
  Phobos_type.StringSet.elt list -> Phobos_type.StringSet.t
val find_productions :
  'a Phobos_type.PSymbolMTable.t -> Phobos_type.PSymbolMTable.key -> 'a list
val nullables_add :
  Phobos_type.PSymbolSet.t ->
  Phobos_type.PSymbolSet.elt -> Phobos_type.PSymbolSet.t
val is_nullable :
  Phobos_type.PSymbolSet.t -> Phobos_type.PSymbolSet.elt -> bool
val is_nullable_list :
  Phobos_type.PSymbolSet.t -> Phobos_type.PSymbolSet.elt list -> bool
val first_set_find_as_set :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolTable.key -> Phobos_type.PSymbolSet.t
val first_set_find :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolTable.key -> Phobos_type.PSymbolSet.elt list
val first_set_add :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolTable.key ->
  Phobos_type.PSymbolSet.elt ->
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t
val first_set_add_list :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolTable.key ->
  Phobos_type.PSymbolSet.elt list ->
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t
val follow_set_find_as_set :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolTable.key -> Phobos_type.PSymbolSet.t
val follow_set_find :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolTable.key -> Phobos_type.PSymbolSet.elt list
val follow_set_add :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolTable.key ->
  Phobos_type.PSymbolSet.elt ->
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t
val follow_set_add_list :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolTable.key ->
  Phobos_type.PSymbolSet.elt list ->
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t
val grammar_table_add :
  'a Phobos_type.PSymbolMTable.t ->
  Phobos_type.PSymbolMTable.key -> 'a -> 'a Phobos_type.PSymbolMTable.t
val grammar_table_find :
  'a Phobos_type.PSymbolMTable.t -> Phobos_type.PSymbolMTable.key -> 'a list
val grammar_table_add_once :
  'a Phobos_type.PSymbolMTable.t ->
  Phobos_type.PSymbolMTable.key -> 'a -> 'a Phobos_type.PSymbolMTable.t
val prod_id_empty : 'a Phobos_type.ProductionIdTable.t
val prod_id_find_unsafe :
  'a Phobos_type.ProductionIdTable.t ->
  Phobos_type.ProductionIdTable.key -> 'a
val prod_id_add :
  'a Phobos_type.ProductionIdTable.t ->
  Phobos_type.ProductionIdTable.key ->
  'a -> 'a Phobos_type.ProductionIdTable.t
val prod_id_replace :
  'a Phobos_type.ProductionIdTable.t ->
  Phobos_type.ProductionIdTable.key ->
  'a -> 'a Phobos_type.ProductionIdTable.t
val print_psym : Phobos_type.psymbol -> unit
val print_psym_list : Phobos_type.psymbol list -> unit
val prod_empty : 'a Phobos_type.ProductionTable.t
val prod_find_unsafe :
  'a Phobos_type.ProductionTable.t -> Phobos_type.ProductionTable.key -> 'a
val prod_find :
  'a Phobos_type.ProductionTable.t ->
  Phobos_type.psymbol * Phobos_type.psymbol list -> 'a
val prod_add :
  'a Phobos_type.ProductionTable.t ->
  Phobos_type.ProductionTable.key -> 'a -> 'a Phobos_type.ProductionTable.t
val rewrite_empty : 'a Phobos_type.ProductionIdMTable.t
val rewrite_find_unsafe :
  'a Phobos_type.ProductionIdMTable.t ->
  Phobos_type.ProductionIdMTable.key -> 'a list
val rewrite_add :
  'a Phobos_type.ProductionIdMTable.t ->
  Phobos_type.ProductionIdMTable.key ->
  'a -> 'a Phobos_type.ProductionIdMTable.t
val rewrite_add_list :
  'a Phobos_type.ProductionIdMTable.t ->
  Phobos_type.ProductionIdMTable.key ->
  'a list -> 'a Phobos_type.ProductionIdMTable.t
val rewrite_replace_list :
  'a Phobos_type.ProductionIdMTable.t ->
  Phobos_type.ProductionIdMTable.key ->
  'a list -> 'a Phobos_type.ProductionIdMTable.t
val lex_rewrite_empty : 'a Phobos_type.PSymbolMTable.t
val lex_rewrite_find_unsafe :
  'a Phobos_type.PSymbolMTable.t -> Phobos_type.PSymbolMTable.key -> 'a list
val lex_rewrite_add :
  'a Phobos_type.PSymbolMTable.t ->
  Phobos_type.PSymbolMTable.key -> 'a -> 'a Phobos_type.PSymbolMTable.t
val lex_rewrite_add_list :
  'a Phobos_type.PSymbolMTable.t ->
  Phobos_type.PSymbolMTable.key -> 'a list -> 'a Phobos_type.PSymbolMTable.t
val lex_rewrite_add_or_replace_list :
  'a Phobos_type.PSymbolMTable.t ->
  Phobos_type.PSymbolMTable.key -> 'a list -> 'a Phobos_type.PSymbolMTable.t
val state_empty : Phobos_type.state_struct
val state_find_unsafe :
  Phobos_type.state_struct ->
  Phobos_type.Parser_state.key -> Phobos_type.PSymbolSet.t
val state_find_safe :
  Phobos_type.state_struct ->
  Phobos_type.Parser_state.key -> Phobos_type.PSymbolSet.t
val state_add :
  Phobos_type.state_struct ->
  Phobos_type.Parser_state.key ->
  Phobos_type.PSymbolSet.elt -> Phobos_type.state_struct * bool
val state_add_set :
  Phobos_type.state_struct ->
  Phobos_type.Parser_state.key ->
  Phobos_type.PSymbolSet.t ->
  Phobos_type.state_struct * (bool * Phobos_type.PSymbolSet.t)
val state_union :
  Phobos_type.state_struct ->
  Phobos_type.state_struct -> Phobos_type.state_struct * bool
val state_equal :
  Phobos_type.state_struct -> Phobos_type.state_struct -> bool
val states_empty : Phobos_type.state_list_struct
val states_nth : 'a list -> int -> 'a
val states_contain_aux :
  Phobos_type.state_list_struct ->
  Phobos_type.state_struct -> 'a -> bool * int
val states_contain :
  Phobos_type.state_list_struct -> Phobos_type.state_struct -> bool
val states_id_of :
  Phobos_type.state_list_struct -> Phobos_type.state_struct -> int
val states_contain_and_id :
  Phobos_type.state_list_struct -> Phobos_type.state_struct -> bool * int
exception IntSetRetrieve of Phobos_type.IntSetBase.t
val int_set_retrieve_key :
  'a Phobos_type.IntSetMap.t -> 'a -> Phobos_type.IntSetBase.t
val states_union_symbols :
  Phobos_type.state_list_struct ->
  int ->
  Phobos_type.state_struct ->
  Phobos_type.state_list_struct * Phobos_type.state_struct * bool
val obtain_id :
  Phobos_type.state_list_struct ->
  Phobos_type.state_struct ->
  Phobos_type.state_list_struct * int * bool * bool *
  Phobos_type.state_struct
val actions_empty : Phobos_type.Action_edges.t
val actions_union :
  Phobos_type.Action_edges.t ->
  Phobos_type.Action_edges.t -> Phobos_type.Action_edges.t
val actions_add :
  Phobos_type.Action_edges.t ->
  Phobos_type.Action_edges.elt -> Phobos_type.Action_edges.t
val actions_equal :
  Phobos_type.Action_edges.t -> Phobos_type.Action_edges.t -> bool
val accepts_empty : Phobos_type.Accepts.t
val accepts_add :
  Phobos_type.Accepts.t -> Phobos_type.Accepts.elt -> Phobos_type.Accepts.t
val accepts_equal : Phobos_type.Accepts.t -> Phobos_type.Accepts.t -> bool
val parsing_table_empty : 'a Phobos_type.ParserFA.t
val parsing_table_find_unsafe :
  'a Phobos_type.ParserFA.t -> Phobos_type.ParserFA.key -> 'a list
val parsing_table_find :
  'a Phobos_type.ParserFA.t -> Phobos_type.ParserFA.key -> 'a list
val parsing_table_add :
  'a Phobos_type.ParserFA.t ->
  Phobos_type.ParserFA.key list ->
  Phobos_type.ParserFA.key ->
  'a -> 'a Phobos_type.ParserFA.t * Phobos_type.ParserFA.key list
val parsing_table_remove :
  'a Phobos_type.ParserFA.t ->
  Phobos_type.ParserFA.key -> 'a Phobos_type.ParserFA.t
val parsing_errors_remove :
  ('a * Phobos_type.psymbol) list ->
  'a * Phobos_type.psymbol -> ('a * Phobos_type.psymbol) list
val chop_extension : string -> string
val filename_of_compiled_grammar : string -> string
val filename_of_grammar_output : string -> string
val find_file : string list -> string -> string
val print_warning : Phobos_type.pos -> string -> unit
