val print_string_list : string list -> unit
val print_string_set : Phobos_type.StringSet.t -> unit
val string_of_psymbol : Phobos_type.psymbol -> string
val print_psymbol : Phobos_type.psymbol -> unit
val print_psymbol_set : Phobos_type.PSymbolSet.t -> unit
val print_psymbol_list : Phobos_type.psymbol list -> unit
val print_psymbol_set_of_sets :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t -> unit
val print_first_set :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t -> unit
val print_follow_set :
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t -> unit
val print_grammar_productions : (Phobos_type.psymbol * 'a) list -> unit
val print_opt_prec : (string * 'a) option -> unit
val print_grammar :
  (Phobos_type.psymbol * 'a * (Phobos_type.psymbol * 'b) list *
   (string * 'c) option * 'd)
  list -> unit
val print_rule_loc :
  (Phobos_type.psymbol * Phobos_type.psymbol list * string option)
  Phobos_type.ProductionIdTable.t ->
  Phobos_type.ProductionIdTable.key * int -> unit
val print_rule_loc_list :
  (Phobos_type.psymbol * Phobos_type.psymbol list * string option)
  Phobos_type.ProductionIdTable.t ->
  (Phobos_type.ProductionIdTable.key * int) list -> unit
val print_item :
  (Phobos_type.psymbol * Phobos_type.psymbol list * string option)
  Phobos_type.ProductionIdTable.t ->
  Phobos_type.ProductionIdTable.key * int -> Phobos_type.PSymbolSet.t -> unit
val print_productions :
  Phobos_type.psymbol -> Phobos_type.psymbol list list -> unit
val print_state :
  Phobos_type.parser_env ->
  Phobos_type.PSymbolSet.t Phobos_type.Parser_state.t -> unit
val print_states :
  Phobos_type.parser_env -> Phobos_type.state_struct list -> unit
val print_pentry : Phobos_type.parser_env -> Phobos_type.pentry -> unit
val print_pentry_simple : Phobos_type.pentry -> unit
val print_pentry_list :
  Phobos_type.parser_env -> Phobos_type.pentry list -> unit
val print_pentry_simple_list : Phobos_type.pentry list -> unit
val print_ploc :
  Phobos_type.parser_env ->
  Phobos_type.pentry Phobos_type.ParserFA.t ->
  Phobos_type.state_id * Phobos_type.psymbol -> unit
val print_ploc_list :
  Phobos_type.parser_env ->
  Phobos_type.pentry Phobos_type.ParserFA.t ->
  (Phobos_type.state_id * Phobos_type.psymbol) list -> unit
val print_pre_term : Simple_print.SimplePrint.term * 'a -> unit
val print_pre_terms : (Simple_print.SimplePrint.term * 'a) list -> unit
val print_pre_term_rewrite :
  Simple_print.SimplePrint.term * 'a ->
  Simple_print.SimplePrint.term * 'b -> unit
val print_pre_rewrite :
  (Simple_print.SimplePrint.term * 'a) list ->
  Simple_print.SimplePrint.term * 'b -> unit
val print_pre_rewrites :
  ((Simple_print.SimplePrint.term * 'a) list *
   (Simple_print.SimplePrint.term * 'b))
  list -> unit
val print_pre_term_rewrites :
  ((Simple_print.SimplePrint.term * 'a) *
   (Simple_print.SimplePrint.term * 'b))
  list -> unit
val print_rewrite_table :
  ((Simple_print.SimplePrint.term * 'a) list *
   (Simple_print.SimplePrint.term * 'b))
  Phobos_type.PSymbolMTable.t -> unit
val print_term : Simple_print.SimplePrint.term -> unit
val print_terms : Simple_print.SimplePrint.term list -> unit
val print_rewrite :
  Simple_print.SimplePrint.term list -> Simple_print.SimplePrint.term -> unit
val print_rewrites :
  (Simple_print.SimplePrint.term list * Simple_print.SimplePrint.term) list ->
  unit
val print_stack : Phobos_type.stack_entry Stack.t -> unit
