val debug_string : string -> unit
val debug_int : int -> unit
val debug_list : ('a -> unit) -> string -> 'a list -> unit
val debug_tokens : string -> (string * string * Phobos_type.pos) list -> unit
val debug_token_options : Phobos_type.token_option list -> unit
val debug_token_rules :
  string ->
  ('a * ((string * 'b) * Phobos_type.token_option list) * 'c *
   ((Simple_print.SimplePrint.term * 'd) list *
    (Simple_print.SimplePrint.term * 'e))
   list)
  list -> unit
val debug_symbols :
  string -> Phobos_type.StringSet.t -> Phobos_type.StringSet.t -> unit
val debug_regexps : (bool * string * string list) list -> unit
val debug_pre_grammar :
  ((string * 'a) *
   ('b * 'c *
    ((Simple_print.SimplePrint.term * 'd) list *
     (Simple_print.SimplePrint.term * 'e))
    list))
  list -> unit
val debug_grammar :
  string ->
  (Phobos_type.psymbol * 'a * (Phobos_type.psymbol * 'b) list *
   (string * 'c) option * 'd)
  list -> unit
val debug_parser_sets :
  string ->
  Phobos_type.PSymbolSet.t ->
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t ->
  Phobos_type.PSymbolSet.t Phobos_type.PSymbolTable.t -> unit
val debug_symbol : string -> Phobos_type.psymbol -> unit
val debug_states :
  string ->
  Phobos_type.parser_env ->
  Phobos_type.state_struct list -> Phobos_type.Accepts.t -> unit
val debug_ptable :
  string ->
  Phobos_type.parser_env ->
  Phobos_type.pentry Phobos_type.ParserFA.t ->
  (Phobos_type.state_id * Phobos_type.psymbol) list -> unit
val debug_ploc_list :
  Phobos_type.parser_env ->
  Phobos_type.pentry Phobos_type.ParserFA.t ->
  (Phobos_type.state_id * Phobos_type.psymbol) list -> unit
