val start_state_id : int
val global_start_string : string
val global_start_symbol : Phobos_type.psymbol
val bogus_symbol : Phobos_type.psymbol
val eof_symbol : Phobos_type.psymbol
val stack_start_state : Phobos_type.stack_entry
val bogus_pos : string * int * int * int * int
val string_add : string list -> string
val string_format : ('a, unit, string) format -> 'a
val list_mem : 'a list -> 'a -> bool
val list_add : 'a list -> 'a -> 'a list
val list_firstn : 'a list -> int -> 'a list -> 'a list
val list_first_n : 'a list -> int -> 'a list
val list_from_nth : 'a list -> int -> 'a list
val list_nth : 'a list -> int -> 'a
val array_sub : 'a array -> int -> int -> 'a array
val string_of_file : string -> string
val first_of_option : ('a * 'b) option -> 'a option
val breakup_term :
  Refiner.Refiner.TermType.term ->
  Opname.opname * Refiner.Refiner.TermType.param' list *
  Refiner.Refiner.TermType.bound_term list
val breakup_bterm :
  Refiner.Refiner.TermType.bound_term ->
  Opname.opname * Refiner.Refiner.TermType.param' list *
  Refiner.Refiner.TermType.bound_term list
val timestamp_of : string -> float
val size_of : string -> int
