val stack_top : 'a Stack.t -> 'a
val stack_push : 'a Stack.t -> 'a -> 'a Stack.t
val stack_pop : 'a Stack.t -> 'a Stack.t
val state_of_stack_symbol : Phobos_type.stack_entry -> Phobos_type.state_id
val term_of_stack_symbol : Phobos_type.stack_entry -> Phobos_type.mp_term
val stack_pop_list :
  Phobos_type.stack_entry Stack.t ->
  'a list ->
  Phobos_type.stack_entry Stack.t * Phobos_type.state_id list *
  Phobos_type.mp_term list
val stack_size : 'a Stack.t -> int
val current_state_of_stack :
  Phobos_type.stack_entry Stack.t -> Phobos_type.state_id
val result_term_of_stack :
  Phobos_type.stack_entry Stack.t -> Phobos_type.mp_term
val is_epsilon_transformation : Phobos_type.psymbol list -> bool
val is_rr_problem : Phobos_type.pentry list -> bool
val is_sr_problem :
  Phobos_type.pentry list ->
  bool * (Phobos_type.production_id * Phobos_type.state_id) option
val select_action : Phobos_type.pentry list -> Phobos_type.pentry
