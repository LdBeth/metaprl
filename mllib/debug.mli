(*
 * Debugging tools.
 *)

(*
 * Particular debug variables.
 *)
val debug_load : bool ref

(*
 * Info about debug variables.
 * The variables themselves are defined in the Debug module.
 *)
type debug_info =
   { debug_name : string;
     debug_description : string;
     debug_value : bool
   }

(*
 * We create named debug variables.
 *)
val create_debug : debug_info -> bool ref
val load_debug : string -> bool ref

(*
 * Operations to inspect debug flags.
 *)
val set_debug : string -> bool -> unit
val get_debug : string -> debug_info
val debuggers : unit -> debug_info array

(*
 * We allow flags to be set from the environment.
 * they may be set before the vars are created,
 * so we add them as "possible" debug flags,
 * then check them later.
 *)
val set_possible_debug : string -> bool -> unit
val check_debug : unit -> unit

(*
 * Print a list of strings.
 *)
val print_any_list : (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
val print_string_list :  out_channel -> string list -> unit
val print_int_list :  out_channel -> int list -> unit

(*
 * Flush output.
 *)
val eflush : out_channel -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
