(*
 * Magic numbers for interactive files.
 *)

val int_term_sig_magic : int
val int_raw_sig_magic  : int
val int_term_str_magic : int
val int_raw_str_magic  : int
val int_lib_sig_magic  : int
val int_lib_str_magic  : int
val interactive_magics : int list

(*
 * This is a test.
 * The name is the filename without the suffix.
 *)
val file_interactive : string -> bool

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
