(*
 * Commands to manipulate debug variables.
 *)

open Printf

open Mp_debug

(*
 * Command line formatting.
 * string -> path commands
 *)
let set_path doc var path =
   List.iter (function name -> set_possible_debug name true) (String_util.split ':' path)

let set_path_arg doc var =
   Arg.String (set_path doc var)

(*
 * Start the debugger.
 *)
let _ = Env_arg.general "debug" [] "Debug flags" set_path set_path_arg

let init () =
   check_debug ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
