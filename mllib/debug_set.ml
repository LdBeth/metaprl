(*
 * Commands to manipulate debug variables.
 *)

open Printf

open Debug

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
 * $Log$
 * Revision 1.6  1998/06/12 13:46:48  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.5  1998/06/01 13:54:31  jyh
 * Proving twice one is two.
 *
 * Revision 1.4  1998/05/01 14:59:30  jyh
 * Updating display forms.
 *
 * Revision 1.3  1998/04/28 18:30:26  jyh
 * ls() works, adding display.
 *
 * Revision 1.2  1998/04/24 19:38:49  jyh
 * Updated debugging.
 *
 * Revision 1.1  1998/04/24 02:42:29  jyh
 * Added more extensive debugging capabilities.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
