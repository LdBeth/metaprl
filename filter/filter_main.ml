(*
 * Main function collects arguments, and starts parsing.
 *)

open Arg
open Printf
open Debug

open Filter_parse

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_main%t" eflush


(*
 * string -> path commands
 *)
let set_path doc var path =
   let path' = String_util.split ':' path in
      var := path'

let set_path_arg doc var =
   Arg.String (set_path doc var)

(*
 * This is a list of hosts to use for database lookup.
 *)
let include_path = Env_arg.general "include" ["."] "Include directories" set_path set_path_arg

(*
 * Add the include path.
 *)
let _ = set_include_path !include_path

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
