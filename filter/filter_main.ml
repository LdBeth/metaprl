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
 * $Log$
 * Revision 1.3  1998/04/24 19:38:23  jyh
 * Updated debugging.
 *
 * Revision 1.2  1998/04/24 02:41:54  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/04/28 15:50:54  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
