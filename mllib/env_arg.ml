(*
 * This is taken from ensemble/apply/harg.ml
 * Handles options as command line options
 * or environment variables.
 *
 *)

open Printf
open Mp_debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Env_arg%t" eflush

(*
 * Type of variable setting functions.
 *)
type 'a env_set = string -> 'a ref -> string -> unit
type 'a arg_set = string -> 'a ref -> Arg.spec
type ('a, 'b) var_set = string -> 'a ref -> 'b -> unit

(*
 * Environment variables are prefixed with this string.
 *)
let environ_prefix = "MP_"

(*
 * Master list of args.
 *)
let param_args = ref []

(*
 * Call a function if an environment variable is set.
 *)
let if_getenv key f =
   let key = String.uppercase key in
   let key = environ_prefix ^ key in
      try f (Sys.getenv key) with
         Not_found -> ()

(*
 * General case argument manager.  All the others use this.
 *)
let general name default info env_set arg_set =
   let v = ref default in
      (* Check the environment variable *)
      if_getenv name (fun s -> env_set name v s);

      (* Then add the command-line argument *)
      let com_set = arg_set name v in
      let com_name = "-" ^ name in
      let com_info = ": " ^ info in
         param_args := (com_name, com_set, com_info) :: !param_args;

         (* Return ref to the value *)
         v

(*
 * Special cases, where argument is of type string.
 * Handle string-based args.
 *)
let string name default info set =
   let env_set = set in
   let arg_set name v =
      let helper s = set name v s in
         Arg.String helper
   in
      general name default info env_set arg_set

(*
 * Handle int-based args.
 *)
let int name default info set =
   let env_set name v s =
      let i =
         try int_of_string s with
            Failure "int_of_string" ->
               eprintf "CDARG: bad value for %s: '%s' not integer, exiting\n" name s ;
               exit 1
      in
         set name v i
   in

   let arg_set name v =
      let helper i = set name v i in
         Arg.Int helper
   in
      general name default info env_set arg_set

(*
 * Handle boolean args.
 *)
let bool name default info set =
   let env_set name v s =
      let flag =
      match s with
         "true" | "TRUE" ->
            true
       | "false" | "FALSE" ->
            false
       | _ ->
            eprintf "CDARG: bad value for %s: '%s' not boolean, exiting\n" name s;
            exit 1
      in
         set name v flag
   in
   let arg_set name v =
      if default then
         Arg.Clear v
      else
         Arg.Set v
   in
      general name default info env_set arg_set

(*
 * Standard variable setting functions.
 *)
let set_string_string _ cell s =
   cell := s

let set_string_option_string _ cell s =
   cell := Some s

let set_int_int _ cell i =
   cell := i

let set_int_option_int _ cell i =
   cell := Some i

let set_bool_bool _ cell b =
   cell := b

(*
 * Return arguments to be passed to the parser.
 *)
let args () = !param_args

(*
 * Parse the arguments.
 *)
let parse spec usage errmsg =
   Arg.parse (!param_args @ spec) usage errmsg

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
