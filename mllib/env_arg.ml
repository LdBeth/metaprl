(*
 * This is taken from ensemble/apply/harg.ml
 * Handles options as command line options
 * or environment variables.
 *
 *)

open Printf
open Lm_debug

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Env_arg%t"

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
               eprintf "Env_arg: bad value for %s: '%s' not integer, exiting\n" name s ;
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
            eprintf "Env_arg: bad value for %s: '%s' not boolean, exiting\n" name s;
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
let usage spec errmsg =
   Arg.usage (!param_args @ spec) errmsg;
   debug_usage ()

let parse spec def errmsg =
   Arg.parse (!param_args @ spec) def errmsg

(*
 * Set debug flags from the environment.
 *)
let set_possible_debug_flags _ _ flags =
   List.iter (fun name -> set_possible_debug name true) (Lm_string_util.split ":" flags)

let set_debug_flags _ _ =
   let set flags =
      let names = Lm_string_util.split ":" flags in
      try List.iter (fun name -> set_debug name true) names with
         Failure _ ->
            usage [] "illegal option: valid options are:";
            exit 1
      in
         Arg.String set

let _ = general "debug" () "set debugging flags" set_possible_debug_flags set_debug_flags

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
