(*
 * This is taken from ensemble/apply/harg.ml
 * Handles options as command line options
 * or environment variables.
 *
 *)

open Printf
open Debug

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
type 'a var_set = string -> 'a ref -> 'a -> unit

(*
 * Environment variables are prefixed with this string.
 *)
let environ_prefix = "NL_"

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
 * Return arguments to be passed to the parser.
 *)
let args () = !param_args

(*
 * Parse the arguments.
 *)
let parse spec usage errmsg =
   Arg.parse (!param_args @ spec) usage errmsg

(*
 * $Log$
 * Revision 1.5  1998/06/01 13:54:33  jyh
 * Proving twice one is two.
 *
 * Revision 1.4  1998/04/24 19:38:50  jyh
 * Updated debugging.
 *
 * Revision 1.3  1998/02/18 18:46:42  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.2  1997/11/12 22:28:17  jyh
 * Small changes for NT.
 *
 * Revision 1.1  1997/08/06 16:17:53  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:19  jyh
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
 * Revision 1.2  1997/02/18 02:54:50  jyh
 * Added radios, initial compilations.
 * This is a working version in the old style.
 * Converting ejb_file to work with radios.
 *
 * Revision 1.1  1997/02/10 16:17:48  jyh
 * Added some new files.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
