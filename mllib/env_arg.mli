(*
 * Environment/command line option interface to arguments.
 * Taken from ensemble/appl/cdarg.ml
 *
 *)

(*
 * Type of variable setting functions.
 *)
type 'a env_set = string -> 'a ref -> string -> unit
type 'a arg_set = string -> 'a ref -> Arg.spec
type 'a var_set = string -> 'a ref -> 'a -> unit

(*
 * Add an argument:
 *    1. name
 *    2. default value
 *    3. documentation string
 *    4. function to call when set as env variable
 *    5. function to call as command line option
 * Returns reference to variable value.
 *)
val general : string -> 'a -> string -> 'a env_set -> 'a arg_set -> 'a ref

(*
 * Special cases.
 *)
val string : string -> string -> string -> string var_set -> string ref
val int : string -> int -> string -> int var_set -> int ref
val bool : string -> bool -> string -> bool var_set -> bool ref

(*
 * Return the args to be added to the command prompt.
 *)
val args : unit -> (string * Arg.spec * string) list

(*
 * Our parser saves the spec list.
 *)
val parse : (string * Arg.spec * string) list -> (string -> unit) -> string -> unit

(*
 * $Log$
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
 * Revision 1.1  1997/02/10 16:17:48  jyh
 * Added some new files.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
