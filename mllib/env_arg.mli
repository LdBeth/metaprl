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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
