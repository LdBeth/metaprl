(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *)

open Filter_type
open Filter_summary

exception BadMagicNumber of int

type module_base_io

(* Suffix of info files *)
val info_suffix : string

(* Creation *)
val new_module_base_io : string list -> module_base_io
val set_module_base_path : module_base_io -> string list -> unit

(* Access *)
val find_module : module_base_io -> module_path -> int -> module_info
val load_module : module_base_io -> string -> module_path -> int -> module_info
val save_module : module_info -> string -> unit

(* Info *)
val module_name : module_base_io -> module_info -> string
val module_fullname : module_base_io -> module_info -> module_path
val push_module : module_base_io -> string -> module_path -> module_info -> unit

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:00  jyh
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
 * Revision 1.1  1996/09/02 19:43:18  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
