(*
 * Common utilities for filtering modules.
 *
 *)

open Opname
open Term
open Term_util
open Filter_type

(************************************************************************
 * UTILITIES								*
 ************************************************************************)

val context_vars_list : term list -> string list
val binding_vars_list : term list -> string list
val unzip_rewrite : string -> meta_term -> term list * term * term

(************************************************************************
 * OPNAMES								*
 ************************************************************************)

val string_of_opname_list : string list -> string
val translate_opname : opname -> string

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:02  jyh
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
 * Revision 1.1  1996/09/02 19:43:08  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
