(*
 * Extra operations on strings.
 *)

(************************************************************************
 * STRINGS                                                              *
 ************************************************************************)

(*
 * Find a char in a string.
 *)
val strchr : string -> char -> int

(*
 * Mapping.
 *)
val for_all : (char -> bool) -> string -> bool

(*
 * Membership.
 *)
val mem : char -> string -> bool

(*
 * Get the index of any char in the set.
 *)
val index_set : string -> string -> int
val rindex_set : string -> string -> int

(*
 * Split at a char.
 *)
val split : char -> string -> string list
val split_set : string -> string -> string list

(*
 * Cat strings together.
 *)
val concat : string -> string list -> string

(*
 * $Log$
 * Revision 1.2  1998/02/23 14:46:39  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1997/08/06 16:18:02  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:41  jyh
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
