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
 * Split at a char.
 *)
val split : char -> string -> string list

(*
 * $Log$
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
