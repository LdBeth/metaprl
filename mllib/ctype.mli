(*
 * Some character type operations.
 *)

(* Case conversion *)
val toupper : char -> char
val tolower : char -> char

(* Digits *)
val is_digit : char -> bool

(* Capital letter *)
val is_upperchar : char -> bool
val is_lowerchar : char -> bool

(* String operations *)
val is_capitalized : string -> bool
val is_uppercase : string -> bool
val is_lowercase : string -> bool

(*
 * $Log$
 * Revision 1.2  1998/06/01 13:54:29  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1997/08/06 16:17:51  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:15  jyh
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
