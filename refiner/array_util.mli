(*
 * Operations on arrays.
 *)

(* Membership in an array *)
val mem : 'a -> 'a array -> bool
val index : 'a -> 'a array -> int
val exists : ('a -> bool) -> 'a array -> bool
val find_index : 'a array -> ('a -> bool) -> int

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:14  jyh
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
