(*
 * Reference operations.
 *)

(* Stack ops *)
val push : 'a -> ('a list) ref -> unit
val pop : ('a list) ref -> 'a

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:30  jyh
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
