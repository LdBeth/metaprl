(*
 * Test the interface filter.
 *)

declare test{'a}
declare hello{'a}

define hello : hello{'a} <--> test{'a}
rewrite hello2 : hello{'a} <--> test{'a}

axiom hello3 'H :
   sequent { 'H >- test{'a} } -->
   sequent { 'H >- hello{'a} }

infix HELLO

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:09  jyh
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
