(*
 * Test some terms.
 *)

infix HELLO

declare test{'a}
declare hello{'a}

primrw hello : hello{'a} <--> test{'a}
rwthm hello2 : (hello{'a} <--> test{'a}) = hello

prim hello3 'H : sequent { 'H >- test{'a} } : sequent { 'H >- hello{'a} } =
   hello;;

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:08  jyh
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
 * Caml-master: "camlp4"
 * End:
 * -*-
 *)
