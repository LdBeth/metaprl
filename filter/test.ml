(*
 * Test some terms.
 *)

let t = << sequent [] { x: 'A; y: 'B['x]; z: 'C['x; 'y] >- 'D['x; 'y; 'z] } >>

(*
 * $Log$
 * Revision 1.2  1998/04/06 21:39:54  jyh
 * Test program with a sequent.
 *
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
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
