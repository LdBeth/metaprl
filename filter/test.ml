(*
 * Test some terms.
 *)

let t0 = << x{1} >>
let t1 = << x[1]{a, b{a, 2}} >>
let t2 = << -x -> -y + 1 >>

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
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
