(*
 * This file just exists to extract the code for adding and infix
 * expression.
 *)

open Pcaml

(*
 * Make an infix expression.
 *)
let make_infix loc op e1 e2 =
   let lop = "prefix_" ^ op in
      <:expr< $lid:lop$ $e1$ $e2$ >>

(*
 * Add an infix keyword.
 * This is computed in infix.ml.
 *)
let add_infix (keyword : string) =
   EXTEND
      GLOBAL: expr;

      expr: BEFORE "top" (**)
         [[ e1 = expr; op = "KEYWORD"; e2 = expr ->
             make_infix loc op e1 e2
          ]];
   END

(*
 * $Log$
 * Revision 1.2  1998/06/01 13:53:20  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1997/04/28 15:51:04  jyh
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
