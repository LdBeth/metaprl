(*
 * This module computes term templates for use by hashtables.
 *
 * $Log$
 * Revision 1.1  1997/04/28 15:51:48  jyh
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
 * Revision 1.1  1996/11/13 22:59:22  jyh
 * Initial version of forward/backward chaining cache.
 *
 *)

open Opname
open Term

type term_template

(* Compute templates from terms *)
val compute_template : term -> term_template

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
