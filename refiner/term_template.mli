(*
 * This module computes term templates for use by hashtables.
 *
 * $Log$
 * Revision 1.2  1998/04/29 14:48:35  jyh
 * Added ocaml_sos.
 *
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

type t

(* Compute templates from terms *)
val of_term : term -> t
val of_term_list : term list -> t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
