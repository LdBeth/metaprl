(*
 * Simple mechanism for recording refiners and display forms.
 *
 *)

open Refine
open Dform_print

(*
 * Info is saved in this format.
 *)
type theory =
   { thy_name : string;
     thy_refiner : Refiner.refiner;
     thy_dformer : dform_mode_base
   }

(* Save the theory *)
val record_theory : theory -> unit

(* Get back all the theories that have been recorded *)
val get_theories : unit -> theory list

(*
 * Get all the parent theories for a theory.
 * This is not guaranteed to be accurate.
 *)
val get_parents : theory -> theory list

(*
 * $Log$
 * Revision 1.3  1998/04/28 18:30:53  jyh
 * ls() works, adding display.
 *
 * Revision 1.2  1998/02/23 14:46:44  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1997/04/28 15:51:50  jyh
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
 * Revision 1.2  1996/09/02 19:43:32  jyh
 * Semi working package management.
 *
 * Revision 1.1  1996/06/11 18:35:05  jyh
 * Demo version 0.0
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
