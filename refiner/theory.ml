(*
 * Simple recorder for all the theories in the system.
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
     thy_dformer : dform_mode_base;
     thy_id : int
   }

(*
 * Save all the theories on a list.
 *)
let base = ref ([] : theory list)

(*
 * Record a theory by pushing it onto the list.
 *)
let record_theory thy =
   Ref_util.push thy base

(*
 * Get all the theories.
 *)
let get_theories () = !base

(*
 * $Log$
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
 * Revision 1.1  1996/06/11 18:35:04  jyh
 * Demo version 0.0
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
