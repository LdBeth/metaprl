(*
 * Operations on references.
 *)

open Printf
open Debug
(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Ref_util%t" eflush

(*
 * Stack operations.
 *)
let push a stack =
   stack := a::!stack

let pop stack =
   match !stack with
      h::t ->
         stack := t;
         h
    | [] ->
         raise (Invalid_argument "pop")

(*
 * $Log$
 * Revision 1.2  1998/04/24 19:39:00  jyh
 * Updated debugging.
 *
 * Revision 1.1  1997/08/06 16:18:01  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:29  jyh
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
