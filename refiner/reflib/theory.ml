(*
 * Simple recorder for all the theories in the system.
 *
 *)

open Printf
open Debug

open Refiner.Refiner
open Refiner.Refiner.Refine
open Dform_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Theory%t" eflush

(*
 * Info is saved in this format.
 *)
type theory =
   { thy_name : string;
     thy_refiner : refiner;
     thy_dformer : dform_mode_base
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
let get_theories () =
   !base

(*
 * Get the parents of a theory.
 *)
let get_parents thy =
   let rec find_parent parent = function
      thy :: tl ->
         let { thy_refiner = refiner } = thy in
            if refiner == parent then
               thy
            else
               find_parent parent tl
    | [] ->
         raise Not_found
   in
   let rec search refiner =
      if is_null_refiner refiner then
         []
      else
         let item, refiner' = dest_refiner refiner in
            match item with
               RIParent parent ->
                  begin
                     try find_parent parent !base :: search refiner' with
                        Not_found ->
                           search refiner'
                  end
             | _ ->
                  search refiner'
   in
   let { thy_refiner = refiner } = thy in
      search refiner

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:01:29  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.5  1998/05/27 15:15:12  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.4  1998/04/28 18:30:52  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/04/24 02:43:08  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/02/23 14:46:42  jyh
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
 * Revision 1.1  1996/06/11 18:35:04  jyh
 * Demo version 0.0
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
