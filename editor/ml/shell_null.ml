(*
 * Null object raises exception on every operation.
 *)

include Shell_type

open Printf
open Debug

open Rformat
open Refiner.Refiner.RefineErrors

open Shell_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Shell_null%t" eflush

let null_object =
   let edit_format _ buf =
      format_string buf "There is no object to edit"
   in
   let one_error _ =
      raise (RefineError ("Shell_null.null_object", StringError "no current object"))
   in
   let three_errors _ _ _ =
      raise (RefineError ("Shell_null.null_object", StringError "no current object"))
   in
      { edit_format = edit_format;
        edit_set_goal = one_error;
        edit_set_redex = one_error;
        edit_set_contractum = one_error;
        edit_set_assumptions = one_error;
        edit_set_params = one_error;
        edit_save = one_error;
        edit_check = one_error;
        edit_expand = one_error;
        edit_root = one_error;
        edit_up = one_error;
        edit_down = one_error;
        edit_refine = three_errors;
        edit_undo = one_error;
        edit_fold = one_error;
        edit_fold_all = one_error
      }

(*
 * $Log$
 * Revision 1.6  1998/07/01 04:36:31  nogin
 * Moved Refiner exceptions into a separate module RefineErrors
 *
 * Revision 1.5  1998/06/15 22:31:51  jyh
 * Added CZF.
 *
 * Revision 1.4  1998/06/12 13:45:15  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.3  1998/05/28 13:45:59  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.2  1998/04/24 02:41:36  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1998/04/23 20:04:07  jyh
 * Initial rebuilt editor.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
