(*
 * Null object raises exception on every operation.
 *)

include Shell_type

open Printf
open Nl_debug

open Rformat
open Refiner.Refiner.RefineError

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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
