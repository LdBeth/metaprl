(*
 * Create an ediable rewrite object.
 *)

include Shell_type
include Package_info
include Package_df

open Printf
open Debug

open Term
open Opname
open Refine_sig

open Filter_summary

open Tactic_type
open Tactic_cache
open Shell_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Shell_rewrite%t" eflush

(*
 * This is the actual rewrite object.
 *)
type rw =
   { mutable rw_params : param list;
     mutable rw_assums : term list;
     mutable rw_redex : term;
     mutable rw_contractum : term;
     mutable rw_ped : Proof_edit.t
   }

(*
 * Make a rewrite goal from the assumptions,
 * and the rewrite.
 *)
let seq = << sequent { 'H >- 'rw } >>

let mk_goal resources redex contractum =
   let rw = replace_goal seq (mk_xrewrite_term redex contractum) in
   let arg =
      { ref_label = "main";
        ref_args = [];
        ref_fcache = extract (new_cache ());    (* BUG: this should be fixed *)
        ref_rsrc = resources
      }
   in
      { tac_goal = rw;
        tac_hyps = [];
        tac_arg = arg
      }

let mk_cond_goal resources assums redex contractum =
   let rw = replace_goal seq (mk_xrewrite_term redex contractum) in
   let assums = List.map (replace_goal seq) assums in
   let arg =
      { ref_label = "main";
        ref_args = [];
        ref_fcache = extract (new_cache ());
        ref_rsrc = resources
      }
   in
      { tac_goal = rw;
        tac_hyps = assums;
        tac_arg = arg
      }

let mk_ped resources params assums redex contractum =
   let goal =
      if assums = [] then
         mk_goal resources redex contractum
      else
         mk_cond_goal resources assums redex contractum
   in
      Proof_edit.create params goal
   
(*
 * The object has a package in scope.
 *)
let unit_term = mk_simple_term nil_opname []

let create pack prog resources name =
   let obj =
      { rw_assums = [];
        rw_params = [];
        rw_redex = unit_term;
        rw_contractum = unit_term;
        rw_ped = mk_ped resources [] [] unit_term unit_term
      }
   in
   let update_ped () =
      let { rw_params = params;
            rw_assums = assums;
            rw_redex = redex;
            rw_contractum = contractum
          } = obj
      in
         obj.rw_ped <- mk_ped resources params assums redex contractum;
   in
   let extractf () =
      let { rw_ped = ped } = obj in
         try Proof_edit.check_ped ped with
            RefineError err ->
               raise (RefineError (GoalError (name, err)))
   in
   let edit_format db buf =
      (* Convert to a term *)
      let { rw_ped = ped } = obj in
         Proof_edit.format db buf ped
   in
   let edit_set_goal t =
      raise (Failure "Shell_rewrite.edit_set_goal: bogus edit")
   in
   let edit_set_redex t =
      obj.rw_redex <- t;
      update_ped ()
   in
   let edit_set_contractum t =
      obj.rw_contractum <- t;
      update_ped ()
   in
   let edit_set_assumptions tl =
      obj.rw_assums <- tl;
      update_ped ()
   in
   let edit_set_params pl =
      obj.rw_params <- pl;
      update_ped ()
   in
   let edit_check = extractf in
   let edit_expand () =
      Proof_edit.expand_ped obj.rw_ped
   in
   let edit_root () =
      Proof_edit.root_ped obj.rw_ped
   in
   let edit_up () =
      Proof_edit.up_ped obj.rw_ped
   in
   let edit_down i =
      Proof_edit.down_ped obj.rw_ped i
   in
   let edit_refine text ast tac =
      Proof_edit.refine_ped obj.rw_ped text ast tac
   in
   let edit_undo () =
      Proof_edit.undo_ped obj.rw_ped
   in
   let edit_nop () =
      Proof_edit.nop_ped obj.rw_ped
   in
   let edit_fold () =
      Proof_edit.fold_ped obj.rw_ped
   in
   let edit_fold_all () =
      Proof_edit.fold_all_ped obj.rw_ped
   in
      { edit_format = edit_format;
        edit_set_goal = edit_set_goal;
        edit_set_redex = edit_set_redex;
        edit_set_contractum = edit_set_contractum;
        edit_set_assumptions = edit_set_assumptions;
        edit_set_params = edit_set_params;
        edit_check = edit_check;
        edit_expand = edit_expand;
        edit_root = edit_root;
        edit_up = edit_up;
        edit_down = edit_down;
        edit_refine = edit_refine;
        edit_undo = edit_undo;
        edit_fold = edit_fold;
        edit_fold_all = edit_fold_all
      }

(*
 * $Log$
 * Revision 1.5  1998/04/24 02:41:38  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.4  1998/04/23 20:04:20  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.3  1998/04/22 14:06:28  jyh
 * Implementing proof editor.
 *
 * Revision 1.2  1998/04/21 20:57:58  jyh
 * Fixed typing problems introduced by refiner msequents.
 *
 * Revision 1.1  1998/04/17 20:48:16  jyh
 * Updating refiner for extraction.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
