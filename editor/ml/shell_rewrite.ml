(*
 * Create an ediable rewrite object.
 *)

include Shell_type
include Package_info
include Package_df

open Printf
open Debug

open Rformat
open Dform
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.Refine
open Opname

open Filter_summary
open Filter_cache
open Filter_ocaml

open Tactic_type
open Tactic_cache
open Shell_type
open Package_info

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
     mutable rw_ped : Proof_edit.t proof_type
   }

(*
 * Make a rewrite goal from the assumptions,
 * and the rewrite.
 *)
let seq = << sequent { 'H >- 'rw } >>

let mk_goal arg redex contractum =
   let rw = replace_goal seq (mk_xrewrite_term redex contractum) in
      create_arg { mseq_goal = rw; mseq_hyps = [] } arg

let mk_cond_goal arg assums redex contractum =
   let rw = replace_goal seq (mk_xrewrite_term redex contractum) in
   let assums = List.map (replace_goal seq) assums in
      create_arg { mseq_goal = rw; mseq_hyps = assums } arg

let mk_rw_goal arg assums redex contractum =
   if assums = [] then
      mk_goal arg redex contractum
   else
      mk_cond_goal arg assums redex contractum

let mk_ped arg params assums redex contractum =
   Proof_edit.create params (mk_rw_goal arg assums redex contractum)

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Commenting function.
 *)
let comment loc t =
   t

(*
 * Format the tactic text.
 *)
let format_tac db buf arg =
   let { mseq_goal = rw; mseq_hyps = hyps } = tactic_seq arg in
   let format_hyp hyp =
      format_term db buf hyp;
      format_newline buf
   in
      format_string buf "Hyps:";
      format_newline buf;
      List.iter format_hyp hyps;
      format_string buf "Goal: ";
      format_term db buf rw;
      format_newline buf

(*
 * A primtive rewrite.
 *)
let format_prim_rewrite db buf arg params assums redex contractum =
   let tac = mk_rw_goal arg assums redex contractum in
      format_tac db buf tac;
      format_newline buf;
      format_string buf "Primitive";
      format_newline buf

let format_cond_rewrite db buf arg params assums redex contractum expr =
   let tac = mk_rw_goal arg assums redex contractum in
      format_tac db buf tac;
      format_newline buf;
      format_string buf "Derived> ";
      format_term db buf (term_of_expr [] comment expr);
      format_newline buf

(*
 * The object has a package in scope.
 *)
let unit_term = mk_simple_term nil_opname []

let edit pack arg name obj =
   let update_ped () =
      obj.rw_ped <- Primitive unit_term
   in
   let edit_format db buf =
      (* Convert to a term *)
      let { rw_params = params;
            rw_assums = assums;
            rw_redex = redex;
            rw_contractum = contractum;
            rw_ped = ped
          } = obj
      in
         match ped with
            Primitive t ->
               format_prim_rewrite db buf arg params assums redex contractum
          | Derived expr ->
               format_cond_rewrite db buf arg params assums redex contractum expr
          | Interactive ped ->
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
   let edit_check () =
      match obj.rw_ped with
         Primitive _ ->
            raise (RefineError (StringError "Shell_rewrite.check: can't check primitive rules"))
       | Derived _ ->
            raise (RefineError (StringError "Shell_rewrite.check: can't check noninteractive proofs"))
       | Interactive ped ->
            try Proof_edit.check_ped ped with
               RefineError err ->
                  raise (RefineError (GoalError (name, err)))
   in
   let get_ped obj =
      match obj.rw_ped with
         Primitive _
       | Derived _ ->
            raise (RefineError (StringError "Shell_rewrite: proof is not interactive"))
       | Interactive ped ->
            ped
   in
   let edit_expand df =
      Proof_edit.expand_ped df (get_ped obj)
   in
   let edit_root () =
      Proof_edit.root_ped (get_ped obj)
   in
   let edit_up () =
      Proof_edit.up_ped (get_ped obj)
   in
   let edit_down i =
      Proof_edit.down_ped (get_ped obj) i
   in
   let edit_undo () =
      Proof_edit.undo_ped (get_ped obj)
   in
   let edit_nop () =
      Proof_edit.nop_ped (get_ped obj)
   in
   let edit_fold () =
      Proof_edit.fold_ped (get_ped obj)
   in
   let edit_fold_all () =
      Proof_edit.fold_all_ped (get_ped obj)
   in
   let edit_refine text ast tac =
      let ped =
         match obj.rw_ped with
            Primitive _
          | Derived _ ->
               (* Convert to a ped *)
               let { rw_params = params;
                     rw_assums = assums;
                     rw_redex = redex;
                     rw_contractum = contractum
                   } = obj
               in
               let ped = mk_ped arg params assums redex contractum in
                  obj.rw_ped <- Interactive ped;
                  ped
          | Interactive ped ->
               ped
      in
         Proof_edit.refine_ped ped text ast tac
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

let create pack name =
   let obj =
      { rw_assums = [];
        rw_params = [];
        rw_redex = unit_term;
        rw_contractum = unit_term;
        rw_ped = Primitive unit_term
      }
   in
   let arg = Package.argument pack in
      edit pack arg name obj

let ped_of_proof pack = function
   Primitive proof ->
      Primitive proof
 | Derived expr ->
      Derived expr
 | Interactive proof ->
      Interactive (Package.ped_of_proof pack proof)

let view_rw pack
    { Filter_summary.rw_name = name;
      Filter_summary.rw_redex = redex;
      Filter_summary.rw_contractum = contractum;
      Filter_summary.rw_proof = proof
    } =
   let obj =
      { rw_assums = [];
        rw_params = [];
        rw_redex = redex;
        rw_contractum = contractum;
        rw_ped = ped_of_proof pack proof
      }
   in
   let arg = Package.argument pack in
      edit pack arg name obj

let view_crw pack
    { crw_name = name;
      crw_params = params;
      crw_args = args;
      crw_redex = redex;
      crw_contractum = contractum;
      crw_proof = proof
    } =
   let obj =
      { rw_assums = args;
        rw_params = params;
        rw_redex = redex;
        rw_contractum = contractum;
        rw_ped = ped_of_proof pack proof
      }
   in
   let arg = Package.argument pack in
      edit pack arg name obj

(*
 * $Log$
 * Revision 1.11  1998/06/03 22:19:13  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.10  1998/06/01 13:52:33  jyh
 * Proving twice one is two.
 *
 * Revision 1.9  1998/05/29 14:52:53  jyh
 * Better Makefiles.
 *
 * Revision 1.8  1998/05/28 13:46:02  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.7  1998/05/07 16:02:26  jyh
 * Adding interactive proofs.
 *
 * Revision 1.6  1998/04/28 18:29:58  jyh
 * ls() works, adding display.
 *
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
