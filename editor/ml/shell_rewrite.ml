(*
 * Create an ediable rewrite object.
 *)

include Shell_type
include Package_info
include Package_df
include Proof_type

open Printf
open Nl_debug

open Rformat
open Dform
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Opname

open Filter_summary
open Filter_cache
open Filter_ocaml

open Tacticals
open Tactic_cache
open Shell_type
open Package_info
open Proof_type

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
   { mutable rw_params : term param list;
     mutable rw_assums : term list;
     mutable rw_redex : term;
     mutable rw_contractum : term;
     rw_proof : Package.proof proof_type;
     mutable rw_ped : Proof_edit.t proof_type
   }

(*
 * Make a rewrite goal from the assumptions,
 * and the rewrite.
 *)
let seq = << sequent ['ext] { 'H >- 'rw } >>

let mk_goal sentinal arg redex contractum =
   let rw = TermMan.replace_goal seq (mk_xrewrite_term redex contractum) in
   let { ref_label = label; ref_fcache = cache; ref_args = args } = arg in
      Sequent.create sentinal label (mk_msequent rw []) cache args

let mk_cond_goal sentinal arg assums redex contractum =
   let rw = replace_goal seq (mk_xrewrite_term redex contractum) in
   let assums = List.map (replace_goal seq) assums in
   let { ref_label = label; ref_fcache = cache; ref_args = args } = arg in
      Sequent.create sentinal label (mk_msequent rw assums) cache args

let mk_rw_goal sentinal arg assums redex contractum =
   if assums = [] then
      mk_goal sentinal arg redex contractum
   else
      mk_cond_goal sentinal arg assums redex contractum

let mk_ped arg sentinal params assums redex contractum =
   Proof_edit.create params (mk_rw_goal sentinal arg assums redex contractum)

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Commenting function.
 *)
let comment _ _ t =
   t

(*
 * Format the tactic text.
 *)
let format_tac db buf arg =
   let seq = Sequent.msequent arg in
   let rw, hyps = dest_msequent seq in
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
let format_rewrite_aux s db buf arg sentinal params assums redex contractum =
   let tac = Refine_exn.print db (mk_rw_goal sentinal arg assums redex) contractum in
      format_tac db buf tac;
      format_newline buf;
      format_string buf s;
      format_newline buf

let format_prim_rewrite = format_rewrite_aux "Primitive"
let format_incomplete_rewrite = format_rewrite_aux "Incomplete"

let format_cond_rewrite db buf arg sentinal params assums redex contractum expr =
   let tac = Refine_exn.print db (mk_rw_goal sentinal arg assums redex) contractum in
      format_tac db buf tac;
      format_newline buf;
      format_string buf "Derived> ";
      format_term db buf (term_of_expr [] comment expr);
      format_newline buf

(*
 * Build an item from the object.
 *)
let item_of_obj pack name
    { rw_params = params;
      rw_assums = assums;
      rw_redex = redex;
      rw_contractum = contractum;
      rw_proof = proof
    } ped =
   if params = [] & assums = [] then
      Filter_summary.Rewrite (**)
         { Filter_summary.rw_name = name;
           Filter_summary.rw_redex = redex;
           Filter_summary.rw_contractum = contractum;
           Filter_summary.rw_proof = proof
         }
   else
      Filter_summary.CondRewrite (**)
         { Filter_summary.crw_name = name;
           Filter_summary.crw_params = params;
           Filter_summary.crw_args = assums;
           Filter_summary.crw_redex = redex;
           Filter_summary.crw_contractum = contractum;
           Filter_summary.crw_proof = proof
         }

(*
 * The object has a package in scope.
 *)
let unit_term = mk_simple_term nil_opname []

let edit pack sentinal arg name obj =
   let update_ped () =
      obj.rw_ped <- Primitive unit_term
   in
   let save_ped ped =
      let item = item_of_obj pack name obj ped in
         Package.set pack item;
         obj.rw_ped <- Interactive ped
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
               format_prim_rewrite db buf arg sentinal params assums redex contractum
          | Derived expr ->
               format_cond_rewrite db buf arg sentinal params assums redex contractum expr
          | Incomplete ->
               format_incomplete_rewrite db buf arg sentinal params assums redex contractum
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
   let edit_save () =
      match obj.rw_ped with
         Interactive ped ->
            save_ped ped
       | Incomplete
       | Primitive _
       | Derived _ ->
            ()
   in
   let edit_check () =
      match obj.rw_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rewrite.check", StringError "can't check primitive rules"))
       | Derived _ ->
            raise (RefineError ("Shell_rewrite.check", StringError "can't check noninteractive proofs"))
       | Incomplete ->
            raise (RefineError ("Shell_rewrite.check", StringError "proof is incomplete"))
       | Interactive ped ->
            try Proof_edit.check_ped ped with
               RefineError (name', err) ->
                  raise (RefineError (name, GoalError (name', err)))
   in
   let get_ped obj =
      match obj.rw_ped with
         Primitive _
       | Derived _
       | Incomplete ->
            raise (RefineError ("Shell_rewrite.get_ped", StringError "proof is not interactive"))
       | Interactive ped ->
            ped
   in
   let edit_expand df =
      Proof_edit.expand_ped df (get_ped obj)
   in
   let edit_root () =
      Proof_edit.root_ped (get_ped obj)
   in
   let edit_up i =
      Proof_edit.up_ped (get_ped obj) i
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
   let get_ped () =
      match obj.rw_ped with
         Primitive _
       | Derived _
       | Incomplete ->
               (* Convert to a ped *)
            let { rw_params = params;
                  rw_assums = assums;
                  rw_redex = redex;
                  rw_contractum = contractum
                } = obj
            in
            let ped = mk_ped arg sentinal params assums redex contractum in
               save_ped ped;
               ped
       | Interactive ped ->
            ped
   in
   let edit_goal () =
      Proof_edit.ped_arg (get_ped ())
   in
   let edit_refine text ast tac =
      Proof_edit.refine_ped (get_ped ()) text ast tac
   in
      { edit_format = edit_format;
        edit_set_goal = edit_set_goal;
        edit_set_redex = edit_set_redex;
        edit_set_contractum = edit_set_contractum;
        edit_set_assumptions = edit_set_assumptions;
        edit_set_params = edit_set_params;
        edit_save = edit_save;
        edit_check = edit_check;
        edit_expand = edit_expand;
        edit_root = edit_root;
        edit_up = edit_up;
        edit_down = edit_down;
        edit_goal = edit_goal;
        edit_refine = edit_refine;
        edit_undo = edit_undo;
        edit_fold = edit_fold;
        edit_fold_all = edit_fold_all
      }

let create pack name =
   let proof = Package.new_proof pack name [] unit_term in
   let ped = Package.ped_of_proof pack proof in
   let rw =
      { Filter_summary.rw_name = name;
        Filter_summary.rw_redex = unit_term;
        Filter_summary.rw_contractum = unit_term;
        Filter_summary.rw_proof = Interactive proof
      }
   in
   let obj =
      { rw_assums = [];
        rw_params = [];
        rw_redex = unit_term;
        rw_contractum = unit_term;
        rw_proof = Interactive proof;
        rw_ped = Interactive ped
      }
   in
   let sentinal =
      let refiner =
         try Package.refiner pack with
            Not_found ->
               raise (RefineError ("create_rw", StringStringError ("no refiner", name)))
      in
         sentinal_of_refiner refiner
   in
   let arg = Package.argument pack in
      Package.set pack (Filter_summary.Rewrite rw);
      edit pack sentinal arg name obj

let ped_of_proof pack = function
   Primitive proof ->
      Primitive proof
 | Derived expr ->
      Derived expr
 | Incomplete ->
      Incomplete
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
        rw_proof = proof;
        rw_ped = ped_of_proof pack proof
      }
   in
   let sentinal =
      let refiner =
         try find_refiner (Package.refiner pack) name with
            Not_found ->
               raise (RefineError ("view_rw", StringStringError ("no refiner", name)))
      in
         sentinal_of_refiner refiner
   in
   let arg = Package.argument pack in
      edit pack sentinal arg name obj

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
        rw_proof = proof;
        rw_ped = ped_of_proof pack proof
      }
   in
   let sentinal =
      let refiner =
         try find_refiner (Package.refiner pack) name with
            Not_found ->
               raise (RefineError ("view_rw", StringStringError ("no refiner", name)))
      in
         sentinal_of_refiner refiner
   in
   let arg = Package.argument pack in
      edit pack sentinal arg name obj

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
