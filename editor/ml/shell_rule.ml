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
open Refiner.Refiner.TermMeta
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
      eprintf "Loading Shell_rule%t" eflush

let debug_refine = load_debug "refine"

(*
 * This is the actual rewrite object.
 *)
type info =
   { mutable rule_params : term param list;
     mutable rule_assums : term list;
     mutable rule_goal : term;
     mutable rule_proof : Package.proof proof_type;
     mutable rule_ped : Proof_edit.t proof_type
   }

(*
 * Make a rewrite goal from the assumptions,
 * and the rewrite.
 *)
let mk_rule_goal sentinal arg assums goal =
   let { ref_label = label; ref_fcache = cache; ref_args = args } = arg in
      Sequent.create sentinal label (mk_msequent goal assums) cache args

let mk_ped sentinal arg params assums goal =
   Proof_edit.create params (mk_rule_goal sentinal arg assums goal)

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
let format_rule_aux s db buf sentinal arg params assums goal =
   let tac = Refine_exn.print db (mk_rule_goal sentinal arg assums) goal in
      format_tac db buf tac;
      format_newline buf;
      format_string buf s;
      format_newline buf

let format_prim_rule = format_rule_aux "Primitive"
let format_incomplete_rule = format_rule_aux "Incomplete"

let format_cond_rule db buf sentinal arg params assums goal expr =
   let tac = Refine_exn.print db (mk_rule_goal sentinal arg assums) goal in
      format_tac db buf tac;
      format_newline buf;
      format_string buf "Derived> ";
      format_term db buf (term_of_expr [] comment expr);
      format_newline buf

(*
 * Build an item from the object.
 *)
let item_of_obj pack name
    { rule_params = params;
      rule_assums = assums;
      rule_proof = proof;
      rule_ped = ped;
      rule_goal = goal
    } =
   if params = [] & assums = [] then
      Filter_summary.Axiom (**)
         { Filter_summary.axiom_name = name;
           Filter_summary.axiom_stmt = goal;
           Filter_summary.axiom_proof = proof
         }
   else
      Filter_summary.Rule (**)
         { Filter_summary.rule_name = name;
           Filter_summary.rule_params = params;
           Filter_summary.rule_stmt = zip_mimplies (assums @ [goal]);
           Filter_summary.rule_proof = proof
         }

(*
 * The object has a package in scope.
 *)
let unit_term = mk_simple_term (make_opname ["unit"]) []

let edit pack sentinal arg name obj =
   let update_ped () =
      obj.rule_ped <- Primitive unit_term
   in
   let save_ped () =
      let item = item_of_obj pack name obj in
         Package.set pack item
   in
   let edit_format db buf =
      (* Convert to a term *)
      let { rule_params = params;
            rule_assums = assums;
            rule_goal = goal;
            rule_ped = ped
          } = obj
      in
         match ped with
            Primitive t ->
               format_prim_rule db buf sentinal arg params assums goal
          | Derived expr ->
               format_cond_rule db buf sentinal arg params assums goal expr
          | Incomplete ->
               format_incomplete_rule db buf sentinal arg params assums goal
          | Interactive ped ->
               Proof_edit.format db buf ped
   in
   let edit_set_goal t =
      obj.rule_goal <- t;
      update_ped ()
   in
   let edit_set_redex t =
      raise (RefineError ("Shell_rule.set_redex", StringError "can't set redex in rules"))
   in
   let edit_set_contractum t =
      raise (RefineError ("Shell_rule.set_contractum", StringError "can't set contractum in rules"))
   in
   let edit_set_assumptions tl =
      obj.rule_assums <- tl;
      update_ped ()
   in
   let edit_set_params pl =
      obj.rule_params <- pl;
      update_ped ()
   in
   let edit_save () =
      save_ped ()
(*
      match obj.rule_ped with
         Interactive ped ->
            save_ped ()
       | Primitive _
       | Derived _
       | Incomplete ->
            ()
*)
   in
   let edit_check () =
      match obj.rule_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check primitive rules"))
       | Derived _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check noninteractive proofs"))
       | Incomplete ->
            raise (RefineError ("Shell_rule.check", StringError "proof is incomplete"))
       | Interactive ped ->
            try Proof_edit.check_ped ped with
               RefineError (name', err) ->
                  raise (RefineError (name, GoalError (name', err)))
   in
   let get_ped obj =
      match obj.rule_ped with
         Primitive _
       | Derived _
       | Incomplete ->
            raise (RefineError ("Shell_rule.get_ped", StringError "proof is not interactive"))
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
      match obj.rule_ped with
         Primitive _
       | Derived _
       | Incomplete ->
               (* Convert to a ped *)
            let { rule_params = params;
                  rule_assums = assums;
                  rule_goal = goal
                } = obj
            in
            let proof = Package.new_proof pack name assums goal in
            let ped = Package.ped_of_proof pack proof in
               obj.rule_proof <- Interactive proof;
               obj.rule_ped <- Interactive ped;
               Proof_edit.set_params ped params;
               save_ped ();
               ped
       | Interactive ped ->
            ped
   in
   let edit_goal () =
      Proof_edit.ped_arg (get_ped ())
   in
   let edit_refine text ast tac =
      if !debug_refine then
         eprintf "Shell_rule.edit_refine: starting refinement%t" eflush;
      Proof_edit.refine_ped (get_ped ()) text ast tac;
      if !debug_refine then
         eprintf "Shell_rule.edit_refine: refinement done%t" eflush
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
   let rule =
      { Filter_summary.axiom_name = name;
        Filter_summary.axiom_stmt = unit_term;
        Filter_summary.axiom_proof = Incomplete
      }
   in
   let obj =
      { rule_assums = [];
        rule_params = [];
        rule_goal = unit_term;
        rule_proof = Incomplete;
        rule_ped = Incomplete
      }
   in
   let sentinal =
      let refiner =
         try Package.refiner pack with
            Not_found ->
               raise (RefineError ("create_rule", StringStringError ("no refiner", name)))
      in
         sentinal_of_refiner refiner
   in
   let arg = Package.argument pack in
      (* Package.set pack (Filter_summary.Axiom rule); *)
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

let view_axiom pack
    { Filter_summary.axiom_name = name;
      Filter_summary.axiom_stmt = goal;
      Filter_summary.axiom_proof = proof
    } =
   let obj =
      { rule_assums = [];
        rule_params = [];
        rule_goal = goal;
        rule_proof = proof;
        rule_ped = ped_of_proof pack proof
      }
   in
   let sentinal =
      let refiner =
         let refiner = Package.refiner pack in
            try snd (dest_refiner (find_refiner refiner name)) with
               Not_found ->
                  eprintf "Warning: using default refiner for %s%t" name eflush;
                  refiner
      in
         sentinal_of_refiner refiner
   in
   let arg = Package.argument pack in
      edit pack sentinal arg name obj

let view_rule pack
    { Filter_summary.rule_name = name;
      Filter_summary.rule_params = params;
      Filter_summary.rule_stmt = stmt;
      Filter_summary.rule_proof = proof
    } =
   let assums, goal = unzip_mfunction stmt in
   let assums = List.map snd assums in
   let obj =
      { rule_assums = assums;
        rule_params = params;
        rule_goal = goal;
        rule_proof = proof;
        rule_ped = ped_of_proof pack proof
      }
   in
   let sentinal =
      let refiner =
         let refiner = Package.refiner pack in
            try snd (dest_refiner (find_refiner refiner name)) with
               Not_found ->
                  eprintf "Warning: using default refiner for %s%t" name eflush;
                  refiner
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
