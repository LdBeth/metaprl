(*
 * Create an ediable rewrite object.
 *)

include Shell_type
include Package_info
include Package_df
include Proof_type

open Printf
open Debug

open Rformat
open Dform
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Refine
open Opname

open Filter_summary
open Filter_cache
open Filter_ocaml

open Tactic_type
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
   { mutable rule_params : param list;
     mutable rule_assums : term list;
     mutable rule_goal : term;
     mutable rule_proof : Package.proof proof_type;
     mutable rule_ped : Proof_edit.t proof_type
   }

(*
 * Make a rewrite goal from the assumptions,
 * and the rewrite.
 *)
let mk_rule_goal arg assums goal =
   let { ref_label = label; ref_fcache = cache; ref_args = args; ref_rsrc = resources } = arg in
      Tactic_type.create label { mseq_goal = goal; mseq_hyps = assums } cache args resources

let mk_ped arg params assums goal =
   Proof_edit.create params (mk_rule_goal arg assums goal)

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
   let { mseq_goal = rw; mseq_hyps = hyps } = Tactic_type.msequent arg in
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
let format_prim_rule db buf arg params assums goal =
   let tac = Refine_exn.print db (mk_rule_goal arg assums) goal in
      format_tac db buf tac;
      format_newline buf;
      format_string buf "Primitive";
      format_newline buf

let format_cond_rule db buf arg params assums goal expr =
   let tac = Refine_exn.print db (mk_rule_goal arg assums) goal in
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
let unit_term = mk_simple_term nil_opname []

let edit pack arg name obj =
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
               format_prim_rule db buf arg params assums goal
          | Derived expr ->
               format_cond_rule db buf arg params assums goal expr
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
      match obj.rule_ped with
         Interactive ped ->
            save_ped ()
       | Primitive _
       | Derived _ ->
            ()
   in
   let edit_check () =
      match obj.rule_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check primitive rules"))
       | Derived _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check noninteractive proofs"))
       | Interactive ped ->
            try Proof_edit.check_ped ped with
               RefineError err ->
                  raise (RefineError (name, GoalError err))
   in
   let get_ped obj =
      match obj.rule_ped with
         Primitive _
       | Derived _ ->
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
   let edit_refine text ast tac =
      let ped =
         match obj.rule_ped with
            Primitive _
          | Derived _ ->
               (* Convert to a ped *)
               let { rule_params = params;
                     rule_assums = assums;
                     rule_goal = goal
                   } = obj
               in
               let proof = Package.new_proof pack assums goal in
               let ped = Package.ped_of_proof pack proof in
                  obj.rule_proof <- Interactive proof;
                  obj.rule_ped <- Interactive ped;
                  Proof_edit.set_params ped params;
                  save_ped ();
                  ped
          | Interactive ped ->
               ped
      in
         if !debug_refine then
            eprintf "Shell_rule.edit_refine: starting refinement%t" eflush;
         Proof_edit.refine_ped ped text ast tac;
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
        edit_refine = edit_refine;
        edit_undo = edit_undo;
        edit_fold = edit_fold;
        edit_fold_all = edit_fold_all
      }

let create pack name =
   let proof = Package.new_proof pack [] unit_term in
   let ped = Package.ped_of_proof pack proof in
   let rule =
      { Filter_summary.axiom_name = name;
        Filter_summary.axiom_stmt = unit_term;
        Filter_summary.axiom_proof = Interactive proof
      }
   in
   let obj =
      { rule_assums = [];
        rule_params = [];
        rule_goal = unit_term;
        rule_proof = Interactive proof;
        rule_ped = Interactive ped
      }
   in
   let arg = Package.argument pack in
      Package.set pack (Filter_summary.Axiom rule);
      edit pack arg name obj

let ped_of_proof pack = function
   Primitive proof ->
      Primitive proof
 | Derived expr ->
      Derived expr
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
   let arg = Package.argument pack in
      edit pack arg name obj

let view_rule pack
    { Filter_summary.rule_name = name;
      Filter_summary.rule_params = params;
      Filter_summary.rule_stmt = stmt;
      Filter_summary.rule_proof = proof
    } =
   let assums = unzip_mimplies stmt in
   let assums, goal = List_util.split_last assums in
   let obj =
      { rule_assums = assums;
        rule_params = params;
        rule_goal = goal;
        rule_proof = proof;
        rule_ped = ped_of_proof pack proof
      }
   in
   let arg = Package.argument pack in
      edit pack arg name obj

(*
 * $Log$
 * Revision 1.1  1998/06/15 22:31:54  jyh
 * Added CZF.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
