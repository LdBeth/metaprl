(*
 * The proof editor constructs a proof interactively.
 * We provide a notion of a "current" address into the
 * proof, which is the point in the proof that is displayed
 * on the screen.
 *
 *
 * At the base level, this data structure just adds undo capability
 * to proofs, and in doing so, the operations become imperative.
 *
 * Also add display capability.
 *)

include Proof

open Printf
open Debug

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Rformat
open Dform

open Sequent
open Tacticals

open Proof_step
open Proof

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Proof_edit%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * We include whether we are at the head or
 * a child of the proof.
 *)
type ped_select =
   PedGoal
 | PedChild of int

type ped_proof =
   { ped_proof : Proof.t;
     ped_select : ped_select
   }

(*
 * The is the state of the current proof.
 *    ped_goal: current term trying to be proved
 *    ped_undo: current undo stack, modified by undo operations
 *    ped_stack: global undo stack, not modified by undo operations
 *
 * Current proof is at top of undo stack.
 *)
type t =
   { mutable ped_params : term Filter_summary.param list;
     mutable ped_goal : tactic_arg;
     mutable ped_undo : ped_proof list;
     mutable ped_stack : ped_proof list
   }

(*
 * Line that delineates proofs.
 *)
let hline = "\012----------------------------------------\n"
let bline = "\n++++++++++\n"

(************************************************************************
 * OPERATIONS                                                           *
 ************************************************************************)

(*
 * Constructors.
 *)
let create params t =
   { ped_params = params;
     ped_goal = t;
     ped_undo = [];
     ped_stack = []
   }

let ped_of_proof params pf =
   let ped = { ped_proof = pf; ped_select = PedGoal } in
   let stack = [ped] in
      { ped_params = params;
        ped_goal = Proof.goal pf;
        ped_undo = stack;
        ped_stack = stack
      }

let set_params ped params =
   ped.ped_params <- params

(*
 * Destructors.
 *)
let proof_of_ped { ped_undo = undo } =
   match undo with
      { ped_proof = pf } :: _ ->
         pf
    | [] ->
         raise (Failure "proof_of_ped")

(*
 * Get the goal of a proof.
 *)
let ped_goal { ped_proof = pf; ped_select = select } =
   match select with
      PedGoal ->
         Proof.goal pf
    | PedChild i ->
         try
            match List.nth (Proof.children pf) i with
               Proof.ChildTerm t ->
                  t
             | Proof.ChildProof pf ->
                  raise (Failure "Proof_edit.ped_goal: child is not a leaf")
         with
            Failure "nth" ->
               raise (Failure "Proof_edit.ped_goal: no such child")

(*
 * Get the argument.
 *)
let ped_arg { ped_goal = goal } =
   goal

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

(*
 * Turn the status into a char.
 *)
let proof_status = function
   Proof.Bad -> '-'
 | Proof.Partial -> '#'
 | Proof.Asserted -> '!'
 | Proof.Complete -> '*'

(*
 * Display the subgoals in order.
 *)
let display_children db buf children =
   let rec aux i = function
      h::t ->
	 let status, goal =
	    match h with
	       Proof.ChildTerm goal ->
                  Proof.Partial, goal
	     | Proof.ChildProof pf ->
                  Proof.node_status pf, Proof.goal pf
	 in
         let goal = Sequent.goal goal in
	    (* format_string buf "\n-<subgoal>-\n"; *)
            format_char buf (proof_status status);
            format_char buf ' ';
	    format_int buf i;
	    format_string buf ". ";
	    format_pushm buf 0;
	    format_term db buf goal;
	    format_popm buf;
	    format_newline buf;
	    aux (i + 1) t
    | [] ->
         ()
   in
      aux 1 children

(*
 * Display the extra subgoals.
 *)
let display_extras db buf extras =
   let rec aux = function
      h::t ->
	 let status = Proof.node_status h in
         let goal = Proof.goal h in
         let goal = Sequent.goal goal in
	    (* format_string buf "\n-<subgoal>-\n"; *)
            format_char buf (proof_status status);
            format_string buf " * ";
	    format_pushm buf 0;
	    format_term db buf goal;
	    format_popm buf;
	    format_newline buf;
	    aux t
    | [] ->
         ()
   in
      match extras with
         [] ->
            ()
       | extras ->
            format_string buf "-----\n";
            aux extras

(*
 * Status is displayed as two lines.  The upper
 * line is the status, and the lower is the address.
 *)
let display_status buffer status =
   let stats, addrs = List.split status in
   let rec format_status = function
      (s::st, a::at) ->
	 let s' = string_of_int a in
	 let l = String.length s' in
	 let code = proof_status s in
	    format_char buffer code;
	    format_string buffer (String_util.make "Proof_edit.display_status" l ' ');
	    format_status (st, at)
    | _ -> ()
   in
   let rec format_addr = function
      a::at ->
	 format_int buffer a;
	 format_char buffer ' ';
	 format_addr at
    | _ -> ()
   in
      format_status (stats, addrs);
      format_newline buffer;
      format_addr addrs;
      format_newline buffer

(*
 * Display a goal, with no subgoals
 * and no tactic.
 *)
let display_goal db buffer goal status =
   let seq = Sequent.msequent goal in
   let goal', hyps = dest_msequent seq in
      (* Display the current address *)
      format_string buffer hline;
      display_status buffer status;
      format_string buffer "....";
      format_string buffer (Sequent.label goal);
      format_string buffer "....";
      format_newline buffer;

      (* Print hyps *)
      if hyps <> [] then
         begin
            let print_hyp hyp =
               format_term db buffer hyp;
               format_newline buffer
            in
               List.iter print_hyp hyps;
               format_string buffer "====\n"
         end;

      (* Goal *)
      (* format_string buffer "\n-<main>-\n"; *)
      format_term db buffer goal';

      (* Rule *)
      format_string buffer "\n\n";
      (* format_string buffer "\n\n-<beginrule>-\n"; *)
      format_string buffer "BY \n";
      (* format_string buffer "-<endrule>-\n" *)
      format_string buffer bline

(*
 * Display a proof with an inference.
 *)
let display_proof db buffer pf =
   let pf_goal = Proof.goal pf in
   let seq = Sequent.msequent pf_goal in
   let goal, hyps = dest_msequent seq in
   let item = Proof.item pf in
   let children = Proof.children pf in
   let extras = Proof.extras pf in
   let status = Proof.status pf in
      (* Display the current address *)
      format_string buffer hline;
      display_status buffer status;
      format_string buffer "....";
      format_string buffer (Sequent.label pf_goal);
      format_string buffer "....";
      format_newline buffer;

      (* Print hyps *)
      if hyps <> [] then
         begin
            let print_hyp hyp =
               format_term db buffer hyp;
               format_newline buffer
            in
               List.iter print_hyp hyps;
               format_string buffer "====\n"
         end;

      (* Goal *)
      (* format_string buffer "\n-<main>-\n"; *)
      format_term db buffer goal;

      (* Rule *)
      format_string buffer "\n\n";
      (* format_string buffer "\n\n-<beginrule>-\n"; *)
      begin
         match item with
            ProofStep step ->
               format_string buffer "BY ";
               format_string buffer (Proof_step.text step);
               format_newline buffer
          | ProofProof _ ->
               format_string buffer "BY <proof>\n"
      end;
      (* format_string buffer "-<endrule>-\n"; *)

      (* Subgoals *)
      display_children db buffer children;
      display_extras db buffer extras;
      format_string buffer bline

(*
 * Display the current proof.
 *    0. Display the status
 *    1. Display the goal
 *    2. Display the rule
 *    3. Display the subgoals
 *)
let format db buffer { ped_goal = goal; ped_undo = undo } =
   match undo with
      [] ->
         display_goal db buffer goal [Proof.Partial, 1]
    | { ped_proof = pf; ped_select = select }::_ ->
         match select with
            PedChild i ->
               display_goal db buffer goal ((Proof.status pf) @ [Proof.Partial, i + 1])
          | PedGoal ->
               display_proof db buffer pf

(************************************************************************
 * REFINEMENT                                                           *
 ************************************************************************)

(*
 * Refinement, and undo lists.
 * A finite number of undo's are allowed.
 * After a refine_ped or nop_ped, the undo stack gets reset.
 * The nop_ped does nothing but reset the undo stack.
 *)
let refine_ped ped text ast tac =
   let { ped_goal = goal; ped_undo = undo; ped_stack = stack } = ped in
   let subgoals, _ = Tacticals.refine tac goal in
   let step = Proof_step.create goal subgoals text ast tac in
   let pf' =
      match undo with
         [] ->
            Proof.of_step step
       | ped::_ ->
            let { ped_proof = pf; ped_select = select } = ped in
               match select with
                  PedGoal ->
                     replace_item pf (ProofStep step)
                | PedChild i ->
                     let pf' = replace_child pf i (Proof.of_step step) in
                        Proof.child pf' i
   in
   let ped' = { ped_proof = pf'; ped_select = PedGoal } in
   let stack' = ped' :: stack in
      ped.ped_undo <- stack';
      ped.ped_stack <- stack'

(*
 * Move down the undo stack.
 *)
let undo_ped ped =
   let { ped_undo = undo } = ped in
      match undo with
         _::h::t ->
            let goal = ped_goal h in
               ped.ped_goal <- goal;
               ped.ped_undo <- h::t
       | _ ->
            raise (RefineError ("undo_ped", StringError "undo stack is empty"))

(*
 * Reset the undo stack.
 *)
let nop_ped ped =
   let { ped_stack = stack } = ped in
      ped.ped_undo <- stack;
      match stack with
         { ped_proof = pf }::t ->
            ped.ped_goal <- Proof.goal pf
       | [] ->
            ()

(*
 * Fold the current subgoals into a new proof node.
 *)
let fold f ped =
   let { ped_undo = undo; ped_stack = stack } = ped in
      match undo with
         [] ->
            raise (RefineError ("fold_ped", StringError "no goal"))
       | ped' :: _ ->
            let { ped_proof = pf; ped_select = select } = ped' in
            let ped' = { ped_proof = f pf; ped_select = PedGoal } in
            let stack = ped' :: stack in
               ped.ped_undo <- stack;
               ped.ped_stack <- stack

let fold_ped = fold Proof.fold

let fold_all_ped = fold Proof.fold_all

(************************************************************************
 * NAVIGATION                                                           *
 ************************************************************************)

(*
 * Move to the parent goal.
 *)
let root_ped ped =
   let { ped_undo = undo; ped_stack = stack } = ped in
      match undo with
         { ped_proof = pf }::_ ->
            let pf' = Proof.main pf in
            let ped' = { ped_proof = pf'; ped_select = PedGoal } in
            let stack' = ped' :: stack in
               ped.ped_goal <- Proof.goal pf';
               ped.ped_undo <- stack';
               ped.ped_stack <- stack'
       | [] ->
            ()

(*
 * Move to the parent goal.
 *)
let up_ped ped i =
   if i > 0 then
      let { ped_undo = undo; ped_stack = stack } = ped in
         match undo with
            [] ->
               ()
          | { ped_proof = pf; ped_select = select }::_ ->
               let rec climb i pf =
                  if i = 0 then
                     pf
                  else
                     climb (i - 1) (Proof.parent pf)
               in
               let pf =
                  match select with
                     PedChild _ ->
                        pf
                   | PedGoal ->
                        try Proof.parent pf with
                           Failure "parent" ->
                              pf
               in
               let pf = climb (i - 1) pf in
               let ped' = { ped_proof = pf; ped_select = PedGoal } in
               let stack' = ped' :: stack in
                  ped.ped_goal <- Proof.goal pf;
                  ped.ped_undo <- stack';
                  ped.ped_stack <- stack'

(*
 * Move to a child.
 *)
let rec down_ped ped i =
   let { ped_undo = undo; ped_stack = stack } = ped in
      match undo with
         [] ->
            ()
       | { ped_proof = pf; ped_select = select }::_ ->
            if i <= 0 then
               match Proof.item pf with
                  Proof.ProofStep _ ->
                     raise (RefineError ("down_ped", StringError "Proof is not nested"))
                | Proof.ProofProof pf' ->
                     let ped' = { ped_proof = pf'; ped_select = PedGoal } in
                     let stack' = ped' :: stack in
                        ped.ped_goal <- Proof.goal pf';
                        ped.ped_undo <- stack';
                        ped.ped_stack <- stack'
            else
               let child =
                  try List.nth (Proof.children pf) (i - 1) with
                     Not_found ->
                        raise (RefineError ("down_ped", StringError "Bad child index"))
               in
                  match child with
                     Proof.ChildProof pf' ->
                        let ped' = { ped_proof = pf'; ped_select = PedGoal } in
                        let stack' = ped' :: stack in
                           ped.ped_goal <- Proof.goal pf';
                           ped.ped_undo <- stack';
                           ped.ped_stack <- stack'
                   | Proof.ChildTerm goal ->
                        (* This is a leaf *)
                        let ped' = { ped_proof = pf; ped_select = PedChild (i - 1) } in
                        let stack' = ped' :: stack in
                           ped.ped_goal <- goal;
                           ped.ped_undo <- stack';
                           ped.ped_stack <- stack'

(************************************************************************
 * PROOF CHECKING                                                       *
 ************************************************************************)

(*
 * Check a proof.
 * If there is a failure (ProofRefineError (pf, err)),
 * update the current edit, and raise an exception.
 *)
let check_ped ped =
   let { ped_undo = undo; ped_stack = stack } = ped in
      match undo with
         [] ->
            raise (RefineError ("check_ped", StringError "no goal"))
       | ped' :: _ ->
            let { ped_proof = pf } = ped' in
               try Proof.check pf with
                  Proof.ProofRefineError (pf', name, err) ->
                     let ped' = { ped_proof = pf'; ped_select = PedGoal } in
                     let stack = ped' :: stack in
                        ped.ped_undo <- stack;
                        ped.ped_stack <- stack;
                        raise (RefineError (name, err))

(*
 * When the proof is expanded, we make a dulicate.
 * Expansion never fails, but it may change the status of the proof.
 *)
let expand_ped df ped =
   let { ped_undo =undo; ped_stack = stack } = ped in
      match undo with
         [] ->
            raise (RefineError ("expand_ped", StringError "no goal"))
       | ped' :: _ ->
            let { ped_proof = pf; ped_select = select } = ped' in
            let ped' = { ped_proof = Proof.expand df pf; ped_select = select } in
            let stack = ped' :: stack in
               ped.ped_undo <- stack;
               ped.ped_stack <- stack

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
