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
open Refiner.Refiner.Refine
open Rformat
open Dform

open Tactic_type
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
   { mutable ped_params : Filter_summary.param list;
     mutable ped_goal : tactic_arg;
     mutable ped_undo : ped_proof list;
     mutable ped_stack : ped_proof list
   }

(*
 * Line that delineates proofs.
 *)
let hline = "\n--------------------------------------------------------------------------------\n"
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
let display_children db buf =
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
      aux 1

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
	    format_string buffer (String.make l ' ');
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
   (* Display the current address *)
   format_string buffer hline;
   display_status buffer status;
   format_string buffer "....";
   format_string buffer (Sequent.label goal);
   format_string buffer "....";
   format_newline buffer;

   (* Goal *)
   (* format_string buffer "\n-<main>-\n"; *)
   format_term db buffer (Sequent.goal goal);

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
   let goal = Sequent.goal pf_goal in
   let item = Proof.item pf in
   let children = Proof.children pf in
   let status = Proof.status pf in
      (* Display the current address *)
      format_string buffer hline;
      display_status buffer status;
      format_string buffer "....";
      format_string buffer (Sequent.label pf_goal);
      format_string buffer "....";
      format_newline buffer;

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
   let subgoals, _ = Tactic_type.refine tac goal in
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
   let _ = eprintf "Proof_edit.refine_ped: done%t" eflush in
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
                  Proof.ProofRefineError (pf', err) ->
                     let ped' = { ped_proof = pf'; ped_select = PedGoal } in
                     let stack = ped' :: stack in
                        ped.ped_undo <- stack;
                        ped.ped_stack <- stack;
                        raise (RefineError err)

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
 * $Log$
 * Revision 1.11  1998/06/15 22:31:47  jyh
 * Added CZF.
 *
 * Revision 1.10  1998/06/12 13:45:10  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.9  1998/06/03 22:19:11  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.8  1998/06/01 19:53:10  jyh
 * Working addition proof.  Removing polymorphism from refiner(?)
 *
 * Revision 1.7  1998/06/01 13:52:21  jyh
 * Proving twice one is two.
 *
 * Revision 1.6  1998/05/28 13:45:50  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.5  1998/04/28 18:29:47  jyh
 * ls() works, adding display.
 *
 * Revision 1.4  1998/04/24 02:41:31  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.3  1998/04/23 20:03:52  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.2  1998/04/15 12:39:40  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.1  1997/08/06 16:17:23  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/10/23 15:17:50  jyh
 * First working version of dT tactic.
 *
 * Revision 1.3  1996/09/02 19:33:33  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:40  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 17:00:08  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
