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

open Rformat
open Dform
open Refine

open Tactic_type
open Proof_step
open Proof
open Edit_type

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
   { ped_proof : proof;
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
   { mutable ped_goal : tactic_arg;
     mutable ped_undo : ped_proof list;
     mutable ped_stack : ped_proof list
   }

(************************************************************************
 * OPERATIONS                                                           *
 ************************************************************************)

(*
 * Constructors.
 *)
let create t =
   { ped_goal = t;
     ped_undo = [];
     ped_stack = []
   }

let ped_of_proof pf =
   let ped = { ped_proof = pf; ped_select = PedGoal } in
   let stack = [ped] in
      { ped_goal = proof_goal pf;
        ped_undo = stack;
        ped_stack = stack
      }

(*
 * Destructors.
 *)
let proof_of_ped { ped_undo = undo } =
   match undo with
      { ped_proof = pf }::_ -> pf
    | [] -> raise (Invalid_argument "Proof_edit.proof_of_ped")

(*
 * Get the goal of a proof.
 *)
let ped_goal { ped_proof = pf; ped_select = select } =
   match select with
      PedGoal ->
         proof_goal pf
    | PedChild i ->
         try
            match List.nth (proof_children pf) i with
               ProofChildTerm t -> t
             | ProofChildProof pf ->
                  raise (Failure "Proof_edit.ped_goal: child is not a leaf")
         with
            Not_found ->
               raise (Failure "Proof_edit.ped_goal: no such child")
            
(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

(*
 * Display the subgoals in order.
 *)
let display_children db buf =
   let rec aux i = function
      h::t ->
	 let goal, _ =
	    match h with
	       ProofChildTerm goal -> goal
	     | ProofChildProof pf -> proof_goal pf
	 in
	    format_string buf "\n-<subgoal>-\n";
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
	 let code =
	    match s with
	       StatusBad -> '-'
	     | StatusPartial -> '#'
	     | StatusAsserted -> '!'
	     | StatusComplete -> '*'
	 in
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
   display_status buffer status;

   (* Goal *)
   format_string buffer "\n-<main>-\n";
   format_term db buffer (fst goal);

   (* Rule *)
   format_string buffer "\n\n-<beginrule>-\n";
   format_string buffer "BY \n";
   format_string buffer "-<endrule>-\n"

(*
 * Display a proof with an inference.
 *)
let display_proof db buffer pf =
   let goal, _ = proof_goal pf in
   let item = proof_item pf in
   let children = proof_children pf in
   let status = proof_status pf in
      (* Display the current address *)
      display_status buffer status;

      (* Goal *)
      format_string buffer "\n-<main>-\n";
      format_term db buffer goal;

      (* Rule *)
      format_string buffer "\n\n-<beginrule>-\n";
      begin
         match item with
            ProofStep step ->
               format_string buffer "BY ";
               format_string buffer (step_text step);
               format_newline buffer
          | ProofProof _ ->
               format_string buffer "BY <proof>\n"
      end;
      format_string buffer "-<endrule>-\n";

      (* Subgoals *)
      display_children db buffer children

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
         display_goal db buffer goal [StatusPartial, 1]
    | { ped_proof = pf; ped_select = select }::_ ->
         match select with
            PedChild i ->
               display_goal db buffer goal ((proof_status pf) @ [StatusPartial, i + 1])
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
   let subgoals = fst (Refiner.refine tac goal) in
   let step = Proof_step.create goal subgoals text ast in
   let pf' =
      match undo with
         [] ->
            proof_of_step step
       | ped::_ ->
            let { ped_proof = pf; ped_select = select } = ped in
               match select with
                  PedGoal ->
                     replace_item pf (ProofStep step)
                | PedChild i ->
                     let pf' = replace_child pf i (proof_of_step step) in
                        proof_child pf' i
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
            raise (BadCommand "undo stack is empty")

(*
 * Reset the undo stack.
 *)
let nop_ped ped =
   let { ped_stack = stack } = ped in
      ped.ped_undo <- stack;
      match stack with
         { ped_proof = pf }::t ->
            ped.ped_goal <- proof_goal pf
       | [] ->
            ()

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
            let pf' = proof_main pf in
            let ped' = { ped_proof = pf'; ped_select = PedGoal } in
            let stack' = ped' :: stack in
               ped.ped_goal <- proof_goal pf';
               ped.ped_undo <- stack';
               ped.ped_stack <- stack'
       | [] ->
            ()

(*
 * Move to the parent goal.
 *)
let up_ped ped =
   let { ped_undo = undo; ped_stack = stack } = ped in
      match undo with
         [] ->
            ()
       | { ped_proof = pf; ped_select = select }::_ ->
            let pf' =
               match select with
                  PedChild _ ->
                     pf
                | PedGoal ->
                     match proof_parent pf with
                        Some pf' -> pf'
                      | None -> pf
            in
            let ped' = { ped_proof = pf'; ped_select = PedGoal } in
            let stack' = ped' :: stack in
               ped.ped_goal <- proof_goal pf';
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
            let child =
               try List.nth (proof_children pf) i with
                  Not_found -> raise (BadCommand "Bad child index")
            in
               match child with
                  ProofChildProof pf' ->
                     let ped' = { ped_proof = pf'; ped_select = PedGoal } in
                     let stack' = ped' :: stack in
                        ped.ped_goal <- proof_goal pf';
                        ped.ped_undo <- stack';
                        ped.ped_stack <- stack'
                | ProofChildTerm goal ->
                     (* This is a leaf *)
                     let ped' = { ped_proof = pf; ped_select = PedChild i } in
                     let stack' = ped' :: stack in
                        ped.ped_goal <- proof_goal pf;
                        ped.ped_undo <- stack';
                        ped.ped_stack <- stack'

(*
 * $Log$
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
