(*
 * A proof is a collection of inferences, where each inference is
 * a proof step or it is a nested proof.  Each inference
 * has the same goal as a subgoal of a previous inference.
 *
 *                   Goal           status:
 *                    |                bad: one if the proof_items has failed
 *                    |                partial: some incomplete subgoals
 *                    |                asserted: pretend like the proof is complete
 *                    |                complete: all steps have been checked
 *                    |
 *                   Item           proof_item
 *                  / | \
 *                 /  |  \
 *                /   |   \
 *               C1   C2  C3        children
 *              / |   |   | \
 *             /  |   |   |  \
 *            .   .   .   .   .
 *           .    .   .   .    .
 *          SG1  SG2 SG3 SG4  SG5   subgoals
 *
 * We also provide tools for navigation:
 *    1. Get the parent inference
 *    2. Get a subgoal inference
 *    3. Replace a subgoal inference
 *    4. Replace the tactic of the current inference
 *
 * These are functional structures, and they are singly linked from the
 * parents toward the leaves.  Navigation up the tree takes log time.
 *
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

include Tacticals

include Io_proof_type
include Proof_type
include Proof_step

open Printf
open Mp_debug

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Refine_exn

open Sequent
open Tacticals

open Io_proof_type
open Proof_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Proof%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * An address is a integer path.
 * A 0 in the address means a nested proof,
 * and n means child (n - 1) (starting from 0).
 *)
type address = int list

(*
 * Head of a proof is a single step, or another proof.
 * This is the "justification" for the first step of
 * the proof.
 *)
type item =
   ProofStep of Proof_step.t
 | ProofProof of t

(*
 * Status of a proof node.
 *)
and status =
   Bad
 | Partial
 | Asserted
 | Complete

(*
 * A proof may be just a proof step, or it may be a composition
 * of proofs and proof steps.  The invariants are:
 *   1. There are the same number of children as there are subgoals of the item
 *   2. If a child is (ChildGoal goal), then "goal" is the corresponding
 *      subgoal of the item
 *   3. If a child is (ChildNode node), the the goal of the node is equal to the
 *      corresponding subgoal of the item
 *
 * The node_extra list is used if the item is modified and children
 * have to be removed to preserved the invariant.  In that case, the children
 * a saved as "extras."
 *)
and node =
   { node_status   : status;
     node_item     : node_item;
     node_children : child_node list;
     node_extras   : node list
   }

and node_item =
   Step of Proof_step.t
 | Node of node

and child_node =
   ChildGoal of tactic_arg
 | ChildNode of node

and child =
   ChildTerm of tactic_arg
 | ChildProof of t

(*
 * The actual "proof" is a handle, with a reference
 * to the root of the proof, the current address, and
 * a pointer to the current proof node.
 *
 * The invariant here is:
 *    pf_root[pf_address] = pf_node
 *)
and t =
   { pf_root : node;
     pf_address : address;
     pf_node : node
   }

(*
 * This exception is raised when composed proofs don't match
 *)
exception Match

(*
 * We overload the refinement error to give the location of
 * the error.
 *)
exception ProofRefineError of t * string * refine_error

(*
 * For building IO proofs.
 *)
type io_proof = Refiner_std.Refiner.TermType.term proof

(*
 * Memo tables for converting proofs.
 *)
type 'a denorm =
   { denorm : term -> 'a;
     step_denorm : 'a Proof_step.denorm;
     proof_of_node : ('a denorm, node, 'a proof, 'a proof) Memo.t;
     proof_node_of_node_item : ('a denorm, node_item, 'a proof_node, 'a proof_node) Memo.t;
     proof_child_of_child_node : ('a denorm, child_node, 'a proof_child, 'a proof_child) Memo.t
   }

type 'a norm =
   { norm : 'a -> term;
     step_norm : 'a Proof_step.norm;
     node_of_proof : ('a norm, 'a proof, 'a proof, node) Memo.t;
     node_item_of_proof_node : ('a norm, 'a proof_node, 'a proof_node, node_item) Memo.t;
     child_node_of_proof_child : ('a norm, 'a proof_child, 'a proof_child, child_node) Memo.t
   }

(************************************************************************
 * CONSTRUCTORS                                                         *
 ************************************************************************)

(*
 * Make a proof from a step.
 *)
let of_step step =
   let subgoals = Proof_step.subgoals step in
   let status =
      if subgoals = [] then
         Complete
      else
         Partial
   in
   let children = List.map (function t -> ChildGoal t) subgoals in
   let node =
      { node_status = status;
	node_item = Step step;
	node_children = children;
	node_extras = []
      }
   in
      { pf_root = node;
	pf_address = [];
	pf_node = node
      }

(************************************************************************
 * DESTRUCTORS                                                          *
 ************************************************************************)

(*
 * Get goals and subgoals.
 *)
let rec node_goal { node_item = item } =
   match item with
      Step step ->
         Proof_step.goal step
    | Node node ->
         node_goal node

let rec node_subgoals { node_children = children } =
   let flatten = function
      ChildGoal goal ->
         [goal]
    | ChildNode node ->
         node_subgoals node
   in
      List_util.flat_map flatten children

(*
 * Proof info.
 *)
let goal { pf_node = node } = node_goal node

let subgoals { pf_node = node } = node_subgoals node

(*
 * Walking the tree.
 *)
let item { pf_root = root; pf_address = addr; pf_node = node } =
   match node.node_item with
      Step step ->
         ProofStep step
    | Node node ->
         ProofProof { pf_root = root;
                      pf_address = addr @ [0];
                      pf_node = node
	 }

(*
 * Get the children of a proof.
 *)
let children
    { pf_root = root;
      pf_address = addr;
      pf_node = { node_children = children }
    } =
   let rec collect i = function
      h :: t ->
         let t' = collect (i + 1) t in
         let h' =
            match h with
               ChildGoal goal ->
                  ChildTerm goal
             | ChildNode node ->
                  ChildProof { pf_root = root; pf_address = addr @ [i]; pf_node = node }
         in
            h' :: t'
    | [] ->
         []
   in
      collect 1 children

(*
 * Get the extra subgoals.
 * The addresses are not really correct, since extras are
 * not addressable.
 *)
let extras
    { pf_root = root;
      pf_address = addr;
      pf_node = { node_children = children; node_extras = extras }
    } =
   let rec collect i = function
      h :: t ->
         { pf_root = root; pf_address = addr @ [i]; pf_node = h }
         :: collect (i + 1) t
    | [] ->
         []
   in
      collect (List.length children + 1) extras

(*
 * Select the next node specified by the next int in the address.
 *)
let select node i =
   if i = 0 then
      match node with
         { node_item = Node node' } ->
            node'
       | { node_item = Step _ } ->
            raise (Failure "Proof.select")
   else
      let length = List.length node.node_children in
         if i <= length then
            match List.nth node.node_children (i - 1) with
               ChildGoal _ ->
                  raise (Failure "Proof.select")
             | ChildNode node ->
                  node
         else
            try List.nth node.node_extras (i - length - 1) with
               Failure "nth" ->
                  raise (Failure "Proof.select")

(*
 * Follow the complete address.
 * If the address is not valid, raise an error with the
 * prefix of the address that worked, plus the point where it
 * failed.
 *)
let node_index node addr =
   let rec search node = function
      i :: t ->
         let node' =
            try select node i with
               Failure _ ->
                  raise (Failure "node_index")
         in
            search node' t
    | [] ->
         node
   in
      search node addr

(*
 * Index from a proof node.
 * Again, on failure, interprete the failure,
 * and return the point where the address failed.
 *)
let index { pf_root = root; pf_address = addr; pf_node = node } addr' =
   let node' =
      try node_index node addr' with
         Failure "node_index" ->
            raise (Failure "index")
   in
      { pf_root = root;
        pf_address = addr @ addr';
        pf_node = node'
      }

let child { pf_root = root; pf_address = addr; pf_node = node } i =
   let node' = select node (i + 1) in
      { pf_root = root;
        pf_address = addr @ [i + 1];
        pf_node = node'
      }

(*
 * Back up to parent.
 *)
let parent { pf_root = root; pf_address = addr } =
   if addr = [] then
      raise (Failure "parent")
   else
      let addr', _ = List_util.split_last addr in
      let node' = node_index root addr' in
         { pf_root = root;
           pf_address = addr';
           pf_node = node'
         }

(*
 * Get the main goal.
 *)
let main { pf_root = root } =
   { pf_root = root; pf_address = []; pf_node = root }

(*
 * Address relative to the main goal.
 *)
let address { pf_address = addr } = addr

(*
 * Get the status too.
 *)
let status { pf_root = root; pf_address = addr } =
   let rec search node addr =
      let status = node.node_status in
         match addr with
            i :: t ->
               (* This can't fail! *)
               let node' = select node i in
                  (status, i) :: (search node' t)
          | [] ->
               [status, 0]
   in
      search root addr

let node_status { pf_node = { node_status = status } } =
   status

(************************************************************************
 * UPDATES                                                              *
 ************************************************************************)

(*
 * Combine the status of the item and the children.
 * The status of the children is the minimum of the statuses.
 * The combined status is:
 *    If the item is complete or asserted,
 *        then the children must be complete,
 *        the resulting status is complete
 *
 *    If the item is partial, the status is that of the children
 *
 *    If the item is bad, so is the total status
 *)
let compute_status item children =
   (* Status of the item *)
   let istatus =
      match item with
         Step step ->
            if Proof_step.subgoals step = [] then
               Complete
            else
               Partial
       | Node { node_status = status } ->
            status
   in

   (* Status of a single child *)
   let child_status = function
      ChildGoal _ ->
         Partial
    | ChildNode { node_status = status } ->
         status
   in

   (* Minimize status across all the children *)
   let min_status status1 child =
      let status2 = child_status child in
         match status1 with
            Complete ->
               status2
          | Asserted ->
               begin
                  match status2 with
                     Complete -> status1
                   | _ -> status2
               end
          | Partial ->
               begin
                  match status2 with
                     Complete | Asserted -> status1
                   | _ -> status2
               end
          | Bad ->
               Bad
   in
   let cstatus = List.fold_left min_status Complete children in
      (* Combine the status of the item and of the children *)
      match istatus with
         Complete | Asserted ->
            begin
               match cstatus with
                  Complete -> istatus
                | _ -> raise (Failure "Proof.compute_status")
            end
       | Partial ->
            cstatus
       | Bad ->
            Bad

(*
 * Replace a node in the tree functionally.
 * It is assumed that node' has the same goal as node.
 *
 * Adjust the status as the tree is copied.
 *)
let replace_node { pf_root = root; pf_address = addr; pf_node = node } node' =
   let rec replace
       { node_status = status;
	 node_item = item;
	 node_children = children;
	 node_extras = extras
       } = function
      i :: t ->
         if i = 0 then
            (* Replace the interior node *)
            match item with
	       Node node' ->
                  let item' = Node (replace node' t) in
                  let status' = compute_status item' children in
                     { node_status = status';
		       node_item = item';
		       node_children = children;
		       node_extras = extras
                     }
             | _ ->
                  (* This violates the invariant *)
		  raise (Failure "Proof.replace_node: illegal interior node")

         else
            let child = List.nth children (i - 1) in
            let node'' =
               match child with
		  ChildNode node ->
                     replace node t
                | ChildGoal _ ->
                     (* This violates the invariant *)
		     raise (Failure "Proof.replace_node: truncated proof tree")
            in
            let replace _ = ChildNode node'' in
            let children' = List_util.replacef_nth (i - 1) replace children in
            let status' = compute_status item children' in
               { node_status = status';
		 node_item = item;
		 node_children = children';
		 node_extras = extras
               }

    | [] ->
	 node'
   in
   let root' = replace root addr in
      { pf_root = root';
	pf_address = addr;
	pf_node = node'
      }

(*
 * When the subgoals of a step are replaced we want to find the
 * best match between the old subgoals and the new ones.  Subgoals
 * of the form (ChildGoal _) are not considered to be significant.
 * Extra goals can be added.  Previous subgoals that don't match
 * are pushed onto the extras list.  To optimize for the common case,
 * we first consider lists that match in order before considering
 * permutations.
 *)
let rec child_nodes = function
   [] ->
      []
 | h::t ->
      match h with
         ChildGoal _ ->
            child_nodes t
       | ChildNode n ->
            n :: child_nodes t

(*
 * Get the goal of the child.
 *)
let child_goal = function
   ChildGoal t ->
      t
 | ChildNode node ->
      node_goal node

(*
 * This is the common case.
 *)
let join_ordered_children =
   let join_child subgoal = function
      ChildGoal _ ->
         ChildGoal subgoal
    | ChildNode node ->
	 let subgoal' = Sequent.goal (node_goal node) in
	    if alpha_equal subgoal' (Sequent.goal subgoal) then
	       ChildNode node
	    else
	       raise Match
   in
   let rec join_common = function
      subgoal :: subgoals, child :: children ->
	 let child = join_child subgoal child in
	 let children', extras = join_common (subgoals, children) in
	    child :: children', extras
    | subgoals, [] ->
	 List.map (function x -> ChildGoal x) subgoals, []
    | [], children ->
	 [], child_nodes children
   in
   let join subgoals children = join_common (subgoals, children) in
      join

(*
 * Try permutations, where matching is equality.
 * This function is quadratic in the number of subgoals.
 *)
let join_permuted_children subgoals children =
   let rec search subgoal = function
      child :: children ->
         if alpha_equal (Sequent.goal subgoal) (Sequent.goal (node_goal child)) then
            ChildNode child, children
         else
            let goal, children' = search subgoal children in
               goal, child :: children'
    | [] ->
         ChildGoal subgoal, []
   in
   let rec crossref subgoals children =
      match subgoals with
         subgoal :: subgoals ->
            let subgoal, children = search subgoal children in
            let subgoals, children = crossref subgoals children in
               subgoal :: subgoals, children
       | [] ->
            [], children
   in
      crossref subgoals children

(*
 * Combine them.
 *)
let join_children subgoals children extras =
   let extras = List.map (fun extra -> ChildNode extra) extras in
   let children = children @ extras in
      try join_ordered_children subgoals children with
         Match ->
            join_permuted_children subgoals (child_nodes children)

(*
 * Check that the goals match.
 *)
let check_goals node item =
   let goal = node_goal node in
   let goal' =
      match item with
         Step step ->
            Proof_step.goal step

       | Node node ->
            node_goal node
   in
      if not (tactic_arg_alpha_equal goal goal') then
         raise Match

(*
 * Replace the proof item with another.  The goal of the new item
 * must match the old goal, and the new subgoals should match those of
 * any children that exist.
 *)
let replace_node_item check node item =
   let subgoals =
      match item with
         Step step ->
            Proof_step.subgoals step
       | Node node ->
            node_subgoals node
   in
      let { node_children = children; node_extras = extras } = node in
      let children', extras' = join_children subgoals children extras in
         { node_status = compute_status item children';
           node_item = item;
           node_children = children';
           node_extras = extras'
         }

(*
 * Replace the proof item with another.  The goal of the new item
 * must match the old goal, and the new subgoals should match those of
 * any children that exist.
 *)
let replace_item pf item =
   let item =
      match item with
         ProofStep step ->
            Step step
       | ProofProof { pf_node = node } ->
            Node node
   in
   let { pf_node = node } = pf in
   let _ = check_goals node item in
   let node = replace_node_item true node item in
      replace_node pf node

(*
 * Replace a child.  Check that the child retains the goal.
 *)
let replace_child pf i { pf_node = child' } =
   let { pf_node = node } = pf in
   let { node_item = item;
         node_children = children;
         node_extras = extras
       } = node
   in
   let goal' = Sequent.goal (node_goal child') in
   let replace = function
      ChildGoal goal ->
         if alpha_equal (Sequent.goal goal) goal' then
            ChildNode child'
         else
            raise (RefineError ("replace_child", StringTermError ("ChildGoal: goals are not equal", goal')))
    | ChildNode node ->
         if alpha_equal (Sequent.goal (node_goal node)) goal' then
            ChildNode child'
         else
            raise (RefineError ("replace_child", StringTermError ("ChildNode: goals are not equal", goal')))
   in
   let children' =
      try List_util.replacef_nth i replace children with
         Failure "replacef_nth" ->
            raise (RefineError ("replace_child", StringIntError ("index is out of bounds", i)))
   in
   let status' = compute_status item children' in
   let node' =
      { node_status = status';
        node_item = item;
        node_children = children';
        node_extras = extras
      }
   in
      replace_node pf node'

(*
 * Remove a child, and replace it with just the goal.
 *)
let remove_child pf i =
   let { pf_node = node } = pf in
   let { node_item = item;
         node_children = children;
         node_extras = extras
       } = node
   in
   let replace = function
      ChildGoal goal ->
         ChildGoal goal
    | ChildNode node ->
         let goal = node_goal node in
            ChildGoal goal
   in
   let children' = List_util.replacef_nth i replace children in
   let status' = compute_status item children' in
   let node' =
      { node_status = status';
        node_item = item;
        node_children = children';
        node_extras = extras
      }
   in
      replace_node pf node'

(*
 * Remove all the children.
 *)
let remove_children pf =
   let { pf_node = node } = pf in
   let { node_item = item; node_extras = extras } = node in
   let item_subgoals = function
      Step step -> Proof_step.subgoals step
    | Node node -> node_subgoals node
   in
   let children' = List.map (function x -> ChildGoal x) (item_subgoals item) in
   let status' = compute_status item children' in
   let node' =
      { node_status = status';
        node_item = item;
        node_children = children';
        node_extras = extras
      }
   in
      replace_node pf node'

(*
 * Fold the current proof step, wrapping the subgoals into a subproof.
 *)
let fold pf =
   let { pf_node = node } = pf in
   let { node_status = status;
         node_item = item;
         node_children = children;
         node_extras = extras
       } = node
   in
   let rec collect = function
      child :: childt ->
         let item, children, extras = collect_child child in
         let items, children', extras' = collect childt in
            item :: items, children @ children', extras @ extras'
    | [] ->
         [], [], []
   and collect_child child =
      match child with
         ChildGoal _ ->
            child, [], []
       | ChildNode node ->
            let { node_item = item;
                  node_children = children;
                  node_extras = extras
                } = node
            in
            let children' = List.map (fun child -> ChildGoal (child_goal child)) children in
            let node' =
               { node_status = compute_status item children';
                 node_item = item;
                 node_children = children';
                 node_extras = []
               }
            in
               ChildNode node', children, extras
   in
   let items, children, extras = collect children in
   let node' =
      { node_status = compute_status item items;
        node_item = item;
        node_children = items;
        node_extras = []
      }
   in
   let node =
      { node_status = status;
        node_item = Node node';
        node_children = children;
        node_extras = extras
      }
   in
      replace_node pf node

(*
 * Do a fold, but fold all the way down to the leaves.
 *)
let fold_all pf =
   let { pf_node = node } = pf in
   let { node_status = status;
         node_item = item;
         node_children = children;
         node_extras = extras
       } = node
   in
   let children' = List.map (fun child -> ChildGoal child) (node_subgoals node) in
   let node' =
      { node_status = status;
        node_item = item;
        node_children = children;
        node_extras = []
      }
   in
   let node =
      { node_status = status;
        node_item = Node node';
        node_children = children';
        node_extras = extras
      }
   in
      replace_node pf node

(************************************************************************
 * PROOF CHECKING                                                       *
 ************************************************************************)

(*
 * When a proof is checked, we check each of the steps individually
 * and compose the results with the refiner.  This gives us a lot
 * more control than Kreitz'ing the tactic.
 *
 * Addresses are reversed in the check_ functions.
 *)
let check { pf_root = root; pf_address = addr; pf_node = node } =
   let mk_pf addr node =
      replace_node ({ pf_root = root; pf_address = List.rev addr; pf_node = node }) node
   in
   let rec check_node addr' node =
      let { node_status = status;
            node_item = item;
            node_children = children
          } = node
      in
      let ext =
         if status = Complete then
            match item with
               Step step ->
                  check_step addr' node step
             | Node node ->
                  check_node (0 :: addr') node
         else
            raise (ProofRefineError (mk_pf addr' node, "Proof.check", StringError "Proof is not complete"))
      in
      let rec fold_child i = function
         child :: childt ->
            check_child (i :: addr) node child :: fold_child (i + 1) childt
       | [] ->
            []
      in
      let extl = fold_child 1 children in
         try Tacticals.compose ext extl with
            RefineError (name, err) ->
               raise (ProofRefineError (mk_pf addr' node, name, err))

   and check_step addr' node step =
      try Proof_step.check step with
         RefineError (name, err) ->
            raise (ProofRefineError (mk_pf (List.tl addr') node, name, err))

   and check_child addr' node = function
      ChildGoal goal ->
         (* This can't happen if the status were set correctly *)
         raise (ProofRefineError (mk_pf addr' node, "Proof.check", StringError "Proof is not complete"))
    | ChildNode node ->
         check_node addr' node
   in
      check_node (List.rev addr) node

(*
 * During expansion, we allow the refinement to change arbitrarily.
 * This is functional update, so the entire proof may be copied.
 *
 * This function never fails.
 *)
let expand df pf =
   let { pf_node = node } = pf in
   let rec expand_node node =
      let item =
         match node.node_item with
            Step step ->
               Step (Proof_step.expand df step)
          | Node node' ->
               Node (expand_node node')
      in
      let node' = replace_node_item false node item in
      let { node_children = children; node_extras = extras } = node' in
      let children = List.map expand_child children in
         { node_status = compute_status item children;
           node_item = item;
           node_children = children;
           node_extras = extras
         }
   and expand_child child =
      match child with
         ChildGoal _ ->
            child
       | ChildNode node ->
            ChildNode (expand_node node)
   in
      replace_node pf (expand_node node)

(*
 * Count the number of nodes in the proof.
 *)
let node_count { pf_address = addr; pf_node = node } =
   let rec count_node count node =
      let { node_item = item; node_children = children } = node in
      let count =
         match item with
            Step step ->
               succ count
          | Node node ->
               count_node count node
      in
         List.fold_left count_child count children

   and count_child count = function
      ChildGoal goal ->
         count
    | ChildNode node ->
         count_node count node
   in
      count_node 0 node

(*
 * Kreitz the tree into a single node.
 *)
let kreitz ({ pf_node = node } as proof) =
   let loc = 0, 0 in
   let compose (text, ast, tactic, _) = function
      [] ->
         text, ast, tactic, []
    | subnodes ->
         let rec concat_text = function
            [text, _, _, _] ->
               text
          | (text, _, _, _) :: subnodes ->
               text ^ "; " ^ concat_text subnodes
          | [] ->
               raise (Invalid_argument "Proof.kreitz")
         in
         let rec concat_ast = function
            (_, e, _, _) :: tl ->
               (<:expr< $lid:"::"$ $e$ $concat_ast tl$ >>)
          | [] ->
               (<:expr< [] >>)
         in
         let rec concat_subgoals = function
            (_, _, _, subgoals) :: tl ->
               subgoals @ concat_subgoals tl
          | [] ->
               []
         in
         let text' = concat_text subnodes in
         let ast'= concat_ast subnodes in
         let tactic' = List.map (fun (_, _, t, _) -> t) subnodes in
         let subgoals = concat_subgoals subnodes in
         let text = sprintf "%s thenLT [%s]" text text' in
         let ast = (<:expr< $lid: "prefix_thenLT"$ $ast$ $ast'$ >>) in
         let tactic = tactic thenLT tactic' in
            text, ast, tactic, subgoals
   in
   let rec kreitz_node node =
      let { node_item = item; node_children = children } = node in
      let node =
         match item with
            Step step ->
               Proof_step.text step,
               Proof_step.ast step,
               Proof_step.tactic step,
               Proof_step.subgoals step
          | Node node ->
               kreitz_node node
      in
      let subnodes = List.map kreitz_child children in
         compose node subnodes

   and kreitz_child = function
      ChildGoal goal ->
         "idT", (<:expr< $lid: "idT"$ >>), idT, [goal]
    | ChildNode node ->
         kreitz_node node
   in

   let goal = node_goal node in
   let text, ast, tactic, subgoals = kreitz_node node in
   let step = Proof_step.create goal subgoals text ast tactic in
   let node' =
      { node_status = Partial;
        node_item = Step step;
        node_children = List.map (fun g -> ChildGoal g) subgoals;
        node_extras = []
      }
   in
      replace_node proof node'

(************************************************************************
 * IO                                                                   *
 ************************************************************************)

(*
 * Comparisons.
 *)
let compare_proof
    { proof_status = status1;
      proof_step = step1;
      proof_children = children1;
      proof_extras = extras1
    }
    { proof_status = status2;
      proof_step = step2;
      proof_children = children2;
      proof_extras = extras2
    } =
   (status1 = status2)
   & (step1 == step2)
   & (List_util.compare_eq children1 children2)
   & (List_util.compare_eq extras1 extras2)

let compare_proof_node node1 node2 =
   match node1, node2 with
      Io_proof_type.ProofStep { step_goal = goal1;
                  step_subgoals = subgoals1;
                  step_ast = ast1;
                  step_text = text1
      },
      Io_proof_type.ProofStep { step_goal = goal2;
                  step_subgoals = subgoals2;
                  step_ast = ast2;
                  step_text = text2
      } ->
         (goal1 == goal2)
         & (List_util.compare_eq subgoals1 subgoals2)
         & (text1 = text2)
    | Io_proof_type.ProofNode node1, Io_proof_type.ProofNode node2 ->
         node1 == node2
    | _ ->
         false

let compare_proof_child child1 child2 =
   match child1, child2 with
      Io_proof_type.ChildGoal goal1, Io_proof_type.ChildGoal goal2 ->
         goal1 == goal2
    | Io_proof_type.ChildProof proof1, Io_proof_type.ChildProof proof2 ->
         proof1 == proof2
    | _ ->
         false

(*
 * Convert the status values.
 *)
let io_status_of_status = function
   Bad ->
      Io_proof_type.StatusBad
 | Partial ->
      Io_proof_type.StatusPartial
 | Asserted ->
      Io_proof_type.StatusAsserted
 | Complete ->
      Io_proof_type.StatusComplete

(*
 * Make the proofs.
 *)
let make_proof info { node_status = status;
                      node_item = item;
                      node_children = children;
                      node_extras = extras
    } =
   { proof_status = io_status_of_status status;
     proof_step = Memo.apply info.proof_node_of_node_item info item;
     proof_children = List.map (Memo.apply info.proof_child_of_child_node info) children;
     proof_extras = List.map (Memo.apply info.proof_of_node info) extras
   }

let make_proof_node info = function
   Step step ->
      Io_proof_type.ProofStep (Proof_step.io_step_of_step info.step_denorm step)
 | Node node ->
      Io_proof_type.ProofNode (Memo.apply info.proof_of_node info node)

let make_proof_child info = function
   ChildGoal goal ->
      Io_proof_type.ChildGoal (Proof_step.aterm_of_tactic_arg info.step_denorm goal)
 | ChildNode node ->
      Io_proof_type.ChildProof (Memo.apply info.proof_of_node info node)

(*
 * Create the denormalizer.
 *)
let id _ x = x

let create_denorm () =
   let term_info = Term_copy.create_denorm () in
   let denorm = Term_copy.denormalize_term term_info in
      { denorm = denorm;
        step_denorm = Proof_step.create_denorm denorm;
        proof_of_node = Memo.create make_proof id compare_proof;
        proof_node_of_node_item = Memo.create make_proof_node id compare_proof_node;
        proof_child_of_child_node = Memo.create make_proof_child id compare_proof_child
      }

let io_proof_of_proof { pf_root = root } =
   let info = create_denorm () in
      Memo.apply info.proof_of_node info root

(*
 * Build a proof from an IO proof.
 *)
let status_of_io_status = function
   Io_proof_type.StatusBad ->
      Bad
 | Io_proof_type.StatusPartial ->
      Partial
 | Io_proof_type.StatusAsserted ->
      Asserted
 | Io_proof_type.StatusComplete ->
      Complete

let make_proof info { proof_status = status;
                      proof_step = step;
                      proof_children = children;
                      proof_extras = extras
    } =
   { node_status = status_of_io_status status;
     node_item = Memo.apply info.node_item_of_proof_node info step;
     node_children = List.map (Memo.apply info.child_node_of_proof_child info) children;
     node_extras = List.map (Memo.apply info.node_of_proof info) extras
   }

let make_node_item info = function
   Io_proof_type.ProofStep step ->
      Step (Proof_step.step_of_io_step info.step_norm step)
 | Io_proof_type.ProofNode proof ->
      Node (Memo.apply info.node_of_proof info proof)

let make_child_node info = function
   Io_proof_type.ChildGoal goal ->
      ChildGoal (Proof_step.tactic_arg_of_aterm info.step_norm goal)
 | Io_proof_type.ChildProof proof ->
      ChildNode (Memo.apply info.node_of_proof info proof)

(*
 * Create the state.
 *)
let proof_of_io_proof arg parser create_tactic sentinal pf =
   let { ref_fcache = fcache; ref_args = args } = arg in
   let term_info = Term_copy.create_norm () in
   let norm = Term_copy.normalize_term term_info in
   let info =
      { norm = norm;
        step_norm = Proof_step.create_norm norm arg parser create_tactic sentinal;
        node_of_proof = Memo.create id make_proof compare_proof;
        node_item_of_proof_node = Memo.create id make_node_item compare_proof_node;
        child_node_of_proof_child = Memo.create id make_child_node compare_proof_child
      }
   in
   let node = Memo.apply info.node_of_proof info pf in
      { pf_root = node;
        pf_address = [];
        pf_node = node
      }

(*
let rec io_child_of_child = function
   ChildGoal goal ->
      let seq = Sequent.msequent goal in
      let t, hyps = dest_msequent seq in
         Io_proof_type.ChildGoal (**)
            { Io_proof_type.aterm_goal = t;
              Io_proof_type.aterm_hyps = hyps;
              Io_proof_type.aterm_label = Sequent.label goal;
              Io_proof_type.aterm_args = io_attributes (Sequent.attributes goal)
            }
 | ChildNode node ->
      Io_proof_type.ChildProof (io_proof_of_node node)

and io_node_of_item = function
   Step step ->
      Io_proof_type.ProofStep (Proof_step.io_step_of_step step)
 | Node node ->
      Io_proof_type.ProofNode (io_proof_of_node node)

and io_proof_of_node
    { node_status = status;
      node_item = item;
      node_children = children;
      node_extras = extras
    } =
   { Io_proof_type.proof_status = io_status_of_status status;
     Io_proof_type.proof_step = io_node_of_item item;
     Io_proof_type.proof_children = List.map io_child_of_child children;
     Io_proof_type.proof_extras = List.map io_proof_of_node extras
   }

let io_proof_of_proof { pf_node = node } =
   io_proof_of_node node

(*
 * Restore an io proof.
 *)
let status_of_io_status = function
   Io_proof_type.StatusBad ->
      Bad
 | Io_proof_type.StatusPartial ->
      Partial
 | Io_proof_type.StatusAsserted ->
      Asserted
 | Io_proof_type.StatusComplete ->
      Complete

let proof_of_io_proof arg tacs sentinal pf =
   let { ref_fcache = fcache; ref_args = args } = arg in
   let hash = Hashtbl.create (max (Array.length tacs) 17) in
   let _ = Array.iter (function (name, tac) -> Hashtbl.add hash name tac) tacs in
   let rec child_of_io_child = function
      Io_proof_type.ChildGoal (**)
         { Io_proof_type.aterm_goal = goal;
           Io_proof_type.aterm_hyps = hyps;
           Io_proof_type.aterm_label = label;
           Io_proof_type.aterm_args = args'
         } ->
         ChildGoal (Sequent.create sentinal label (mk_msequent goal hyps) fcache (args' @ args))
    | Io_proof_type.ChildProof pf ->
         ChildNode (node_of_io_proof pf)

   and item_of_io_node = function
      Io_proof_type.ProofStep step ->
         Step (Proof_step.step_of_io_step arg hash sentinal step)
    | Io_proof_type.ProofNode node ->
         Node (node_of_io_proof node)

   and node_of_io_proof
       { Io_proof_type.proof_status = status;
         Io_proof_type.proof_step = item;
         Io_proof_type.proof_children = children;
         Io_proof_type.proof_extras = extras
       } =
      { node_status = status_of_io_status status;
        node_item = item_of_io_node item;
        node_children = List.map child_of_io_child children;
        node_extras = List.map node_of_io_proof extras
      }
   in
   let node = node_of_io_proof pf in
      { pf_root = node;
        pf_address = [];
        pf_node = node
      }
*)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
