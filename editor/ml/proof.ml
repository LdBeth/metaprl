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
 *)

include Tactic_type

include Io_proof_type
include Proof_step

open Printf
open Debug

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.Refine
open Refine_exn
open Tactic_type

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
exception ProofRefineError of t * refine_error

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
      try
         match List.nth node.node_children (i - 1) with
            ChildGoal _ ->
               raise (Failure "Proof.select")
          | ChildNode node ->
               node
      with
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
	 let { tac_goal = subgoal' } = node_goal node in
	    if alpha_equal subgoal' subgoal.tac_goal then
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
 * NOTE: may later upgrade this to alpha-equality, or
 * generalization, or something else.
 *)
let join_permuted_children subgoals children =
   (* Use a hash function to help matching *)
   let make_index1 subgoal =
      Hashtbl.hash subgoal.tac_goal, subgoal
   in
   let make_index2 node =
      let goal = node_goal node in
	 Hashtbl.hash goal.tac_goal, goal, node
   in
   let indices1 = List.map make_index1 subgoals in
   let indices2 = List.map make_index2 children in
      
   (* Cross reference index1 to index2 *)
   let find_match index subgoal =
      let rec find = function
	 (index', goal, node) :: indices2 ->
	    if index = index' & subgoal = goal then
	       ChildNode node, indices2
	    else
	       let child, indices' = find indices2 in
		  child, (index', goal, node) :: indices'
       | [] ->
	    ChildGoal subgoal, []
      in
	 find
   in
      let rec crossref indices2 = function
	 (index, subgoal) :: indices1 ->
	    let child, indices2' = find_match index subgoal indices2 in
	    let children', extras = crossref indices2' indices1 in
	       child :: children', extras
       | [] ->
	    [], List.map (function _, _, node -> node) indices2
   in
      crossref indices2 indices1

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
         { node_status = compute_status item children;
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
   let { tac_goal = goal' } = node_goal child' in
   let replace = function
      ChildGoal goal ->
         if alpha_equal goal.tac_goal goal' then
            ChildNode child'
         else
            raise Match
    | ChildNode node ->
         if alpha_equal (node_goal node).tac_goal goal' then
            ChildNode child'
         else
            raise Match
   in
   let children' =
      try List_util.replacef_nth i replace children with
         Failure "replacef_nth" ->
            raise Match
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
            raise (ProofRefineError (mk_pf addr' node, (StringError "Proof is not complete")))
      in
      let rec fold_child i = function
         child :: childt ->
            check_child (i :: addr) node child :: fold_child (i + 1) childt
       | [] ->
            []
      in
      let extl = fold_child 1 children in
         try Refine.compose ext extl with
            RefineError err ->
               raise (ProofRefineError (mk_pf addr' node, err))

   and check_step addr' node step =
      try Proof_step.check step with
         RefineError err ->
            raise (ProofRefineError (mk_pf (List.tl addr') node, err))

   and check_child addr' node = function
      ChildGoal goal ->
         (* This can't happen if the status were set correctly *)
         raise (ProofRefineError (mk_pf addr' node, StringError "Proof is not complete"))
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

(************************************************************************
 * IO                                                                   *
 ************************************************************************)

(*
 * Make an io proof.
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

let rec io_child_of_child = function
   ChildGoal { tac_goal = t;
               tac_hyps = hyps;
               tac_arg = { ref_label = label; ref_args = args } } ->
      Io_proof_type.ChildGoal (**)
         { tac_goal = t;
           tac_hyps = hyps;
           tac_arg = { Io_proof_type.aterm_label = label;
                       Io_proof_type.aterm_args = args
                     }
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

let proof_of_io_proof arg tacs pf =
   let { ref_rsrc = resources; ref_fcache = fcache } = arg in
   let hash = Hashtbl.create (Array.length tacs) in
   let _ = Array.iter (function (name, tac) -> Hashtbl.add hash name tac) tacs in
   let rec child_of_io_child = function
      Io_proof_type.ChildGoal (**)
         { tac_goal = goal;
           tac_hyps = hyps;
           tac_arg = { Io_proof_type.aterm_label = label;
                       Io_proof_type.aterm_args = args
                     }
         } ->
         ChildGoal { tac_goal = goal;
                     tac_hyps = hyps;
                     tac_arg = { ref_label = label;
                                 ref_args = args;
                                 ref_fcache = fcache;
                                 ref_rsrc = resources
                               }
         }
    | Io_proof_type.ChildProof pf ->
         ChildNode (node_of_io_proof pf)

   and item_of_io_node = function
      Io_proof_type.ProofStep step ->
         Step (Proof_step.step_of_io_step arg hash step)
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

(*
 * $Log$
 * Revision 1.9  1998/05/28 13:45:46  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.8  1998/04/28 18:29:44  jyh
 * ls() works, adding display.
 *
 * Revision 1.7  1998/04/24 02:41:29  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.6  1998/04/23 20:03:47  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.5  1998/04/22 22:44:17  jyh
 * *** empty log message ***
 *
 * Revision 1.4  1998/04/17 01:30:44  jyh
 * Editor is almost constructed.
 *
 * Revision 1.3  1998/04/13 21:10:53  jyh
 * Added interactive proofs to filter.
 *
 * Revision 1.2  1998/04/09 19:07:23  jyh
 * Updating the editor.
 *
 * Revision 1.1  1997/08/06 16:17:22  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/09/02 19:33:30  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:36  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 17:00:05  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:19  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
