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

open Printf

open Term
open Refine
open Proof_step

include Itt_theory

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
type proof_item =
   ProofStep of Proof_step.t
 | ProofProof of proof

(*
 * Status of a proof node.
 *)
and proof_status =
   StatusBad
 | StatusPartial
 | StatusAsserted
 | StatusComplete

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
and proof_node =
   { node_status : proof_status;
     node_item : node_item;
     node_children : child_node list;
     node_extras : proof_node list
   }

and node_item =
   NodeStep of Proof_step.t
 | NodeNode of proof_node

and child_node =
   ChildGoal of tactic_arg
 | ChildNode of proof_node

and proof_child =
   ProofChildTerm of tactic_arg
 | ProofChildProof of proof

(*
 * The actual "proof" is a handle, with a reference
 * to the root of the proof, the current address, and
 * a pointer to the current proof node.
 *
 * The invariant here is:
 *    pf_root[pf_address] = pf_node
 *)
and proof =
   { pf_root : proof_node;
     pf_address : address;
     pf_node : proof_node
   }

(*
 * IO tree uses handles to proof steps.
 *)
magic_block magic_number =
struct
   type io_node_item =
      IONodeStep of Proof_step.handle
    | IONodeNode of io_proof_node
   
   and io_proof_node =
      { io_node_item : io_node_item;
        io_node_children : io_proof_node list
      }
   
   (* A handle is an index into the array *)
   type handle = int

   (* The in_base is an array mapping numbers to proofs *)
   type in_base =
      { in_proofs : io_proof_node array;
        in_steps : Proof_step.in_base
      }
end

(*
 * The out_base is just a list of proofs.
 *)
type out_base =
   { mutable out_proofs : io_proof_node list;
     mutable out_count : int;
     out_steps : Proof_step.out_base
   }

(*
 * This exception is raised when composed proofs don't match
 *)
exception ProofMatch

(*
 * Can't get a child at a bogus address.
 *)
exception InvalidAddress of proof * int

(*
 * This is a local exception.
 *)
exception NodeInvalidAddress of int list * int * proof_node

(************************************************************************
 * CONSTRUCTORS                                                         *
 ************************************************************************)

(*
 * Make a proof from a step.
 *)
let proof_of_step step =
   let subgoals = step_subgoals step in
   let status =
      if subgoals = [] then
         StatusComplete
      else
         StatusPartial
   in
   let children = List.map (function t -> ChildGoal t) subgoals in
   let node =
      { node_status = status;
	node_item = NodeStep step;
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
      NodeStep step -> step_goal step
    | NodeNode node -> node_goal node

let rec node_subgoals { node_children = children } =
   let flatten = function
      ChildGoal goal -> [goal]
    | ChildNode node -> node_subgoals node
   in
      List_util.flat_map flatten children

(*
 * Proof info.
 *)
let proof_goal { pf_node = node } = node_goal node

let proof_subgoals { pf_node = node } = node_subgoals node

(*
 * Walking the tree.
 *)
let proof_item { pf_root = root; pf_address = addr; pf_node = node } =
   match node.node_item with
      NodeStep step ->
         ProofStep step
    | NodeNode node ->
         ProofProof { pf_root = root;
                      pf_address = addr @ [0];
                      pf_node = node
	 }

(*
 * Get the children of a proof.
 *)
let proof_children
    { pf_root = root;
      pf_address = addr;
      pf_node = { node_children = children }
    } =
   let rec collect i = function
      h::t ->
         let t' = collect (i + 1) t in
         let h' =
            match h with
               ChildGoal goal ->
                  ProofChildTerm goal
             | ChildNode node ->
                  ProofChildProof { pf_root = root; pf_address = addr @ [i]; pf_node = node }
         in
            h' :: t'
    | [] -> []
   in
      collect 1 children

(*
 * Select the next node specified by the next int in the address.
 *)
let select node i =
   if i = 0 then
      match node with
         { node_item = NodeNode node' } ->
            node'
       | { node_item = _ } ->
            raise (Invalid_argument "proof_index")
   else
      match List.nth node.node_children (i - 1) with
         ChildGoal _ -> raise (Invalid_argument "proof_index")
       | ChildNode node -> node

(*
 * Follow the complete address.
 * If the address is not valid, raise an error with the
 * prefix of the address that worked, plus the point where it
 * failed.
 *)
let index node addr =
   let rec aux node = function
      i::t ->
         let node' =
            try select node i with
               Invalid_argument _ ->
                  raise (NodeInvalidAddress (List_util.remove_suffix addr (i::t), i, node))
         in
            aux node' t
    | [] ->
         node
   in
      aux node addr

(*
 * Index from a proof node.
 * Again, on failure, interprete the failure,
 * and return the point where the address failed.
 *)
let proof_index { pf_root = root; pf_address = addr; pf_node = node } addr' =
   let node' =
      try index node addr' with
         NodeInvalidAddress (addr'', step, node') ->
            let pf' = { pf_root = root; pf_address = addr @ addr''; pf_node = node } in
               raise (InvalidAddress (pf', step))
   in
      { pf_root = root;
        pf_address = addr @ addr';
        pf_node = node'
      }

let proof_child { pf_root = root; pf_address = addr; pf_node = node } i =
   let node' = select node (i + 1) in
      { pf_root = root;
        pf_address = addr @ [i + 1];
        pf_node = node'
      }

(*
 * Back up to parent.
 *)
let proof_parent { pf_root = root; pf_address = addr } =
   if addr = [] then
      None
   else
      let addr', _ = List_util.split_last addr in
      let node' = index root addr' in
         Some { pf_root = root;
                pf_address = addr';
                pf_node = node'
              }

(*
 * Get the main goal.
 *)
let proof_main { pf_root = root } =
   { pf_root = root; pf_address = []; pf_node = root }
   
(*
 * Address relative to the main goal.
 *)
let proof_address { pf_address = addr } = addr

(*
 * Get the status too.
 *)
let proof_status { pf_root = root; pf_address = addr } =
   let rec aux node addr =
      let status = node.node_status in
         match addr with
            i::t ->
               (* This can't fail! *)
               let node' = select node i in
                  (status, i)::(aux node' t)
          | [] ->
               [status, 0]
   in
      aux root addr

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
         NodeStep step ->
            if step_subgoals step = [] then
               StatusComplete
            else
               StatusPartial
       | NodeNode { node_status = status } ->
            status
   in

   (* Status of a single child *)
   let child_status = function
      ChildGoal _ -> StatusPartial
    | ChildNode { node_status = status } -> status
   in

   (* Minimize status across all the children *)
   let min_status status1 child =
      let status2 = child_status child in
         match status1 with
            StatusComplete -> status2
          | StatusAsserted ->
	    begin
	       match status2 with
		  StatusComplete -> status1
		| _ -> status2
	    end
          | StatusPartial ->
	    begin
	       match status2 with
		  StatusComplete | StatusAsserted -> status1
		| _ -> status2
	    end
          | StatusBad -> StatusBad
   in
   let cstatus = List.fold_left min_status StatusComplete children in
      (* Combine the status of the item and of the children *)
      match istatus with
         StatusComplete | StatusAsserted ->
	 begin
	    match cstatus with
	       StatusComplete -> istatus
	     | _ -> raise (Invalid_argument "compute_status")
	 end
       | StatusPartial -> cstatus
       | StatusBad -> StatusBad

(*
 * Replace a node in the tree functionally.
 * It is guaranteed that node' has the same goal as node.
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
      i::t ->
         if i = 0 then
            (* Replace the interior node *)
            match item with
	       NodeNode node' ->
                  let item' = NodeNode (replace node' t) in
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
            let children' = List_util.replacef_nth children (i - 1) replace in
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
   [] -> []
 | h::t ->
      match h with
         ChildGoal _ ->
            child_nodes t
       | ChildNode n ->
            n :: child_nodes t

(*
 * This is the common case.
 *)
let join_order_children =
   let join_child subgoal = function
      ChildGoal _ ->
         ChildGoal subgoal
    | ChildNode node ->
	 let subgoal', _ = node_goal node in
	    if alpha_equal subgoal' (fst subgoal) then
	       ChildNode node
	    else
	       raise ProofMatch
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
 * May later upgrade this to alpha-equality, or
 * generalization, or something else.
 *)
let join_permuted_children subgoals children =
   (* Use a hash function to help matching *)
   let make_index1 subgoal =
      Hashtbl.hash (fst subgoal), subgoal
   in
   let make_index2 node =
      let goal = node_goal node in
	 Hashtbl.hash (fst goal), goal, node
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
let join_children subgoals children =
   try join_order_children subgoals children with
      ProofMatch -> join_permuted_children subgoals (child_nodes children)

(*
 * Replace the proof item with another.  The goal of the new item
 * must match the old goal, and the new subgoals should match those of
 * any children that exist.
 *)
let replace_item pf = function
   (*
    * When the node is replaced with a step,
    * check that the subgoals match up.
    *)
   ProofStep step ->
      let { pf_node = node } = pf in
      let { node_children = children; node_extras = extras } = node in
      let _ =
	 if not (fst (node_goal node) = fst (step_goal step)) then
	    raise ProofMatch
      in
      let subgoals = step_subgoals step in
      let children', extras' = join_children subgoals children in
      let item' = NodeStep step in
      let status' = compute_status item' children' in
      let node' =
	 { node_status = status';
	   node_item = item';
	   node_children = children';
	   node_extras = extras @ extras'
	 }
      in
	 replace_node pf node'

 | ProofProof pf' ->
      let { pf_node = node } = pf in
      let { pf_node = node' } = pf' in
      let { node_children = children; node_extras = extras } = node in
      let _ =
	 if not (fst (node_goal node) = fst (node_goal node')) then
	    raise ProofMatch
      in
      let subgoals = node_subgoals node' in
      let create_child subgoal = function
	 ChildGoal _ ->
            ChildGoal subgoal
       | ChildNode node ->
            if fst subgoal = fst (node_goal node) then
               ChildNode node
            else
               raise ProofMatch
      in
      let children' =
         try List.map2 create_child subgoals children with
            Invalid_argument "map2" -> raise ProofMatch
      in
      let item' = NodeNode node' in
      let status' = compute_status item' children' in
      let node' =
	 { node_status = status';
	   node_item = item';
	   node_children = children';
	   node_extras = extras
	 }
      in
	 replace_node pf node'

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
   let goal', _ = node_goal child' in
   let replace = function
      ChildGoal goal ->
         if alpha_equal (fst goal) goal' then
            ChildNode child'
         else
            raise ProofMatch
    | ChildNode node ->
         if alpha_equal (fst (node_goal node)) goal' then
            ChildNode child'
         else
            raise ProofMatch
   in
   let children' = List_util.replacef_nth children i replace in
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
   let children' = List_util.replacef_nth children i replace in
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
      NodeStep step -> step_subgoals step
    | NodeNode node -> node_subgoals node
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

(************************************************************************
 * IO                                                                   *
 ************************************************************************)

(*
 * An out_base is just a collection of the proofs.
 *)
let create_out_base () =
   { out_proofs = []; out_count = 0; out_steps = Proof_step.create_out_base () }

(*
 * Save a proof in the table.
 * Convert it to an IO proof.
 * We just save the node, not the main proof.
 *)
let save_proof base { pf_node = node } =
   let { out_proofs = proofs; out_count = count; out_steps = steps } = base in
   let rec convert { node_item = item; node_children = children; node_extras = extras } =
      let item' =
         match item with
            NodeStep step ->
               IONodeStep (save_step steps step)
          | NodeNode node' ->
               IONodeNode (convert node')
      in
      let rec convert_children = function
         [] ->
            List.map convert extras
       | child::t ->
            match child with
               ChildGoal _ ->
                  convert_children t
             | ChildNode node ->
                  convert node :: convert_children t
      in
         { io_node_item = item';
           io_node_children = convert_children children
         }
   in
   let ionode = convert node in
      base.out_proofs <- ionode :: proofs;
      base.out_count <- count + 1;
      count

(*
 * Save the base into a file.
 *)
let save_base { out_proofs = proofs; out_steps = steps } out =
   output_binary_int out magic_number;
   Proof_step.save_base steps out;
   output_value out (Array.of_list (List.rev proofs))

(*
 * Get the tactics from the step base.
 *)
let restore_tactics inx =
   try 
      let magic' = input_binary_int inx in
         if magic' = magic_number then
            Proof_step.restore_tactics inx
         else
            raise (Failure "Proof.restore_tactics: bad magic number")
   with
      End_of_file ->
         raise (Failure "Proof.restore_tactics: premature end of file")

(*
 * Restore the entire base.
 *)
let restore_base inx =
   try 
      let magic' = input_binary_int inx in
         if magic' = magic_number then
            let steps = Proof_step.restore_base inx in
            let proofs = (input_value inx : io_proof_node array) in
               { in_proofs = proofs; in_steps = steps }
         else
            raise (Failure "Proof.restore_base: bad magic number")
   with
      End_of_file ->
         raise (Failure "Proof.restore_base: premature end of file")

(*
 * Restore a proof from its handle.
 * Have to match the children with the subgoals.
 *)
let restore_proof { in_proofs = proofs; in_steps = steps } resources tacs hand =
   let rec restore { io_node_item = item; io_node_children = children } =
      let item', subgoals =
         match item with
            IONodeStep hand ->
               let step = restore_step steps resources tacs hand in
               let subgoals = step_subgoals step in
                  NodeStep step, subgoals
          | IONodeNode node ->
               let node' = restore node in
               let subgoals = node_subgoals node' in
                  NodeNode node', subgoals
      in
      let children' = List.map restore children in
      let children'', extras = join_permuted_children subgoals children' in
         { node_status = compute_status item' children'';
           node_item = item';
           node_children = children'';
           node_extras = extras
         }
   in
   let node = restore proofs.(hand) in
      { pf_root = node;
        pf_address = [];
        pf_node = node
      }

(*
 * $Log$
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
