(*
 * Marshal proofs to terms.
 *)

include Io_proof_type

open Printf
open Debug

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan

open Filter_ocaml

open Tactic_type
open Io_proof_type

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_proof%t" eflush


let summary_opname         = mk_opname "Summary" nil_opname

let interface_op           = mk_opname "interface"         summary_opname
let implementation_op      = mk_opname "implementation"    summary_opname

let interactive_op         = mk_opname "interactive"       summary_opname

let status_bad_op          = mk_opname "status_bad"        summary_opname
let status_partial_op      = mk_opname "status_partial"    summary_opname
let status_asserted_op     = mk_opname "status_asserted"   summary_opname
let status_complete_op     = mk_opname "status_complete"   summary_opname

let proof_step_op          = mk_opname "proof_step"        summary_opname
let proof_node_op          = mk_opname "proof_node"        summary_opname

let proof_child_goal_op    = mk_opname "proof_child_goal"  summary_opname
let proof_child_proof_op   = mk_opname "proof_child_proof" summary_opname

let proof_aterm_op         = mk_opname "proof_aterm"       summary_opname

let proof_term_arg_op      = mk_opname "proof_term_arg"    summary_opname
let proof_type_arg_op      = mk_opname "proof_type_arg"    summary_opname
let proof_int_arg_op       = mk_opname "proof_int_arg"     summary_opname
let proof_bool_arg_op      = mk_opname "proof_bool_arg"    summary_opname
let proof_subst_arg_op     = mk_opname "proof_subst_arg"   summary_opname
let proof_tactic_arg_op    = mk_opname "proof_tactic_arg"  summary_opname

(*
 * Make a term with a string parameter.
 *)
let mk_simple_string_term opname s terms =
   let param = make_param (String s) in
   let op = mk_op opname [param] in
   let bterms = List.map (fun t -> mk_bterm [] t) terms in
      mk_term op bterms

let mk_simple_int_term opname i terms =
   let param = make_param (Number (Num.Int i)) in
   let op = mk_op opname [param] in
   let bterms = List.map (fun t -> mk_bterm [] t) terms in
      mk_term op bterms

let mk_string_string_term opname s1 s2 =
   let param1 = make_param (String s1) in
   let param2 = make_param (String s2) in
   let op = mk_op opname [param1; param2] in
      mk_term op []

let mk_string_int_term opname s i =
   let param1 = make_param (String s) in
   let param2 = make_param (Number (Num.Int i)) in
   let op = mk_op opname [param1; param2] in
      mk_term op []

let dest_string_string_term t =
   let { term_op = op } = dest_term t in
   let { op_params = params } = dest_op op in
      match List.map dest_param params with
         [String s1; String s2] ->
            s1, s2
       | _ ->
            raise (Failure "dest_string_string_term")

let dest_string_int_term t =
   let { term_op = op } = dest_term t in
   let { op_params = params } = dest_op op in
      match List.map dest_param params with
         [String s; Number n] ->
            s, Num.int_of_num n
       | _ ->
            raise (Failure "dest_string_string_term")

let comment _ t = t

(*
 * Convert a proof to a term.
 *)
let rec term_of_proof
    { proof_status = status;
      proof_step = step;
      proof_children = children;
      proof_extras = extras
    } =
   mk_simple_term interactive_op [term_of_status status;
                                  term_of_proof_step step;
                                  mk_xlist_term (List.map term_of_proof_child children);
                                  mk_xlist_term (List.map term_of_proof extras)]

and term_of_status status =
   let op =
      match status with
         StatusBad ->
            status_bad_op
       | StatusPartial ->
            status_partial_op
       | StatusAsserted ->
            status_asserted_op
       | StatusComplete ->
            status_complete_op
   in
      mk_simple_term op []

and term_of_proof_step = function
   ProofStep { step_goal = goal;
               step_subgoals = subgoals;
               step_ast = ast;
               step_text = text
   } ->
      mk_simple_term proof_step_op [term_of_aterm goal;
                                    mk_xlist_term (List.map term_of_aterm subgoals);
                                    term_of_expr [] comment ast;
                                    mk_simple_string_term proof_step_op text []]
 | ProofNode proof ->
      mk_simple_term proof_node_op [term_of_proof proof]

and term_of_proof_child = function
   ChildGoal goal ->
      mk_simple_term proof_child_goal_op [term_of_aterm goal]
 | ChildProof proof ->
      mk_simple_term proof_child_proof_op [term_of_proof proof]

and term_of_aterm
    { aterm_goal = goal;
      aterm_hyps = hyps;
      aterm_label = label;
      aterm_args = args
    } =
   mk_simple_term proof_aterm_op [goal;
                                  mk_xlist_term hyps;
                                  mk_simple_string_term proof_aterm_op label [];
                                  mk_xlist_term (List.map term_of_attribute args)]

and term_of_attribute (name, att) =
   match att with
      TermArg t ->
         mk_simple_string_term proof_term_arg_op name [t]
    | TypeArg t ->
         mk_simple_string_term proof_type_arg_op name [t]
    | IntArg i ->
         mk_string_int_term proof_int_arg_op name i
    | BoolArg flag ->
         let s =
            if flag then
               "true"
            else
               "false"
         in
            mk_string_string_term proof_bool_arg_op name s
    | SubstArg t ->
         mk_simple_string_term proof_subst_arg_op name [t]
    | TacticArg tac ->
         eprintf "Saved proof has a tactic argument that will not be saved.%t" eflush;
         mk_simple_string_term proof_tactic_arg_op name []

(*
 * Convert the term back into a proof.
 *)
let rec proof_of_term t =
   let status, step, children, extras = four_subterms t in
      { proof_status = status_of_term status;
        proof_step = step_of_term step;
        proof_children = List.map child_of_term (dest_xlist children);
        proof_extras = List.map proof_of_term (dest_xlist extras)
      }

and status_of_term t =
   let op = opname_of_term t in
      if op == status_bad_op then
         StatusBad
      else if op == status_partial_op then
         StatusPartial
      else if op == status_asserted_op then
         StatusAsserted
      else if op == status_complete_op then
         StatusComplete
      else
         raise (Failure "Filter_proof.status_of_term")

and step_of_term t =
   let op = opname_of_term t in
      if op == proof_step_op then
         let goal, subgoals, ast, text = four_subterms t in
            ProofStep { step_goal = aterm_of_term goal;
                        step_subgoals = List.map aterm_of_term (dest_xlist subgoals);
                        step_ast = expr_of_term ast;
                        step_text = dest_string_param text
            }
      else if op == proof_node_op then
         ProofNode (proof_of_term t)
      else
         raise (Failure "Filter_proof.step_of_term")

and child_of_term t =
   let op = opname_of_term t in
      if op == proof_child_goal_op then
         ChildGoal (aterm_of_term (one_subterm t))
      else if op == proof_child_proof_op then
         ChildProof (proof_of_term (one_subterm t))
      else
         raise (Failure "Filter_proof.child_of_term")

and aterm_of_term t =
   let goal, hyps, label, args = four_subterms t in
      { aterm_goal = goal;
        aterm_hyps = dest_xlist hyps;
        aterm_label = dest_string_param label;
        aterm_args = List.map attribute_of_term (dest_xlist args)
      }

and attribute_of_term t =
   let op = opname_of_term t in
      if op == proof_term_arg_op then
         let name, t = dest_string_dep0_term op t in
            name, TermArg t
      else if op == proof_type_arg_op then
         let name, t = dest_string_dep0_term op t in
            name, TypeArg t
      else if op == proof_int_arg_op then
         let name, i = dest_string_int_term t in
            name, IntArg i
      else if op == proof_bool_arg_op then
         let name, flag = dest_string_string_term t in
            name, BoolArg (flag <> "false")
      else if op == proof_subst_arg_op then
         let name, t = dest_string_dep0_term op t in
            name, SubstArg t
      else if op == proof_tactic_arg_op then
         let name = dest_string_term op t in
            name, TacticArg (Tacticals.failWithT "can't restore tactic from file")
      else
         raise (Failure "Filter_proof.attribute_of_term")

(************************************************************************
 * TACTIC HANDLING                                                      *
 ************************************************************************)

(*
 * Get tactic array.
 *)
let tactics_of_proof proof =
   let hash = Hashtbl.create 107 in
   let rec collect_proof
       { proof_step = step;
         proof_children = children
       } =
      collect_proof_step step;
      List.iter collect_proof_child children

   and collect_proof_step = function
      ProofStep { step_ast = ast; step_text = text } ->
         Hashtbl.remove hash text;
         Hashtbl.add hash text ast
    | ProofNode proof ->
         collect_proof proof

   and collect_proof_child = function
      ChildGoal goal ->
         ()
    | ChildProof proof ->
         collect_proof proof
   in

   let loc = (0, 0) in
   let entries = ref [] in
   let collect name tac =
      let name = <:expr< $str: name$ >> in
      let pair = <:expr< ( $list: [ name; tac ]$ ) >> in
         entries := pair :: !entries
   in
   let _ = collect_proof proof in
   let _ = Hashtbl.iter collect hash in
      <:expr< [| $list: !entries$ |] >>

(*
 * $Log$
 * Revision 1.4  1998/06/09 20:51:09  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.3  1998/06/03 22:19:07  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.2  1998/06/01 13:52:08  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 13:45:34  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
