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
open Refiner.Refiner.Refine

open Filter_ocaml

open Tactic_type
open Io_proof_type

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_proof%t" eflush


let summary_opname = mk_opname "Summary" nil_opname

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

let proof_var_args_op      = mk_opname "proof_var_args"    summary_opname
let proof_term_args_op     = mk_opname "proof_term_args"   summary_opname
let proof_type_arg_op      = mk_opname "proof_type_args"   summary_opname
let proof_int_args_op      = mk_opname "proof_int_args"    summary_opname
let proof_thin_op          = mk_opname "proof_thin"        summary_opname
let proof_dont_thin_op     = mk_opname "proof_dont_thin"   summary_opname
let proof_subst_arg_op     = mk_opname "proof_subst_arg"   summary_opname

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

let mk_msequent_term { mseq_goal = goal; mseq_hyps = hyps } =
   mk_xlist_term (goal :: hyps)

let dest_msequent t =
   match dest_xlist t with
      h::t ->
         { mseq_goal = h; mseq_hyps = t }
    | [] ->
         raise (Failure "dest_msequent")

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
      mk_simple_term proof_step_op [term_of_aterm_tactic_arg goal;
                                    mk_xlist_term (List.map term_of_aterm_tactic_arg subgoals);
                                    term_of_expr [] comment ast;
                                    mk_simple_string_term proof_step_op text []]
 | ProofNode proof ->
      mk_simple_term proof_node_op [term_of_proof proof]

and term_of_proof_child = function
   ChildGoal goal ->
      mk_simple_term proof_child_goal_op [term_of_aterm_tactic_arg goal]
 | ChildProof proof ->
      mk_simple_term proof_child_proof_op [term_of_proof proof]

and term_of_aterm_tactic_arg
    { tac_goal = goal;
      tac_hyps = hyps;
      tac_arg = { aterm_label = label;
                  aterm_args = args
                }
    } =
   mk_simple_term proof_aterm_op [goal;
                                  mk_xlist_term hyps;
                                  mk_simple_string_term proof_aterm_op label [];
                                  mk_xlist_term (List.map term_of_attribute args)]

and term_of_attribute = function
   VarArgs sl ->
      mk_simple_term proof_var_args_op (**)
         [mk_xlist_term (List.map (fun s -> mk_simple_string_term proof_var_args_op s []) sl)]
 | TermArgs tl ->
      mk_simple_term proof_term_args_op [mk_xlist_term tl]
 | TypeArg t ->
      mk_simple_term proof_type_arg_op [t]
 | IntArgs il ->
      mk_simple_term proof_int_args_op (**)
         [mk_xlist_term (List.map (fun i -> mk_simple_int_term proof_int_args_op i []) il)]
 | ThinArg flag ->
      let op =
         if flag then
            proof_thin_op
         else
            proof_dont_thin_op
      in
         mk_simple_term op []
 | SubstArg stl ->
      mk_simple_term proof_subst_arg_op (**)
         [mk_xlist_term (List.map (fun (s, t) -> mk_simple_string_term proof_subst_arg_op s [t]) stl)]

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
            ProofStep { step_goal = aterm_tactic_arg_of_term goal;
                        step_subgoals = List.map aterm_tactic_arg_of_term (dest_xlist subgoals);
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
         ChildGoal (aterm_tactic_arg_of_term (one_subterm t))
      else if op == proof_child_proof_op then
         ChildProof (proof_of_term (one_subterm t))
      else
         raise (Failure "Filter_proof.child_of_term")

and aterm_tactic_arg_of_term t =
   let goal, hyps, label, args = four_subterms t in
      { tac_goal = goal;
        tac_hyps = dest_xlist hyps;
        tac_arg = { aterm_label = dest_string_param label;
                    aterm_args = List.map attribute_of_term (dest_xlist args)
                  }
      }

and attribute_of_term t =
   let op = opname_of_term t in
      if op == proof_var_args_op then
         VarArgs (List.map dest_string_param (dest_xlist (one_subterm t)))
      else if op == proof_term_args_op then
         TermArgs (dest_xlist (one_subterm t))
      else if op == proof_type_arg_op then
         TypeArg (one_subterm t)
      else if op == proof_int_args_op then
         IntArgs (List.map (fun t -> Num.int_of_num (dest_number_any_term t)) (dest_xlist (one_subterm t)))
      else if op == proof_thin_op then
         ThinArg true
      else if op == proof_dont_thin_op then
         ThinArg false
      else if op == proof_subst_arg_op then
         SubstArg (List.map (dest_string_dep0_term proof_subst_arg_op) (dest_xlist (one_subterm t)))
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
 * Revision 1.2  1998/06/01 13:52:08  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 13:45:34  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.7  1998/05/04 13:01:13  jyh
 * Ocaml display without let rec.
 *
 * Revision 1.6  1998/04/28 21:37:59  jyh
 * Adjusted uppercasing.
 *
 * Revision 1.5  1998/04/24 19:38:31  jyh
 * Updated debugging.
 *
 * Revision 1.4  1998/04/24 02:42:05  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.3  1998/04/22 22:44:26  jyh
 * *** empty log message ***
 *
 * Revision 1.2  1998/04/21 20:58:03  jyh
 * Fixed typing problems introduced by refiner msequents.
 *
 * Revision 1.1  1998/04/17 01:31:02  jyh
 * Editor is almost constructed.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
