(*
 * Some utilities for refiners.
 *)

open Term
open Refine_sig

(*
 * Compare two sequents for alpha eqivalence.
 *)
let msequent_alpha_equal
    { mseq_goal = goal1; mseq_hyps = hyps1 }
    { mseq_goal = goal2; mseq_hyps = hyps2 } =
   let rec compare = function
      hyp1::hyps1, hyp2::hyps2 ->
         alpha_equal hyp1 hyp2 & compare (hyps1, hyps2)
    | [], [] ->
         true
    | _ ->
         false
   in
      alpha_equal goal1 goal2 & compare (hyps1, hyps2)
   
(*
 * Compare two sequents for alpha eqivalence.
 *)
let tactic_arg_alpha_equal
    { tac_goal = goal1; tac_hyps = hyps1 }
    { tac_goal = goal2; tac_hyps = hyps2 } =
   let rec compare = function
      hyp1::hyps1, hyp2::hyps2 ->
         alpha_equal hyp1 hyp2 & compare (hyps1, hyps2)
    | [], [] ->
         true
    | _ ->
         false
   in
      alpha_equal goal1 goal2 & compare (hyps1, hyps2)
   
(*
 * Split the goals from the hyps.
 *)
let rec split_sequent_list = function
   { mseq_goal = goal; mseq_hyps = hyps }::t ->
      let goals, hypsl = split_sequent_list t in
         goal :: goals, hyps :: hypsl
 | [] ->
      [], []
   
(*
 * Get the sequent from a tactic.
 * If the compiler were smarter, this wouldn't have to do anything.
 *)
let msequent_of_tactic_arg { tac_goal = goal; tac_hyps = hyps } =
   { mseq_goal = goal; mseq_hyps = hyps }

(*
 * $Log$
 * Revision 1.1  1998/04/22 14:14:30  jyh
 * Utilities for the refiner.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
