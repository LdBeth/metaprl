(*
 * Term operations for library.
 * Convert sequents to regular terms.
 *)

open Opname
open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.RefineError

(* Sequent operator names *)
let hyp_opname = mk_opname "hyp" xperv
let concl_opname = mk_opname "concl" xperv

(* Sequent wrapper *)
let sequent_op = mk_op sequent_opname []
let is_sequent_term = is_simple_term_opname sequent_opname

(* Dependent hypotheses *)
let is_hyp_term = is_dep0_dep1_term hyp_opname
let mk_hyp_term = mk_dep0_dep1_term hyp_opname
let dest_hyp = dest_dep0_dep1_term hyp_opname

(* Conclusions *)
let is_concl_term = is_dep0_dep0_term concl_opname
let mk_concl_term = mk_dep0_dep0_term concl_opname
let dest_concl = dest_dep0_dep0_term concl_opname
let null_concl = mk_simple_term concl_opname []

(*
 * Make a term sequent from an exploded sequent.
 *)
let rec mk_goals goals i len =
   if i = len then
      null_concl
   else
      mk_concl_term (SeqGoal.get goals i) (mk_goals goals (i + 1) len)

let rec mk_sequent_inner_term hyps goals i len =
   if i = len then
      mk_goals goals 0 (SeqGoal.length goals)
   else
      match SeqHyp.get hyps i with
         Hypothesis (v, t') ->
            mk_hyp_term v t' (mk_sequent_inner_term hyps goals (i + 1) len)
       | Context (v, subterms) ->
            mk_context_term v (mk_sequent_inner_term hyps goals (i + 1) len) subterms

let mk_sequent_term hyps goals =
   mk_sequent_inner_term hyps goals 0 (SeqHyp.length hyps)

(*
 * Make an exploded sequent from a term sequent.
 *)
let match_hyp_all name t = function
   [bterm1; bterm2] ->
      begin
         match dest_bterm bterm1, dest_bterm bterm2 with
            ({ bvars = []; bterm = t }, { bvars = [x]; bterm = term }) ->
               t, x, term
          | _ ->
               raise (RefineError (name, TermMatchError (t, "malformed hypothesis")))
      end
 | _ ->
      raise (RefineError (name, TermMatchError (t, "malformed hypothesis")))

let match_context name t = function
   [] ->
      raise (RefineError (name, TermMatchError (t, "malformed context")))
 | bterms ->
      match dest_bterm (List_util.last bterms) with
         { bvars = []; bterm = term } ->
            term
       | _ ->
            raise (RefineError (name, TermMatchError (t, "malformed context")))

let match_concl_all name t = function
   [bterm1; bterm2] ->
      begin
         match dest_bterm bterm1, dest_bterm bterm2 with
            ({ bvars = []; bterm = t }, { bvars = []; bterm = term }) ->
               t, term
          | _ ->
               raise (RefineError (name, TermMatchError (t, "malformed conclusion")))
      end
 | _ ->
      raise (RefineError (name, TermMatchError (t, "malformed conclusion")))

(*
 * Explode the sequent into a list of hyps and concls.
 *)
let dest_sequent_name = "explode_sequent"
let dest_sequent t =
   let rec collect hyps concls term =
      let { term_op = op; term_terms = bterms } = dest_term term in
      let { op_name = opname } = dest_op op in
         if Opname.eq opname hyp_opname then
            let t, x, term = match_hyp_all dest_sequent_name t bterms in
               collect (Hypothesis (x, t) :: hyps) concls term
         else if Opname.eq opname context_opname then
            let name, term, args' = dest_context term in
               collect (Context (name, args') :: hyps) concls term
         else if Opname.eq opname concl_opname then
            if bterms = [] then
               SeqHyp.of_list (List.rev hyps), SeqGoal.of_list (List.rev concls)
            else
               let goal, term = match_concl_all dest_sequent_name t bterms in
                  collect hyps (goal :: concls) term
         else
            raise (RefineError (dest_sequent_name, TermMatchError (t, "malformed sequent")))
   in
      collect [] [] t

(*
 * Term functions.
 *)
let mk_term op bterms =
   let { op_name = opname } = dest_op op in
      if Opname.eq opname sequent_opname then
         match List.map dest_bterm bterms with
            [{ bvars = []; bterm = args };
             { bvars = []; bterm = goal }
            ] ->
               let hyps, goals = dest_sequent goal in
                  TermMan.mk_sequent_term { sequent_args = args;
                                            sequent_hyps = hyps;
                                            sequent_goals = goals
                  }
          | _ ->
               raise (RefineError ("Lib_term.mk_term", TermMatchError (Term.mk_term op bterms, "malformed sequent")))
      else
         Term.mk_term op bterms

let make_term { term_op = op; term_terms = bterms } =
   mk_term op bterms

let dest_term t =
   if TermMan.is_sequent_term t then
      let { sequent_args = args;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = TermMan.explode_sequent t
      in
         { term_op = sequent_op;
           term_terms =
              [mk_simple_bterm args;
               mk_simple_bterm (mk_sequent_term hyps goals)]
         }
   else
      Term.dest_term t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
