(*
 * Display all the elements in a particular theory.
 *)

include Itt_theory

open Printf
open Nl_debug

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Refiner.Refiner.RefineError
open Itt_logic
open Itt_struct
open Tacticals
open Base_auto_tactic
open Base_dtactic
open Conversionals
open Var

(**)
let debug_prove_int =
  create_debug
     { debug_name = "prove_int";
       debug_description = "show proveInt operations";
       debug_value = false
     }

(* Like onSomeHyp, but works backwards. *)
let revOnSomeHypT tac p =
   let rec aux i =
      if i = 1 then
         tac i
      else if i > 1 then
         tac i orelseT (aux (i - 1))
      else
         idT
   in
      aux (Sequent.hyp_count p) p

(* Operate on all non-wf subgoals *)
let ifNotWT tac p =
   (if (Sequent.label p) = "wf" then
       idT
    else
       tac) p

(* Term classes *)
let is_imp_and_term term =
   is_implies_term term & is_and_term (term_subterm term (make_address [0]))

let is_imp_or_term term =
   is_implies_term term & is_or_term (term_subterm term (make_address [0]))

let is_imp_imp_term term =
   is_implies_term term & is_implies_term (term_subterm term (make_address [0]))

interactive imp_and_rule 'H 'J 'u :
   sequent [squash] { 'H; x: "and"{'C; 'D} => 'B; 'J['x] >- "type"{'C} } -->
   sequent [squash] { 'H; x: "and"{'C; 'D} => 'B; 'J['x] >- "type"{'D} } -->
   sequent ['ext] { 'H; x: "and"{'C; 'D} => 'B; 'J['x];
                     u: 'C => 'D => 'B >- 'T['x] } -->
   sequent ['ext] { 'H; x: "and"{'C; 'D} => 'B; 'J['x] >- 'T['x] }

interactive imp_or_rule 'H 'J 'u 'v :
   sequent [squash] { 'H; x: "or"{'C; 'D} => 'B; 'J['x] >- "type"{'C} } -->
   sequent [squash] { 'H; x: "or"{'C; 'D} => 'B; 'J['x] >- "type"{'D} } -->
   sequent ['ext] { 'H; x: "or"{'C; 'D} => 'B; 'J['x];
                     u: 'C => 'B; v: 'D => 'B >- 'T['x] } -->
   sequent ['ext] { 'H; x: "or"{'C; 'D} => 'B; 'J['x] >- 'T['x] }

interactive imp_imp_rule 'H 'J 'u :
   sequent [squash] { 'H; x: "implies"{'C; 'D} => 'B; 'J['x] >- "type"{'C} } -->
   sequent [squash] { 'H; x: "implies"{'C; 'D} => 'B; 'J['x] >- "type"{'D} } -->
   sequent ['ext] { 'H; x: "implies"{'C; 'D} => 'B; 'J['x];
                     u: 'D => 'B >- 'T['x] } -->
   sequent ['ext] { 'H; x: "implies"{'C; 'D} => 'B; 'J['x] >- 'T['x] }

(* Create a tactic for the X-implication-elimination. *)
let d_and_impT i p =
   if i = 0 then
      raise (RefineError ("d_and_impT", StringError "no introduction form"))
   else
      let u = maybe_new_vars1 p "u" in
      let j, k = Sequent.hyp_indices p i in
         (imp_and_rule j k u
          thenLT [addHiddenLabelT "wf";
                  addHiddenLabelT "wf";
                  thinT i]) p

let d_or_impT i p =
   if i = 0 then
      raise (RefineError ("d_or_impT", StringError "no introduction form"))
   else
      let u, v = maybe_new_vars2 p "u" "v" in
      let j, k = Sequent.hyp_indices p i in
         (imp_or_rule j k u v
          thenLT [addHiddenLabelT "wf";
                  addHiddenLabelT "wf";
                  thinT i]) p

let d_imp_impT i p =
   if i = 0 then
      raise (RefineError ("d_and_impT", StringError "no introduction form"))
   else
      let u = maybe_new_vars1 p "u" in
      let j, k = Sequent.hyp_indices p i in
         (imp_and_rule j k u
          thenLT [addHiddenLabelT "wf";
                  addHiddenLabelT "wf";
                  thinT i]) p

(* Try to decompose a hypothesis *)
let rec decompIntHypT i p =
   if !debug_prove_int then
      begin
         eprintf "decompIntHypT %d: %a%t" i Refiner.Refiner.Term.debug_print (snd(Sequent.nth_hyp p i)) eflush
      end;
   (let term = snd(Sequent.nth_hyp p i) in
       if is_false_term term then
          dT i
       else if is_and_term term or is_or_term term then
          dT i thenT ifNotWT internalIntT
       else if is_imp_and_term term then
          (* {C & D => B} => {C => D => B} *)
          (d_and_impT i)
          thenT ifNotWT internalIntT
       else if is_imp_or_term term then
          (* {C or D => B} => {(C => B) & (D => B)} *)
          (d_or_impT i)
          thenT ifNotWT internalIntT
       else if is_imp_imp_term term then
          (* {(C => D) => B} => {D => B} *)
          (d_imp_impT i)
          thenT ifNotWT internalIntT
       else if is_implies_term term then
          dT i thenT thinT i thenT ifNotWT internalIntT
       else
          (* Nothing recognized, try to see if we're done. *)
          nthHypT i) p

(* Decompose the goal *)
and decompIntConclT p =
   (let goal = Sequent.concl p in
       if is_or_term goal then
          (selT 1 (dT 0) thenT ifNotWT internalIntT)
          orelseT (selT 2 (dT 0) thenT ifNotWT internalIntT)
       else if is_and_term goal or is_implies_term goal then
          dT 0 thenT ifNotWT internalIntT
       else
          trivialT) p

(* Prove the proposition - internal version that does not handle not's *)
and internalIntT p =
   (onSomeHypT decompIntHypT orelseT decompIntConclT) p

(* Convert all "not X" terms to "X => False" *)
let notToImpliesFalseC =
   sweepUpC (unfoldNot andthenC foldImplies andthenC (addrC [1] foldFalse))

(* Toplevel tactic *)
let proveIntT p =
   (* Should remove tryT when it's ok to apply a convn on a context *)
   ((onAllClausesT (fun i -> tryT (rw notToImpliesFalseC i))
     thenT internalIntT)
    orelseT (fun p -> raise (RefineError("proveIntT", StringError "formula is not valid"))))
   p


(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
interactive test 'H 'J : :
   sequent ['ext] { 'H; x: "not"{(("not"{'a}) & ("not"{'b}))}; y: "not"{'a}; z: "not"{("not"{'a})} -> "void"; w: "implies"{'a; 'b}; 'J['x] >- "not"{.'a => 'b} }
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
