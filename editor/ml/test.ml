(*
 * Display all the elements in a particular theory.
 *)

include Itt_theory

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Itt_logic
open Itt_struct
open Tacticals
open Base_auto_tactic
open Base_dtactic

let ifNotWT tac p =
   (if (Sequent.label p) = "wf" then
       idT
    else
       tac) p

(* Term classes *)
let is_imp_term term =
   is_implies_term term or is_not_term term

let is_imp_and_term term =
   is_imp_term term & is_and_term (term_subterm term (make_address [1]))

let is_imp_or_term term =
   is_imp_term term & is_or_term (term_subterm term (make_address [1]))

let is_imp_imp_term term =
   is_imp_term term & is_implies_term (term_subterm term (make_address [1]))

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let fwdThruLemmaT str int_list = idT
let iDT i = dT i
interactive test 'H 'J : :
   sequent ['ext] { 'H; x: 'a & 'b; y: "not"{'a}; z: 'a -> "void"; w: "implies"{'a; 'b}; 'J['x] >- 'a => 'b }
interactive test1 'H 'J : :
   sequent ['ext] { 'H; x: 'a => 'b; 'J['x] >- 'b }
interactive test2 'H 'J : :
   sequent ['ext] { 'H; x: "implies"{'a; 'b}; 'J['x] >- 'b }
interactive test3 'H 'J : :
   sequent ['ext] { 'H; x: 'a -> 'b; 'J['x] >- 'b }
interactive test4 'H 'J : :
   sequent ['ext] { 'H; x: 'a -> "void"; 'J['x] >- 'b }
(*
interactive test5 'H 'J : :
   sequent ['ext] { 'H; x: "implies"{'a; "false"}; 'J['x] >- 'b }
interactive test6 'H 'J : :
   sequent ['ext] { 'H; x: "implies"{'a; "void"}; 'J['x] >- 'b }
*)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)

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

(* Try to decompose a hypothesis *)
let rec decompIntHypT i p =
   (let term = snd(Sequent.nth_hyp p i) in
       if is_false_term term then
          trivialT
       else if is_and_term term or is_or_term term then
          dT i thenT ifNotWT proveIntT
       else if is_imp_and_term term then
          (* {C & D => B} => {C => D => B} *)
          (fwdThruLemmaT "imp_and_elim" [i]
           orelseT fwdThruLemmaT "not_and_elim" [i])
          thenT ifNotWT (thinT i)
          thenT ifNotWT proveIntT
       else if is_imp_or_term term then
          (* {C or D => B} => {(C => B) & (D => B)} *)
          (fwdThruLemmaT "imp_or_elim" [i]
              orelseT fwdThruLemmaT "not_or_elim" [i])
          thenT ifNotWT (dT (-1))
          thenT ifNotWT (thinT i)
          thenT ifNotWT proveIntT
       else if is_imp_imp_term term then
          (* {(C => D) => B} => {D => B} *)
          (iDT i)
          thenAT (fwdThruLemmaT "imp_imp_elim" [i]
                     orelseT fwdThruLemmaT "not_imp_elim" [i])
          thenMT thinT i
          thenT ifNotWT proveIntT
       else if is_imp_term term then
          dT i thenT ifNotWT proveIntT
       else
          trivialT) p

(* Decompose the goal *)
and decompIntConclT p =
   (let goal = Sequent.concl p in
       if is_or_term goal then
          (selT 1 (dT 0) thenT ifNotWT proveIntT)
          orelseT (selT 2 (dT 0) thenT ifNotWT proveIntT)
       else if is_and_term goal or is_imp_term goal then
          dT 0 thenT ifNotWT proveIntT
       else
          trivialT) p

(* Prove the proposition *)
and proveIntT p =
   (onSomeHypT decompIntHypT orelseT decompIntConclT) p


(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
