(*
 * Just add the infixes to the current grammar.
 *)

open Debug
open Printf

open MLast
open Pcaml

open Infix

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_grammar%t" eflush

(*
 * Unit is just used as a dummy.
 *)
module type UnitSig =
sig
end

(*
 * Enclose the grammar in a functor so that it is not always evaluated.
 *)
module MakeFilterGrammar (Unit : UnitSig) =
struct
   EXTEND
       GLOBAL: expr;

      (*
       * Pre-add some infix operators.
       *)
      expr: AFTER "expr1" (**)
         [LEFTA
          [ t1 = expr; op = "THEN"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "THENL"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "orelseT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "andalsoT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "orthenT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenLT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenFLT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "then_OnEachT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "then_OnFirstT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "then_OnLastT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "then_OnSameConclT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenLabLT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenMT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenMLT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenAT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenALT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenWT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenET"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "thenPT"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "andthenC"; t2 = expr ->
             make_infix loc op t1 t2
           | t1 = expr; op = "orelseC"; t2 = expr ->
             make_infix loc op t1 t2
          ]];
   END
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
