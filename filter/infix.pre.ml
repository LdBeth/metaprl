(*
 * This file just exists to extract the code for adding and infix
 * expression.
 *)

open Pcaml

(*
 * Make an infix expression.
 *)
let make_infix loc op e1 e2 =
   let lop = "prefix_" ^ op in
      <:expr< $lid:lop$ $e1$ $e2$ >>

(*
 * Add an infix keyword.
 * This is computed in infix.ml.
 *)
let add_infix (keyword : string) =
   EXTEND
      GLOBAL: expr;

      expr: BEFORE "top" (**)
         [[ e1 = expr; op = "KEYWORD"; e2 = expr ->
             make_infix loc op e1 e2
          ]];
   END

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
