open Printf
open Nl_debug
open Refiner.Refiner.Term
open Evaluator

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Itt_redrules%t" eflush

include Itt_theory

(*
let _ = add_simple_rule <<apply{lambda{x.'b};'a}>> (make_1subst_term <<'b>> "x" <<'a>>)

let _ = add_simple_rule <<spread{'a,'b;x,y.'t}>> (make_2subst_term <<'t>> "x" "y" <<'a>> <<'b>>)

let _ = add_simple_rule <<decide{inl{'a};x.'s;y.'t}>> (make_1subst_term <<'s>> "a" <<'x>>)

let _ = add_simple_rule <<decide{inr{'b};x.'s;y.'t}>> (make_1subst_term <<'t>> "b" <<'y>>)

let _ = add_simple_rule <<list_ind{nil;'s;x,y,u.'t}>> <<'s>>

let _ = add_simple_rule <<list_ind{cons{'a;'b};'s;x,y,u.'t}>>
           (make_subst_term <<'t>> ["x";"y";"u"] [<<'a>>;<<'b>>;<<list_ind{'b;'s;x,y,u.'t}>>])

let _ = add_reduction_rule <<add{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (n1+n2) |
              _ -> raise (Failure "Evaluator: ITT!add"))

let _ = add_reduction_rule <<sub{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (n1-n2) |
              _ -> raise (Failure "Evaluator: ITT!sub"))

let _ = add_reduction_rule <<mul{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (n1*n2) |
              _ -> raise (Failure "Evaluator: ITT!mul"))

let _ = add_reduction_rule <<div{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (if n2=0 then 0 else n1/n2) |
              _ -> raise (Failure "Evaluator: ITT!div"))

let _ = add_reduction_rule <<rem{natural_number[@n1:n];natural_number[@n2:n]}>>
           (function
              [Number n1;Number n2] -> mk_natural_number_term (if n2=0 then 0 else n1 mod n2) |
              _ -> raise (Failure "Evaluator: ITT!rem"))

let _ =
   let make_ind_term n = mk_ind_term (mk_natural_number_term n) "x" "y" <<'s>> <<'b>> "u" "v" <<'t>> in
   add_reduction_rule <<ind{natural_number[@n:n];x,y.'s;'b;u,v.'t}>>
           (function
              [Number n] ->
                 if n=0 then <<'b>> else
                 if n>0 then make_2subst_term <<'t>> "u" "v" (mk_natural_number_term n) (make_ind_term (n-1))
                 else make_2subst_term <<'s>> "x" "y" (mk_natural_number_term n) (make_ind_term (n+1)) |
              _ ->  raise (Failure "Evaluator: ITT!ind"))

*)


