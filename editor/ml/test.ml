(*
 * Display all the elements in a particular theory.
 *)

include Itt_theory

open Conversionals
open Itt_rfun
open Itt_bool
open Itt_int
open Itt_int_bool

declare guard{'a}
declare fact{'i}

primrw fold : 'a <--> guard{'a}

primrw reduceFact : fact{'i} <--> fix{f. lambda{i. ifthenelse{eq_int{'i; 0}; 1; .'i *@ 'f ('i -@ 1)}}} 'i

primrw test : fact{40} <--> 0 (* 815915283247897734345611269596115894272000000000 *)

dform fact_df : parens :: "prec"[prec_apply] :: fact{'i} =
   `"fact" " " slot{'i}

let redexC =
   firstC [reduceBeta;
           reduceEQInt;
           reduceFact;
           reduceBoolTrue;
           reduceBoolFalse;
           reduceIfthenelseTrue;
           reduceIfthenelseFalse;
           reduceAdd;
           reduceSub;
           reduceMul;
           reduceDiv;
           reduceFix]

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
