(*
 * Test the display mechanism.
 *)

include Itt_theory

open Conversionals

open MLast

declare guard{'a}
declare fact{'i}

rewrite reduceFact : fact{'i} <--> (fix{f. lambda{i. ifthenelse{eq_int{'i; 0}; 1; .'i *@ 'f ('i -@ 1)}}} 'i)

rewrite fold : 'a <--> guard{'a}

val redexC : conv

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
