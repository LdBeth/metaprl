(*
 * Test the interface filter.
 *)

declare test{'a}
declare hello{'a}

define hello : hello{'a} <--> test{'a}
rewrite hello2 : hello{'a} <--> test{'a}

axiom hello3 'H :
   sequent { 'H >- test{'a} } -->
   sequent { 'H >- hello{'a} }

infix HELLO

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
