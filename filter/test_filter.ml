(*
 * Test some terms.
 *)

infix HELLO

declare test{'a}
declare hello{'a}

primrw hello : hello{'a} <--> test{'a}
rwthm hello2 : (hello{'a} <--> test{'a}) = hello

prim hello3 'H : sequent { 'H >- test{'a} } : sequent { 'H >- hello{'a} } =
   hello;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "camlp4"
 * End:
 * -*-
 *)
