(*
 * Wrap a closure marshaler around the application
 * interface.
 *)

open Ensemble

val full : 'msg Appl_intf.New.full -> Iovecl.t Appl_intf.New.full

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
