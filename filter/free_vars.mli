(*
 * Compute the free vars of a term.
 *)

val patt_vars : MLast.patt -> string list
val free_vars : MLast.expr -> string list
val new_vars : MLast.expr -> string list -> string list

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
