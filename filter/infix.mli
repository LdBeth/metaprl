(*
 * Code for adding infixes to the expression syntax.
 *)

open Filter_type

val make_infix : MLast.loc -> string -> MLast.expr -> MLast.expr -> MLast.expr
val add_infix : string -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
