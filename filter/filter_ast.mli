(*
 * Grammar utilities.
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Filter_type

val build_ml_term : MLast.loc -> term -> MLast.expr
val build_ml_mterm : MLast.loc -> meta_term -> MLast.expr
val list_expr : MLast.loc -> ('a -> MLast.expr) -> 'a list -> MLast.expr
val apply_patt : MLast.loc -> ('a -> MLast.patt) -> 'a list -> MLast.patt
val list_patt : MLast.loc -> ('a -> MLast.patt) -> 'a list -> MLast.patt
val fun_expr : MLast.loc -> string list -> MLast.expr -> MLast.expr

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
