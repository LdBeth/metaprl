(*
 * Simplified version of termTable.
 *)

open Refiner.Refiner.Term

type 'a term_stable
type 'a term_sextract

val new_stable : unit -> 'a term_stable

val sinsert : 'a term_stable -> term -> 'a -> 'a term_stable
val join_stables : 'a term_stable -> 'a term_stable -> 'a term_stable
val sextract : 'a term_stable -> 'a term_sextract
val slookup : 'a term_sextract -> term -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
