(*
 * Simplified version of termTable.
 *)

open Refiner.Refiner.Term

type 'a term_dtable
type 'a term_dextract

type 'a pair_fun = (term * term * 'a) list -> term * term -> 'a

val new_dtable : unit -> 'a term_dtable

val insert_left : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val insert_right : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val insert : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val join_tables : 'a term_dtable -> 'a term_dtable -> 'a term_dtable
val extract : 'a term_dtable -> 'a term_dextract
val lookup : 'a term_dextract -> term -> term -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
