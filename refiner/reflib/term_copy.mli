(*
 * This function copies a term, producing as much
 * aliasing as possible.  It is parameterized by the
 * term type, so it can be used for conversion between
 * term types.
 *)

open Refiner_sig
open Memo

module MakeTermCopy (FromRefiner : RefinerSig) (ToRefiner : RefinerSig) :
sig
   type t

   (* These save the state to get sharing for multiple calls *)
   val create : unit -> t
   val copy_term : t -> FromRefiner.TermType.term -> ToRefiner.TermType.term
   val copy_meta_term : t -> FromRefiner.TermType.meta_term -> ToRefiner.TermType.meta_term

   (* Single use versions *)
   val copy_term_single : FromRefiner.TermType.term -> ToRefiner.TermType.term
   val copy_meta_term_single : FromRefiner.TermType.meta_term -> ToRefiner.TermType.meta_term
end

(*
 * Common cases.
 *)
type normalize
type denormalize

val create_norm : unit -> normalize
val create_denorm : unit -> denormalize

val normalize_term : normalize ->
   Refiner_std_verb.Refiner.TermType.term ->
   Refiner.Refiner.TermType.term

val normalize_meta_term : normalize ->
   Refiner_std_verb.Refiner.TermType.meta_term ->
   Refiner.Refiner.TermType.meta_term

val denormalize_term : denormalize ->
   Refiner.Refiner.TermType.term ->
   Refiner_std_verb.Refiner.TermType.term

val denormalize_meta_term : denormalize ->
   Refiner.Refiner.TermType.meta_term ->
   Refiner_std_verb.Refiner.TermType.meta_term

val normalize_term_single : Refiner_std_verb.Refiner.TermType.term ->
   Refiner.Refiner.TermType.term

val normalize_meta_term_single : Refiner_std_verb.Refiner.TermType.meta_term ->
   Refiner.Refiner.TermType.meta_term

val denormalize_term_single : Refiner.Refiner.TermType.term ->
   Refiner_std_verb.Refiner.TermType.term

val denormalize_meta_term_single : Refiner.Refiner.TermType.meta_term ->
   Refiner_std_verb.Refiner.TermType.meta_term

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
