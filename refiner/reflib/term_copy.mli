(*
 * This function copies a term, producing as much
 * aliasing as possible.  It is parameterized by the
 * term type, so it can be used for conversion between
 * term types.
 *)

open Term_sig
open Term_base_sig
open Memo

module MakeTermCopy (**)
   (FromType : TermSig)
   (FromTerm : TermBaseSig
    with type level_exp_var = FromType.level_exp_var
    with type level_exp = FromType.level_exp
    with type param = FromType.param
    with type operator = FromType.operator
    with type term = FromType.term
    with type bound_term = FromType.bound_term

    with type level_exp_var' = FromType.level_exp_var'
    with type level_exp' = FromType.level_exp'
    with type object_id = FromType.object_id
    with type param' = FromType.param'
    with type operator' = FromType.operator'
    with type term' = FromType.term'
    with type bound_term' = FromType.bound_term')
   (ToType : TermSig)
   (ToTerm : TermBaseSig
    with type level_exp_var = ToType.level_exp_var
    with type level_exp = ToType.level_exp
    with type param = ToType.param
    with type operator = ToType.operator
    with type term = ToType.term
    with type bound_term = ToType.bound_term

    with type level_exp_var' = ToType.level_exp_var'
    with type level_exp' = ToType.level_exp'
    with type object_id = ToType.object_id
    with type param' = ToType.param'
    with type operator' = ToType.operator'
    with type term' = ToType.term'
    with type bound_term' = ToType.bound_term')
:
sig
   type t

   (* These save the state to get sharing for multiple calls *)
   val create : unit -> t
   val copy_term : t -> FromType.term -> ToType.term
   val copy_meta_term : t -> FromType.meta_term -> ToType.meta_term

   (* Single use versions *)
   val copy_term_single : FromType.term -> ToType.term
   val copy_meta_term_single : FromType.meta_term -> ToType.meta_term
end

(*
 * Common cases.
 *)
type normalize
type denormalize

val create_norm : unit -> normalize
val create_denorm : unit -> denormalize

val normalize_term : normalize ->
   Refiner_std.Refiner.TermType.term ->
   Refiner.Refiner.TermType.term

val normalize_meta_term : normalize ->
   Refiner_std.Refiner.TermType.meta_term ->
   Refiner.Refiner.TermType.meta_term

val denormalize_term : denormalize ->
   Refiner.Refiner.TermType.term ->
   Refiner_std.Refiner.TermType.term

val denormalize_meta_term : denormalize ->
   Refiner.Refiner.TermType.meta_term ->
   Refiner_std.Refiner.TermType.meta_term

val normalize_term_single : Refiner_std.Refiner.TermType.term ->
   Refiner.Refiner.TermType.term

val normalize_meta_term_single : Refiner_std.Refiner.TermType.meta_term ->
   Refiner.Refiner.TermType.meta_term

val denormalize_term_single : Refiner.Refiner.TermType.term ->
   Refiner_std.Refiner.TermType.term

val denormalize_meta_term_single : Refiner.Refiner.TermType.meta_term ->
   Refiner_std.Refiner.TermType.meta_term

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
