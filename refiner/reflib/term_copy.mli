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
   val copy_term : FromType.term -> ToType.term
   val copy_meta_term : FromType.meta_term -> ToType.meta_term
end

(*
 * Common cases.
 *)
val normalize_term : Refiner_std.Refiner.TermType.term ->
   Refiner.Refiner.TermType.term

val normalize_meta_term : Refiner_std.Refiner.TermType.meta_term ->
   Refiner.Refiner.TermType.meta_term

val denormalize_term : Refiner.Refiner.TermType.term ->
   Refiner_std.Refiner.TermType.term

val denormalize_meta_term : Refiner.Refiner.TermType.meta_term ->
   Refiner_std.Refiner.TermType.meta_term

(*
 * $Log$
 * Revision 1.1  1998/07/02 22:24:58  jyh
 * Created term_copy module to copy and normalize terms.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
