(*
 * Create the error module.
 *)

open Refine_error_sig
open Term_sig

(*
 * Error module.
 *)
module MakeRefineError (**)
   (TermType : TermSig)
   (AddressType : TypeSig) :
RefineErrorSig
with type level_exp = TermType.level_exp
with type param = TermType.param
with type term = TermType.term
with type bound_term = TermType.bound_term
with type meta_term = TermType.meta_term
with type address = AddressType.t

(*
 * $Log$
 * Revision 1.1  1998/07/02 18:35:25  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
