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
with type seq_hyps = TermType.seq_hyps
with type seq_goals = TermType.seq_goals

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
