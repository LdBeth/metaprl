(*
 * This is the standard refiner.
 *)

module Refiner =
struct
   module TermType = Term_std.TermType
   module AddressType = Term_addr_gen_verb.MakeAddressType (TermType)
   module RefineError = Refine_error.MakeRefineError (TermType) (AddressType)
   module Term = Term_base_std_verb.Term (RefineError)
   module TermOp = Term_op_std_verb.TermOp (Term) (RefineError)
   module TermSubst = Term_subst_std_verb.TermSubst (Term) (RefineError)
   module TermAddr = Term_addr_gen_verb.TermAddr (TermType) (Term) (TermOp) (RefineError)
   module TermMan = Term_man_gen_verb.TermMan (TermType) (Term) (TermOp) (TermAddr) (TermSubst) (RefineError)
   module TermShape = Term_shape_gen_verb.TermShape (TermType) (Term)
   module TermEval = Term_eval_std_verb.TermEval (Term) (RefineError)
   module TermMeta = Term_meta_gen_verb.TermMeta (TermType) (Term) (TermSubst) (RefineError)
   module Rewrite = Rewrite_verb.Rewrite (TermType) (Term) (TermMan) (TermAddr) (TermSubst) (RefineError)
   module Refine = Refine_verb.Refine (TermType) (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (Rewrite) (RefineError)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
