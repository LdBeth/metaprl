(*
 * This is the standard refiner.
 *)

module Refiner =
struct
   module TermType = Term_std.TermType
   module AddressType = Term_addr_gen_simp.MakeAddressType (TermType)
   module RefineError = Refine_error.MakeRefineError (TermType) (AddressType)
   module Term = Term_base_std_simp.Term (RefineError)
   module TermOp = Term_op_std_simp.TermOp (Term) (RefineError)
   module TermSubst = Term_subst_std_simp.TermSubst (Term) (RefineError)
   module TermAddr = Term_addr_gen_simp.TermAddr (TermType) (Term) (TermOp) (RefineError)
   module TermMan = Term_man_gen_simp.TermMan (TermType) (Term) (TermOp) (TermAddr) (TermSubst) (RefineError)
   module TermShape = Term_shape_gen_simp.TermShape (TermType) (Term)
   module TermEval = Term_eval_std_simp.TermEval (Term) (RefineError)
   module TermMeta = Term_meta_gen_simp.TermMeta (TermType) (Term) (TermSubst) (RefineError)
   module Rewrite = Rewrite_simp.Rewrite (TermType) (Term) (TermMan) (TermAddr) (TermSubst) (RefineError)
   module Refine = Refine_simp.Refine (TermType) (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (Rewrite) (RefineError)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
