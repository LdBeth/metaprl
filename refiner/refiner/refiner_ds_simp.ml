(*
 * This is the standard refiner.
 *)

module Refiner =
struct
   module TermType = Term_ds.TermType
   module AddressType = Term_addr_ds_simp.AddressType
   module RefineError = Refine_error.MakeRefineError (TermType) (AddressType)
   module Term = Term_base_ds_simp.Term (RefineError)
   module TermOp = Term_op_ds_simp.TermOp (Term) (RefineError)
   module TermSubst = Term_subst_ds_simp.TermSubst (Term) (RefineError)
   module TermAddr = Term_addr_ds_simp.TermAddr (Term) (TermOp) (RefineError)
   module TermMan = Term_man_ds_simp.TermMan (Term) (TermOp) (TermAddr) (TermSubst) (RefineError)
   module TermShape = Term_shape_gen_simp.TermShape (TermType) (Term)
   module TermEval = Term_eval_ds_simp.TermEval (Term) (RefineError)
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
