(*
 * This is the standard refiner.
 *)

module Refiner =
struct
   module Term = Term_ds.Term
   module TermOp = Term_op_ds.TermOp
   module TermMan = Term_man_ds.TermMan
   module TermAddr = Term_addr_ds.TermAddr
   module TermSubst = Term_subst_ds.TermSubst
   module TermShape = Term_shape_ds.TermShape
   module TermEval = Term_eval_ds.TermEval

   module TermMeta = Term_meta_std.TermMeta (Term) (TermSubst)
   module Rewrite = Rewrite.Rewrite (Term) (TermMan) (TermAddr) (TermSubst)
   module Refine = Refine.Refine (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (Rewrite)
end

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:00:30  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/28 02:53:05  nogin
 * Splitted Term_ds and Term_ds_simple modules into a smaller modules
 * for use in the functorized refiner
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
