(*
 * This is the standard refiner.
 *)

module Refiner =
struct
   module Term = Term_std.Term
   module TermOp = Term_op_std.TermOp
   module TermMan = Term_man_std.TermMan
   module TermAddr = Term_addr_std.TermAddr
   module TermSubst = Term_subst_std.TermSubst
   module TermShape = Term_shape_std.TermShape
   module TermEval = Term_eval_std.TermEval
   module TermMeta = Term_meta_std.TermMeta

   module Rewrite = Rewrite.Rewrite (Term) (TermMan) (TermAddr) (TermSubst)
   module Refine = Refine.Refine (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (Rewrite)
end

(*
 * $Log$
 * Revision 1.1  1998/05/27 15:14:04  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
