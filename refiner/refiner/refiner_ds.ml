(*
 * This is the standard refiner.
 *)

module Refiner =
struct
   module Term = Term_ds.Term
   module TermOp = Term_op_ds.TermOp
   module TermSubst = Term_subst_ds.TermSubst
   module TermMan = Term_man_gen.TermMan (Term) (TermOp) (TermSubst)
   module TermAddr = Term_addr_gen.TermAddr (Term) (TermOp)
   module TermShape = Term_shape_gen.TermShape (Term)
   module TermEval = Term_eval_ds.TermEval

   module TermMeta = Term_meta_gen.TermMeta (Term) (TermSubst)
   module Rewrite = Rewrite.Rewrite (Term) (TermMan) (TermAddr) (TermSubst)
   module Refine = Refine.Refine (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (Rewrite)
end

(*
 * $Log$
 * Revision 1.3  1998/06/03 15:23:17  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.2  1998/05/29 02:29:16  nogin
 * Created refiner/term_gen directory
 * Moved renamed term_std/term_meta_std to term_gen/term_meta_gen
 *
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
