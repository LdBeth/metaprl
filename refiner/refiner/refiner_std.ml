(*
 * This is the standard refiner.
 *)

module Refiner =
struct
   module Term = Term_std.Term
   module TermOp = Term_op_std.TermOp
   module TermSubst = Term_subst_std.TermSubst
   module TermAddr = Term_addr_gen.TermAddr (Term) (TermOp)
   module TermMan = Term_man_gen.TermMan (Term) (TermOp) (TermAddr) (TermSubst)
   module TermShape = Term_shape_gen.TermShape (Term)
   module TermEval = Term_eval_std.TermEval

   module TermMeta = Term_meta_gen.TermMeta (Term) (TermSubst)
   module Rewrite = Rewrite.Rewrite (Term) (TermMan) (TermAddr) (TermSubst)
   module Refine = Refine.Refine (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (Rewrite)
end

(*
 * $Log$
 * Revision 1.4  1998/06/03 22:19:24  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.3  1998/06/03 15:23:19  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.2  1998/05/29 02:29:17  nogin
 * Created refiner/term_gen directory
 * Moved renamed term_std/term_meta_std to term_gen/term_meta_gen
 *
 * Revision 1.1  1998/05/28 15:00:33  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.2  1998/05/28 02:45:13  nogin
 * Parameterized TermMeta module with Term and TermSubst modules
 *
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
