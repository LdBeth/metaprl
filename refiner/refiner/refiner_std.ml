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
   module Refine = Refine.Refine (TermType) (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (Rewrite) (RefineError)
end

(*
 * $Log$
 * Revision 1.6  1998/07/02 18:35:31  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.5  1998/07/01 04:36:55  nogin
 * Moved Refiner exceptions into a separate module RefineErrors
 *
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
