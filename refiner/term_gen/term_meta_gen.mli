(*
 * Meta-terms include implications, etc.
 *)

open Term_sig
open Term_subst_sig
open Term_meta_sig

module TermMeta
   (Term : TermSig)
   (TermSubst : TermSubstSig
    with type term = Term.term)
       : TermMetaSig
   with type term = Term.term

(*
 * $Log$
 * Revision 1.2  1998/06/01 13:55:26  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/29 02:29:19  nogin
 * Created refiner/term_gen directory
 * Moved renamed term_std/term_meta_std to term_gen/term_meta_gen
 *
 * Revision 1.1  1998/05/28 15:02:33  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.2  1998/05/28 02:44:29  nogin
 * Parameterized by the Term and TermSubst modules
 *
 * Revision 1.1  1998/05/27 15:14:39  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
