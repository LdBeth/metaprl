(*
 * Define terms.  This is the standard straighforward definition.
 *)

open Term_std_sig

module TermType : TermStdTypeSig

(*
 * $Log$
 * Revision 1.8  1998/07/02 18:36:38  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.7  1998/06/22 19:45:58  jyh
 * Rewriting in contexts.  This required a change in addressing,
 * and the body of the context is the _last_ subterm, not the first.
 *
 * Revision 1.6  1998/06/15 21:57:22  jyh
 * Added a few new functions.
 *
 * Revision 1.5  1998/06/03 22:19:38  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.4  1998/06/03 15:23:59  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.3  1998/06/01 19:53:52  jyh
 * Working addition proof.  Removing polymorphism from refiner(?)
 *
 * Revision 1.2  1998/05/30 19:18:49  nogin
 * Eliminated white space in empty lines.
 *
 * Revision 1.1  1998/05/28 15:02:43  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:56  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
