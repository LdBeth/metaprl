(*
 * Meta-terms include implications, etc.
 *)

open Term_std
open Term_meta_sig

module TermMeta : TermMetaSig
                  with type term = Term.term

(*
 * $Log$
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
