(*
 * This is the standard refiner.
 *)

open Term_sig
open Refiner_sig

module Refiner : RefinerSig
                 with type Term.term = Term_std.Term.term

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:00:34  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:06  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
