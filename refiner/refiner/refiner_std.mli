(*
 * This is the standard refiner.
 *)

open Term_sig
open Refiner_sig

module Refiner : RefinerSig
                 with module TermType = Term_std.TermType

(*
 * $Log$
 * Revision 1.2  1998/07/02 18:35:32  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
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
