(*
 * This is the standard refiner.
 *)

open Term_sig
open Refiner_sig

module Refiner : RefinerSig
                 with module TermType = Term_ds.TermType

(*
 * $Log$
 * Revision 1.1  1998/07/02 18:35:31  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.1  1998/05/28 15:00:31  jyh
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
