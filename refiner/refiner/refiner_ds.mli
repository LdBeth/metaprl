(*
 * This is the standard refiner.
 *)

open Term_sig
open Refiner_sig

module Refiner : RefinerSig
                 with module TermType = Term_ds.TermType

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
