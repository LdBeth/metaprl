(*
 * This is the standard refiner.
 *)

open Term_sig
open Refiner_sig

module Refiner : RefinerSig
                 with module TermType = Term_std.TermType

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
