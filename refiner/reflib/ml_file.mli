(*
 * Module for printing terms to an ML module.
 *
 *)

open Ml_print_sig

module MLFile : FileSig
with type name = string
and type out = unit

module IOFile : FileSig
with type name = string
and type out = unit
and type t = out_channel

module MLPrint : PrinterSig with
type t = MLFile.t

module IOPrint : PrinterSig with
type t = out_channel

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
