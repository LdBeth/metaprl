(*
 * Module for printing terms to an ML module.
 *
 *)

open Ml_print_sig

(*
 * Printer maker.
 *)
module MakePrinter (File : FileSig) : PrinterSig
with type t = File.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
