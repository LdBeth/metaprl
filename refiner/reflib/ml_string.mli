(*
 * Print a term as a string.
 *)

open Ml_print_sig

module StringFile : FileSig
with type name = unit
and type out = string

module StringPrint : PrinterSig with
type t = StringFile.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
