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
 * $Log$
 * Revision 1.1  1997/04/28 15:51:27  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
