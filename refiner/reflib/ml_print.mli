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
 * $Log$
 * Revision 1.1  1998/05/28 15:00:58  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1997/04/28 15:51:25  jyh
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
 * Revision 1.5  1996/05/21 02:13:58  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.4  1996/03/25 20:50:42  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.3  1996/03/05 19:48:32  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.2  1996/02/13 21:32:23  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.1  1996/02/10 20:19:54  jyh
 * Initial checkin of filter (prlcomp).
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
