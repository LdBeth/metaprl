(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *)

open Refiner.Refiner.Term

open File_base_type

open Filter_summary
open Filter_summary_type

(*
 * Create a summary base for a specific format.
 *)
module MakeSummaryBase
   (Address : AddressSig)
   (FileBase : FileBaseSig with type cooked = Address.t) :
      (SummaryBaseSig
       with type cooked = FileBase.cooked
       with type select = FileBase.select)

(*
 * $Log$
 * Revision 1.4  1998/05/27 15:13:05  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.3  1998/02/19 17:14:01  jyh
 * Splitting filter_parse.
 *
 * Revision 1.2  1997/08/06 16:17:34  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:00  jyh
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
 * Revision 1.1  1996/09/02 19:43:18  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
