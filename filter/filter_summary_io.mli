(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *)

open File_base_type

open Filter_summary
open Filter_summary_type

(*
 * Create a summary base for a specific format.
 *)
module MakeSummaryBase (Types : SummaryTypesSig)
   (FileBase : FileBaseSig
            with type cooked = Types.proof module_info
            with type select = Types.select) :
      (SummaryBaseSig
       with type proof = Types.proof
       with type select = Types.select)

(*
 * $Log$
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
