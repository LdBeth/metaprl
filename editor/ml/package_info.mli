(*
 * The editor collects information about each theory and
 * summarizes it in a "package".  At start-up, the pre-loaded packages
 * are collected and presented as read-only theories.
 *
 * We can also create interactive packages, which are writable,
 * and may contain interactivly generated proofs.
 *)

include Package_type

module Package : PackageSig

(*
 * $Log$
 * Revision 1.3  1998/04/15 12:39:34  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.2  1998/04/09 19:07:22  jyh
 * Updating the editor.
 *
 * Revision 1.1  1997/08/06 16:17:17  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:33:24  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:31  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:59  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
