(*
 * Main routine.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:14  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/09/02 19:33:03  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:22  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 16:59:45  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:15  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

include Itt_theory

(*
 * Also include a guard term for debugging.
 *)
declare guard
rewrite foldGuard : 'x <--> guard{'x}

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
