#open "opname";;
#open "term";;
#open "refine";;
#open "precedence";;
#open "dformPrint";;
# 1 "editor_init.prli"
(*
 * Initialization.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:10  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.3  1996/09/02 19:32:56  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:16  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:43  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 *)

#open "sequent";;
#open "tacticals";;
#infix "thenPT";;
#infix "thenWT";;
#infix "thenALT";;
#infix "thenAT";;
#infix "thenMLT";;
#infix "thenMT";;
#infix "thenLabLT";;
#infix "then_OnSameConclT";;
#infix "then_OnLastT";;
#infix "then_OnFirstT";;
#infix "then_OnEachT";;
#infix "thenFLT";;
#infix "thenLT";;
#infix "thenT";;
#infix "orthenT";;
#infix "andalsoT";;
#infix "orelseT";;
#open "baseDForm";;
#open "nuprlFont";;
#open "baseDTactic";;
#open "baseTheory";;
#open "itt_equal";;
#open "itt_void";;
#open "itt_atom";;
#open "itt_set";;
#open "itt_rfun";;
#open "itt_dprod";;
#open "itt_union";;
#open "itt_soft";;
#open "itt_logic";;
#open "itt_int";;
#open "itt_dfun";;
#open "itt_fun";;
#open "itt_prod";;
#open "itt_struct";;
#open "itt_isect";;
#open "itt_subtype";;
#open "itt_prec";;
#open "itt_srec";;
#open "itt_quotient";;
#open "itt_list";;
#open "itt_theory";;
# 17 "editor_init.prli"

#open "shell";;
# 18 "editor_init.prli"


(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
value eqcd_resource_extern : (eqcd_resource_info, tactic, eqcd_data) resource__rsrc;;
value eqcd_extern : tactic;;
value d_resource_extern : (d_resource_info, d_tactic, d_data) resource__rsrc;;
value dT_extern : d_tactic;;
value refiner : refiner;;
value dformer : dformModeBase;;
# 27 "editor_init.prli"
