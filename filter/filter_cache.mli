(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Term

open Filter_summary_type

(*
 * For this compiler, we only use two summaries.
 *)
type select_type =
   InterfaceType
 | ImplementationType

(*
 * Proofs are either primitive terms,
 * or they are tactics.
 *)
type proof_type =
   Primitive of term
 | Derived of MLast.expr

(*
 * The summary_cache for interfaces and implementations.
 *)
module SigFilterCache :
   SummaryCacheSig
   with type sig_proof  = unit
   with type sig_ctyp   = MLast.ctyp
   with type sig_expr   = MLast.expr
   with type sig_item   = MLast.sig_item
   with type str_proof  = unit
   with type str_ctyp   = MLast.ctyp
   with type str_expr   = MLast.expr
   with type str_item   = MLast.sig_item
   with type select = select_type
   
module StrFilterCache :
   SummaryCacheSig
   with type sig_proof  = unit
   with type sig_ctyp   = MLast.ctyp
   with type sig_expr   = MLast.expr
   with type sig_item   = MLast.sig_item
   with type str_proof  = proof_type
   with type str_ctyp   = MLast.ctyp
   with type str_expr   = MLast.expr
   with type str_item   = MLast.str_item
   with type select = select_type
   
(*
 * $Log$
 * Revision 1.4  1998/02/19 21:08:20  jyh
 * Adjusted proof type to be primitive or derived.
 *
 * Revision 1.3  1998/02/19 17:13:55  jyh
 * Splitting filter_parse.
 *
 * Revision 1.2  1997/08/06 16:17:28  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:50:52  jyh
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
 * Revision 1.2  1996/10/23 15:17:55  jyh
 * First working version of dT tactic.
 *
 * Revision 1.1  1996/09/02 19:42:47  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
