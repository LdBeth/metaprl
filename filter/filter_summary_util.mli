(*
 * These are utilities for the filter_summary module.
 *)

open Term
open Opname
open Filter_type
open Filter_summary_type
open Filter_cache

(************************************************************************
 * THE FILTER_CACHE                                                     *
 ************************************************************************)

(*
 * For this compiler, we only use two summaries.
 *)
type select_type =
   InterfaceType
 | ImplementationType

(*
 * The interface type and implementation proofs.
 *)
type imp_proof = MLast.expr

type proof_type =
   InterfaceProof
 | ImplementationProof of imp_proof

(*
 * The summary_cache for interfaces and implementations.
 * Right now, neither case has any
 *)
module FilterCache :
   SummaryCacheSig
   with type proof = proof_type
   with type select = select_type
   
(************************************************************************
 * CONTEXT PARAM OPERATORS                                              *
 ************************************************************************)

val param_expr : MLast.loc -> Filter_summary.param -> MLast.expr
val params_ctyp : MLast.loc -> MLast.ctyp -> Filter_summary.param list -> MLast.ctyp
val extract_params : string list -> string list -> term list -> Filter_summary.param list

(*
(************************************************************************
 * OPNAMES                                                              *
 ************************************************************************)

(*
 * Make an opname from an ascii spec.
 *)
val mk_opname : FilterCache.info -> MLast.loc -> string list -> opname
*)

(*
 * $Log$
 * Revision 1.2  1997/08/06 16:17:36  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:01  jyh
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
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
