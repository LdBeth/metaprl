(*
 * These are the specific types we use in this filter.
 *)

(*
 * This type may contain something useful after a while.
 *)
type imp_proof = MLast.expr

(*
 * For this compiler, we only use two summaries.
 *)
type select_type =
   InterfaceType
 | ImplementationType

(*
 * The interface type and implementation proofs.
 *)
type proof_type =
   InterfaceProof
 | ImplementationProof of imp_proof

(*
 * $Log$
 * Revision 1.1  1998/02/18 18:46:23  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.1  1998/02/12 23:38:17  jyh
 * Added support for saving intermediate files to the library.
 *
 *)