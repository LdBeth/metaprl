(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Refiner.Refiner.Term

open Filter_summary_type

(*
 * For this compiler, we only use two summaries.
 *)
type select_type =
   InterfaceType
 | ImplementationType

(*
 * Proofs are:
 *   1. primitive terms,
 *   2. tactics.
 *   3. inferred from interactive proofs
 *)
type 'a proof_type =
   Primitive of term
 | Derived of MLast.expr
 | Incomplete
 | Interactive of 'a

(*
 * Proof conversion.
 *)
module type ConvertProofSig =
sig
   type t
   type raw
   val to_raw  : string -> t -> raw
   val of_raw  : string -> raw -> t
   val to_expr : string -> t -> MLast.expr
   val to_term : string -> t -> term
   val of_term : string -> term -> t
end

(*
 * Build a cache with a particular proof type.
 *)
module MakeCaches (Convert : ConvertProofSig) :
sig
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
      with type select     = select_type

   module StrFilterCache :
      SummaryCacheSig
      with type sig_proof  = unit
      with type sig_ctyp   = MLast.ctyp
      with type sig_expr   = MLast.expr
      with type sig_item   = MLast.sig_item
      with type str_proof  = Convert.t proof_type
      with type str_ctyp   = MLast.ctyp
      with type str_expr   = MLast.expr
      with type str_item   = MLast.str_item
      with type select     = select_type
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
