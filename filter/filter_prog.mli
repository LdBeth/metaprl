(*
 * Conversion form filter_summary to program text.
 *)

open Refiner.Refiner.TermType
open Refiner.Refiner.Refine

open Filter_type
open Filter_summary_type
open Filter_summary
open Filter_cache

(*
 * Signature for extract module.
 *)
module type ExtractSig =
sig
   type proof

   val extract_sig :
      (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (module_path * MLast.ctyp resource_info) list ->
      string -> (MLast.sig_item * (int * int)) list

   val extract_str :
      (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (term, meta_term, proof proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info ->
      (module_path * MLast.ctyp resource_info) list ->
      string -> (MLast.str_item * (int * int)) list

   (*
    * Defining implementations.
    *)
   type t

   val prim_axiom : t -> loc -> (term, 'proof) axiom_info -> term -> MLast.str_item list
   val derived_axiom : t -> loc -> (term, 'proof) axiom_info -> MLast.expr -> MLast.str_item list

   val prim_rule : t -> loc -> (term, meta_term, 'proof) rule_info -> term -> MLast.str_item list
   val derived_rule : t -> loc -> (term, meta_term, 'proof) rule_info -> MLast.expr -> MLast.str_item list

   val prim_rewrite : t -> loc -> (term, 'proof) rewrite_info -> MLast.str_item list
   val derived_rewrite : t -> loc -> (term, 'proof) rewrite_info -> MLast.expr -> MLast.str_item list

   val prim_cond_rewrite : t -> loc -> (term, 'proof) cond_rewrite_info -> MLast.str_item list
   val derived_cond_rewrite : t -> loc -> (term, 'proof) cond_rewrite_info -> MLast.expr -> MLast.str_item list

   val define_dform : t -> loc -> (term, MLast.expr) dform_info -> term -> MLast.str_item list
   val define_prec : t -> loc -> string -> MLast.str_item list
   val define_prec_rel : t -> loc -> prec_rel_info -> MLast.str_item list
   val define_resource : t -> loc -> MLast.ctyp resource_info -> MLast.str_item list
   val define_parent : t -> loc -> MLast.ctyp parent_info -> MLast.str_item list
   val define_magic_block : t -> loc -> MLast.str_item magic_info -> MLast.str_item list

   val implem_prolog : t -> loc -> MLast.str_item list
   val implem_postlog : t -> loc -> string -> MLast.str_item list
end

module MakeExtract (Convert : ConvertProofSig) :
   ExtractSig with type proof = Convert.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
