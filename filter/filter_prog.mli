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
 * $Log$
 * Revision 1.13  1998/07/03 21:04:17  nogin
 * Specified the full "path" to the Refine module:
 * open Refiner.Refiner.Refine
 *
 * Revision 1.12  1998/07/02 22:24:48  jyh
 * Created term_copy module to copy and normalize terms.
 *
 * Revision 1.11  1998/06/01 13:53:11  jyh
 * Proving twice one is two.
 *
 * Revision 1.10  1998/05/28 13:46:23  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.9  1998/05/27 15:12:56  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.8  1998/05/07 16:02:44  jyh
 * Adding interactive proofs.
 *
 * Revision 1.7  1998/04/21 19:53:39  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.6  1998/04/17 20:48:28  jyh
 * Updating refiner for extraction.
 *
 * Revision 1.5  1998/04/15 12:40:01  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.4  1998/04/13 17:08:36  jyh
 * Adding interactive proofs.
 *
 * Revision 1.3  1998/04/09 18:25:52  jyh
 * Working compiler once again.
 *
 * Revision 1.2  1998/02/23 14:46:16  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1998/02/21 20:57:49  jyh
 * Two phase parse/extract.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
