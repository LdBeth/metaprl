(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *  Can write to library, raw marshaled files, or marhaled term files.
 *)

open Printf

open Nl_debug
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Term_copy

open File_base_type
open File_type_base

open Filter_type
open Filter_util
open Filter_ocaml
open Filter_summary
open Filter_summary_type
open Filter_summary_io
open Filter_cache_fun
open Filter_magic

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_cache%t" eflush


(************************************************************************
 * IO MODULES                                                           *
 ************************************************************************)

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
type 'a proof_type =
   Primitive of term
 | Derived of MLast.expr
 | Incomplete
 | Interactive of 'a

(*
 * This is the common summary type for interface between IO
 * and marshalers.
 *)
type 'a summary_type =
   Interface of (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
 | Implementation of (term, meta_term, 'a proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info

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

(************************************************************************
 * CONFIG                                                               *
 ************************************************************************)

(*
 * We save in three modes:
 *    1. Save to the library
 *    2. Save to the filesystem
 *       a. store raw data to files
 *       b. store data as terms
 *)
let nolib  = ref false
let nofile = ref false
let noraw  = ref false

let set_lib _ _ v =
   if v then
      begin
         nofile := true;
         noraw := true;
         nolib := false
      end
   else
      nolib := true

let set_file _ _ v =
   if v then
      nolib := true
   else
      begin
         nofile := true;
         noraw := true
      end

let set_raw _ _ v =
   if v then
      begin
         nofile := false;
         noraw := false
      end
   else
      noraw := true

let _ = Env_arg.bool "file"   true  "Use the filesystem"      set_file
let _ = Env_arg.bool "raw"    false "Use the raw filesystem"  set_raw
let _ = Env_arg.bool "lib"    false "Use the Nuprl5 library"  set_lib

(************************************************************************
 * IMPLEMTATION                                                         *
 ************************************************************************)

(*
 * Identity used for term normalization.
 *)
external identity : 'a -> 'a = "%identity"

(*
 * Unit term used for interfaces.
 *)
let unit_term = mk_simple_term nil_opname []

(*
 * Save term in term_std format.
 *)
let normalize_info info =
   let share = create_norm () in
   let convert =
      { term_f  = normalize_term share;
        meta_term_f = normalize_meta_term share;
        proof_f = (fun _ pf -> pf);
        ctyp_f  = identity;
        expr_f  = identity;
        item_f  = identity
      }
   in
      summary_map convert info

let denormalize_info info =
   let share = create_denorm () in
   let convert =
      { term_f  = denormalize_term share;
        meta_term_f = denormalize_meta_term share;
        proof_f = (fun _ pf -> pf);
        ctyp_f  = identity;
        expr_f  = identity;
        item_f  = identity
      }
   in
      summary_map convert info

(*
 * When a StrFilterCache ot SigFilterCache is
 * saved, comments are not saved.
 *)
let comment _ _ t = t
let term_of_expr = Filter_ocaml.term_of_expr [] comment
let term_of_type = Filter_ocaml.term_of_type comment
let term_of_sig_item = Filter_ocaml.term_of_sig_item comment
let term_of_str_item = Filter_ocaml.term_of_str_item comment

(*
 * Marshaling proofs.
 *)
let summary_opname = mk_opname "Summary"     nil_opname
let prim_op        = mk_opname "prim"        summary_opname
let derived_op     = mk_opname "derived"     summary_opname
let incomplete_op  = mk_opname "incomplete"  summary_opname
let interactive_op = mk_opname "interactive" summary_opname

let marshal_proof name to_term = function
   Primitive t ->
      mk_simple_term prim_op [t]
 | Derived expr ->
      mk_simple_term derived_op [term_of_expr expr]
 | Incomplete ->
      mk_simple_term incomplete_op []
 | Interactive expr ->
      mk_simple_term interactive_op [to_term name expr]

let unmarshal_proof name of_term t =
   let opname = opname_of_term t in
   let expr = one_subterm t in
      if Opname.eq opname prim_op then
         Primitive expr
      else if Opname.eq opname derived_op then
         Derived (expr_of_term expr)
      else if Opname.eq opname incomplete_op then
         Incomplete
      else if Opname.eq opname interactive_op then
         Interactive (of_term name (one_subterm t))
      else
         raise (Failure "Filter_cache.unmarshal")

let interactive_proof to_raw name proof =
   match proof with
      Primitive t ->
         Primitive t
    | Derived expr ->
         Derived expr
    | Incomplete ->
         Incomplete
    | Interactive pf ->
         Interactive (to_raw name pf)

(*
 * Term signatures.
 *)
module TermSigInfo (Convert : ConvertProofSig) =
struct
   type select = select_type
   type raw    = term
   type cooked = Convert.t summary_type

   let select   = InterfaceType
   let suffix   = "cmit"
   let magics   = [0x73ac6be1; int_term_sig_magic]
   let disabled = nofile

   let marshal = function
      Interface info ->
         let convert =
            { term_f = identity;
              meta_term_f = term_of_meta_term;
              proof_f = (fun _ t -> unit_term);
              ctyp_f = term_of_type;
              expr_f = term_of_expr;
              item_f = term_of_sig_item
            }
         in
            mk_xlist_term (term_list convert info)
    | Implementation _ ->
         raise (Failure "TermSigInfo.unmarshal")

   let unmarshal info =
      let convert =
         { term_f = identity;
           meta_term_f = meta_term_of_term;
           proof_f = (fun _ t -> ());
           ctyp_f = type_of_term;
           expr_f = expr_of_term;
           item_f = sig_item_of_term
         }
      in
         Interface (of_term_list convert (dest_xlist info))
end

(*
 * Raw signatures.
 *)
module RawSigInfo (Convert : ConvertProofSig) =
struct
   type select  = select_type
   type raw     = (Refiner_std_verb.Refiner.TermType.term,
                   Refiner_std_verb.Refiner.TermType.meta_term,
                   unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
   type cooked  = Convert.t summary_type

   let select   = InterfaceType
   let suffix   = "cmiz"
   let magics   = [0x73ac6be2; int_raw_sig_magic]
   let disabled = noraw

   let marshal = function
      Interface info ->
         denormalize_info info
    | Implementation _ ->
         raise (Failure "RawSigInfo.marshal")
   let unmarshal info =
      Interface (normalize_info info)
end

(*
 * Term implementations.
 *)
module TermStrInfo (Convert : ConvertProofSig) =
struct
   type select = select_type
   type raw    = term
   type cooked = Convert.t summary_type

   let select   = ImplementationType
   let suffix   = "cmot"
   let magics   = [0x73ac6be3; int_term_str_magic]
   let disabled = nofile

   let marshal = function
      Implementation info ->
         let convert =
            { term_f = identity;
              meta_term_f = term_of_meta_term;
              proof_f = (fun name pf -> marshal_proof name Convert.to_term pf);
              ctyp_f = term_of_type;
              expr_f = term_of_expr;
              item_f = term_of_str_item
            }
         in
            mk_xlist_term (term_list convert info)
    | Interface _ ->
         raise (Failure "TermStrInfo.marshal")

   let unmarshal info =
      let convert =
         { term_f = identity;
           meta_term_f = meta_term_of_term;
           proof_f = (fun name pf -> unmarshal_proof name Convert.of_term pf);
           ctyp_f = type_of_term;
           expr_f = expr_of_term;
           item_f = str_item_of_term
         }
      in
         Implementation (of_term_list convert (dest_xlist info))
end

(*
 * Raw implementation.
 *)
module RawStrInfo (Convert : ConvertProofSig) =
struct
   type select  = select_type
   type raw     = (Refiner_std_verb.Refiner.TermType.term,
                   Refiner_std_verb.Refiner.TermType.meta_term,
                   Convert.raw proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info
   type cooked  = Convert.t summary_type

   let select   = ImplementationType
   let suffix   = "cmoz"
   let magics   = [0x73ac6be4; int_raw_str_magic]
   let disabled = noraw

   let marshal = function
      Implementation info ->
         let share = create_denorm () in
         let convert =
            { term_f  = denormalize_term share;
              meta_term_f = denormalize_meta_term share;
              proof_f = (fun name proof -> interactive_proof Convert.to_raw name proof);
              ctyp_f  = identity;
              expr_f  = identity;
              item_f  = identity
            }
         in
            summary_map convert info
    | Interface _ ->
         raise (Failure "RawStrInfo.marshal")
   let unmarshal info =
      let share = create_norm () in
      let convert =
         { term_f  = normalize_term share;
           meta_term_f = normalize_meta_term share;
           proof_f = (fun name proof -> interactive_proof Convert.of_raw name proof);
           ctyp_f  = identity;
           expr_f  = identity;
           item_f  = identity
         }
      in
         Implementation (summary_map convert info)
end

(*
 * Library interfaces.
 *)
module LibSigInfo (Convert : ConvertProofSig) =
struct
   type select  = select_type
   type raw     = term
   type cooked  = Convert.t summary_type

   let select   = InterfaceType
   let suffix   = "cmit"
   let magics   = [0x73ac6be5; int_lib_sig_magic]
   let disabled = nolib

   let marshal = function
      Interface info ->
         let convert =
            { term_f = identity;
              meta_term_f = term_of_meta_term;
              proof_f = (fun _ _ -> unit_term);
              ctyp_f = term_of_type;
              expr_f = term_of_expr;
              item_f = term_of_sig_item
            }
         in
            mk_xlist_term (term_list convert info)
    | Implementation _ ->
         raise (Failure "LibSigInfo.marshal")
   let unmarshal info =
      let convert =
         { term_f = identity;
           meta_term_f = meta_term_of_term;
           proof_f = (fun _ t -> ());
           ctyp_f = type_of_term;
           expr_f = expr_of_term;
           item_f = sig_item_of_term
         }
      in
         Interface (of_term_list convert (dest_xlist info))
end

(*
 * Library implementations.
 *)
module LibStrInfo (Convert : ConvertProofSig) =
struct
   type select  = select_type
   type raw     = term
   type cooked  = Convert.t summary_type

   let select   = ImplementationType
   let suffix   = "cmot"
   let magics   = [0x73ac6be6; int_lib_str_magic]
   let disabled = nolib

   let marshal = function
      Implementation info ->
         let convert =
            { term_f = identity;
              meta_term_f = term_of_meta_term;
              proof_f = (fun name pf -> marshal_proof name Convert.to_term pf);
              ctyp_f = term_of_type;
              expr_f = term_of_expr;
              item_f = term_of_str_item
            }
         in
            mk_xlist_term (term_list convert info)
    | Interface _ ->
         raise (Failure "LibStrInfo.marshal")

   let unmarshal info =
      let convert =
         { term_f = identity;
           meta_term_f = meta_term_of_term;
           proof_f = (fun name pf -> unmarshal_proof name Convert.of_term pf);
           ctyp_f = type_of_term;
           expr_f = expr_of_term;
           item_f = str_item_of_term
         }
      in
         Implementation (of_term_list convert (dest_xlist info))
end

(*
 * MArshaler to get interfaces.
 *)
module SigMarshal (Convert : ConvertProofSig) =
struct
   type proof = unit
   type ctyp  = MLast.ctyp
   type expr  = MLast.expr
   type item  = MLast.sig_item

   type select = select_type
   let select = InterfaceType

   type cooked = Convert.t summary_type

   let marshal info =
      Interface info
   let unmarshal = function
      Interface info ->
         info
    | Implementation _ ->
         raise (Failure "SigMarshal.unmarshal")
end

(*
 * Select a submodule.
 *)
module SigAddress (SigMarshal : MarshalSig) =
struct
   type t = SigMarshal.cooked
   let create () = Interface (new_module_info ())
   let find_sub_module info path =
      if path = [] then
         info
      else
         SigMarshal.marshal (Filter_summary.find_sub_module (SigMarshal.unmarshal info) path)
end

(*
 * Marshaler to get implementations.
 *)
module StrMarshal (Convert : ConvertProofSig) =
struct
   type proof = Convert.t proof_type
   type ctyp  = MLast.ctyp
   type expr  = MLast.expr
   type item  = MLast.str_item

   type select = select_type
   let select = ImplementationType

   type cooked = Convert.t summary_type
   let marshal info =
      Implementation info
   let unmarshal = function
      Implementation info ->
         info
    | Interface _ ->
         raise (Failure "StrMarshal.unmarshal")
end

(*
 * Build up the cache.
 *)
module MakeCaches (Convert : ConvertProofSig) =
struct
   module FileTypes =
   struct
      type select = select_type
      type cooked = Convert.t summary_type
   end
   module RawSigInfo1  = RawSigInfo  (Convert)
   module TermSigInfo1 = TermSigInfo (Convert)
   module LibSigInfo1  = LibSigInfo  (Convert)
   module RawStrInfo1  = RawStrInfo  (Convert)
   module TermStrInfo1 = TermStrInfo (Convert)
   module LibStrInfo1  = LibStrInfo  (Convert)
   module RawSigCombo  = MakeSingletonCombo   (RawSigInfo1)
   module TermSigCombo = MakeSingletonCombo   (TermSigInfo1)
   module LibSigCombo  = MakeIOSingletonCombo (Library_type_base.IO) (LibSigInfo1)
   module RawStrCombo  = MakeSingletonCombo   (RawStrInfo1)
   module TermStrCombo = MakeSingletonCombo   (TermStrInfo1)
   module LibStrCombo  = MakeIOSingletonCombo (Library_type_base.IO) (LibStrInfo1)
   module Combo1 = CombineCombo (FileTypes) (RawStrCombo) (RawSigCombo)
   module Combo2 = CombineCombo (FileTypes) (TermStrCombo) (TermSigCombo)
   module Combo3 = CombineCombo (FileTypes) (LibStrCombo)  (LibSigCombo)
   module Combo4 = CombineCombo (FileTypes) (Combo1) (Combo2)
   module Combo5 = CombineCombo (FileTypes) (Combo4) (Combo3)
   module SigMarshal1 = SigMarshal (Convert)
   module SigAddress1 = SigAddress (SigMarshal1)
   module StrMarshal1 = StrMarshal (Convert)
   module FileBase =  MakeFileBase (FileTypes) (Combo5)
   module SummaryBase = MakeSummaryBase (SigAddress1) (FileBase)
   module SigFilterCache = MakeFilterCache (SigMarshal1) (SigMarshal1) (SummaryBase)
   module StrFilterCache = MakeFilterCache (SigMarshal1) (StrMarshal1) (SummaryBase)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
