(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *  Can write to library, raw marshaled files, or marhaled term files.
 *)

open Printf

open Debug
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan

open File_base_type
open File_type_base

open Filter_type
open Filter_util
open Filter_ocaml
open Filter_summary
open Filter_summary_type
open Filter_summary_io
open Filter_cache_fun

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
 | Interactive of 'a

(*
 * This is the common summary type for interface between IO
 * and marshalers.
 *)
type 'a summary_type =
   Interface of (unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
 | Implementation of ('a proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info

(*
 * Proof conversion.
 *)
module type ConvertProofSig =
sig
   type t
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
let identity x = x

(*
 * Unit term used for interfaces.
 *)
let unit_term = mk_simple_term nil_opname []

(*
 * Normalizer.
 *)
let normalize info =
   let convert =
      { term_f  = normalize_term;
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
let comment loc t = t
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
let interactive_op = mk_opname "interactive" summary_opname

let marshal_proof name to_term = function
   Primitive t ->
      mk_simple_term prim_op [t]
 | Derived expr ->
      mk_simple_term derived_op [term_of_expr expr]
 | Interactive expr ->
      mk_simple_term interactive_op [to_term name expr]

let unmarshal_proof name of_term t =
   let opname = opname_of_term t in
   let expr = one_subterm t in
      if opname == prim_op then
         Primitive expr
      else if opname == derived_op then
         Derived (expr_of_term expr)
      else if opname == interactive_op then
         Interactive (of_term name (one_subterm t))
      else
         raise (Failure "Filter_cache.unmarshal")

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
   let magic    = 0x73ac6be1
   let disabled = nofile

   let marshal = function
      Interface info ->
         let convert =
            { term_f = identity;
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
   type raw     = (unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
   type cooked  = Convert.t summary_type

   let select   = InterfaceType
   let suffix   = "cmiz"
   let magic    = 0x73ac6be2
   let disabled = noraw

   let marshal = function
      Interface info ->
         info
    | Implementation _ ->
         raise (Failure "RawSigInfo.marshal")
   let unmarshal info =
      Interface (normalize info)
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
   let magic    = 0x73ac6be3
   let disabled = nofile

   let marshal = function
      Implementation info ->
         let convert =
            { term_f = identity;
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
   type raw     = (Convert.t proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info
   type cooked  = Convert.t summary_type

   let select   = ImplementationType
   let suffix   = "cmoz"
   let magic    = 0x73ac6be4
   let disabled = noraw

   let marshal = function
      Implementation info ->
         info
    | Interface _ ->
         raise (Failure "RawStrInfo.marshal")
   let unmarshal info =
      Implementation (normalize info)
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
   let magic    = 0x73ac6be5
   let disabled = nolib

   let marshal = function
      Interface info ->
         let convert =
            { term_f = identity;
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
   let magic    = 0x73ac6be6
   let disabled = nolib

   let marshal = function
      Implementation info ->
         let convert =
            { term_f = identity;
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
 * $Log$
 * Revision 1.22  1998/06/01 13:52:47  jyh
 * Proving twice one is two.
 *
 * Revision 1.21  1998/05/28 13:46:11  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.20  1998/05/27 15:12:40  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.19  1998/05/07 16:02:34  jyh
 * Adding interactive proofs.
 *
 * Revision 1.18  1998/05/04 13:01:07  jyh
 * Ocaml display without let rec.
 *
 * Revision 1.17  1998/04/28 21:37:56  jyh
 * Adjusted uppercasing.
 *
 * Revision 1.16  1998/04/24 19:38:16  jyh
 * Updated debugging.
 *
 * Revision 1.15  1998/04/24 02:41:46  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.14  1998/04/17 01:30:57  jyh
 * Editor is almost constructed.
 *
 * Revision 1.13  1998/04/15 12:39:49  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.12  1998/04/13 17:08:27  jyh
 * Adding interactive proofs.
 *
 * Revision 1.11  1998/04/09 18:25:48  jyh
 * Working compiler once again.
 *
 * Revision 1.10  1998/04/08 14:57:06  jyh
 * ImpDag is in mllib.
 *
 * Revision 1.9  1998/03/12 00:27:02  jyh
 * Added filter_html, but its not finished yet.
 *
 * Revision 1.8  1998/03/06 17:05:08  jyh
 * Fixed library choice.
 *
 * Revision 1.7  1998/02/23 14:46:00  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.6  1998/02/19 21:08:19  jyh
 * Adjusted proof type to be primitive or derived.
 *
 * Revision 1.5  1998/02/19 17:13:55  jyh
 * Splitting filter_parse.
 *
 * Revision 1.4  1998/02/18 18:46:12  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.3  1997/09/12 17:21:35  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.2  1997/08/06 16:17:27  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:50:51  jyh
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
 * Revision 1.3  1996/10/23 15:17:54  jyh
 * First working version of dT tactic.
 *
 * Revision 1.2  1996/09/25 22:51:55  jyh
 * Initial "tactical" commit.
 *
 * Revision 1.1  1996/09/02 19:42:45  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
