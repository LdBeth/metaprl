(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Printf

open Debug
open Opname
open Term

open File_base_type
open File_type_base

open Filter_debug
open Filter_type
open Filter_util
open Filter_ocaml
open Filter_summary
open Filter_summary_type
open Filter_summary_io
open Filter_cache_fun

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

let _ = Env_arg.bool "lib"    false "Use the Nuprl5 library"  set_lib
let _ = Env_arg.bool "file"   true  "Use the filesystem"      set_file
let _ = Env_arg.bool "raw"    false "Use the raw filesystem"  set_raw

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
 * This is the common summary type for interface between IO
 * and marshalers.
 *)
type summary_type =
   Interface of (unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
 | Implementation of (MLast.expr, MLast.ctyp, MLast.expr, MLast.str_item) module_info

(*
 * Types of objects that are stored in the files.
 *)
module FileTypes =
struct
   type select = select_type
   type cooked = summary_type
end

(*
 * Identity used for term normalization.
 *)
let identity x = x

(*
 * Unit term used for interfaces.
 *)
let unit_term = mk_simple_term nil_opname []

(*
 * When a StrFilterCache ot SigFilterCache is
 * saved, comments are not saved.
 *)
let comment loc t = t
let term_of_expr = Filter_ocaml.term_of_expr comment
let term_of_type = Filter_ocaml.term_of_type comment
let term_of_sig_item = Filter_ocaml.term_of_sig_item comment
let term_of_str_item = Filter_ocaml.term_of_str_item comment

(*
 * Term signatures.
 *)
module TermSigInfo =
struct
   type select = select_type
   type raw    = term
   type cooked = summary_type
   
   let select   = InterfaceType
   let suffix   = "cmit"
   let magic    = 0x73ac6be1
   let disabled = nofile

   let marshal = function
      Interface info ->
         let convert =
            { term_f = identity;
              proof_f = (fun t -> unit_term);
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
           proof_f = (fun t -> ());
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
module RawSigInfo =
struct
   type select  = select_type
   type raw     = (unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
   type cooked  = summary_type
   
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
      Interface info
end

(*
 * Term implementations.
 *)
module TermStrInfo =
struct
   type select = select_type
   type raw    = term
   type cooked = summary_type
   
   let select   = ImplementationType
   let suffix   = "cmot"
   let magic    = 0x73ac6be3
   let disabled = nofile

   let marshal = function
      Implementation info ->
         let convert =
            { term_f = identity;
              proof_f = term_of_expr;
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
           proof_f = expr_of_term;
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
module RawStrInfo =
struct
   type select  = select_type
   type raw     = (MLast.expr, MLast.ctyp, MLast.expr, MLast.str_item) module_info
   type cooked  = summary_type
   
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
      Implementation info
end

(*
 * Library interfaces.
 *)
module LibSigInfo =
struct
   type select  = select_type
   type raw     = term
   type cooked  = summary_type
   
   let select   = InterfaceType
   let suffix   = "cmit"
   let magic    = 0x73ac6be5
   let disabled = nolib

   let marshal = function
      Interface info ->
         let convert =
            { term_f = identity;
              proof_f = (fun t -> unit_term);
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
           proof_f = (fun t -> ());
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
module LibStrInfo =
struct
   type select  = select_type
   type raw     = term
   type cooked  = summary_type
   
   let select   = ImplementationType
   let suffix   = "cmoz"
   let magic    = 0x73ac6be6
   let disabled = nolib

   let marshal = function
      Implementation info ->
         let convert =
            { term_f = identity;
              proof_f = term_of_expr;
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
           proof_f = expr_of_term;
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
module SigMarshal =
struct
   type proof = unit
   type ctyp  = MLast.ctyp
   type expr  = MLast.expr
   type item  = MLast.sig_item

   type select = select_type
   let select = InterfaceType

   type cooked = summary_type

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
module SigAddress =
struct
   type t = summary_type
   let create () = Interface (new_module_info ())
   let find_sub_module info path =
      SigMarshal.marshal (Filter_summary.find_sub_module (SigMarshal.unmarshal info) path)
end

(*
 * Marshaler to get implementations.
 *)
module StrMarshal =
struct
   type proof = MLast.expr
   type ctyp  = MLast.ctyp
   type expr  = MLast.expr
   type item  = MLast.str_item

   type select = select_type
   let select = InterfaceType

   type cooked = summary_type
   let marshal info =
      Implementation info
   let unmarshal = function
      Implementation info ->
         info
    | Interface _ ->
         raise (Failure "SigMarshal.unmarshal")
end

(*
 * Build up the cache.
 *)
module RawSigCombo    = MakeSingletonCombo (RawSigInfo)
module RawStrCombo    = MakeSingletonCombo (RawStrInfo)
module TermSigCombo   = MakeSingletonCombo (TermSigInfo)
module TermStrCombo   = MakeSingletonCombo (TermStrInfo)
module LibSigCombo    = MakeSingletonCombo (LibSigInfo)
module LibStrCombo    = MakeSingletonCombo (LibStrInfo)
module RawCombo       = CombineCombo (FileTypes) (RawSigCombo)  (RawStrCombo)
module TermCombo      = CombineCombo (FileTypes) (TermSigCombo) (TermStrCombo)
module LibCombo       = CombineCombo (FileTypes) (LibSigCombo)  (LibStrCombo)
module FileCombo      = CombineCombo (FileTypes) (RawCombo)     (TermCombo)
module Combo          = CombineCombo (FileTypes) (FileCombo)    (LibCombo)

module FileBase       = MakeFileBase    (FileTypes)  (Combo)
module SummaryBase    = MakeSummaryBase (SigAddress) (FileBase)

module SigFilterCache = MakeFilterCache (SigMarshal) (SigMarshal) (SummaryBase)
module StrFilterCache = MakeFilterCache (SigMarshal) (StrMarshal) (SummaryBase)

(*
 * $Log$
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
