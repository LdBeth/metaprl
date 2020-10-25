(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *  Can write to library, raw marshaled files, or marhaled term files.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_debug

open Opname

open File_type_base

open Refiner_sig
open Refiner_io
open Refiner.Refiner
open Refiner.Refiner.TermType

open Tactic_boot_sig
open Filter_type
open Filter_summary
open Filter_summary_type
open Filter_summary_io
open Filter_cache_fun
open Filter_magic

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_cache%t"

(************************************************************************
 * IO MODULES                                                           *
 ************************************************************************)

(*
 * This is the common summary type for interface between IO
 * and marshalers.
 *)
type 'a summary_type =
   Interface of (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
 | Implementation of (term, meta_term, 'a proof_type, (MLast.ctyp, MLast.expr) resource_str, MLast.ctyp, MLast.expr, MLast.str_item) module_info

type term_io = Refiner_io.TermType.term
type meta_term_io = Refiner_io.TermType.meta_term

module type ConvertInternalSig =
sig
   type t
   type cooked
   type term

   val interface_suffix : string
   val implementation_suffix : string

   val to_term : t -> string -> cooked -> term
   val of_term : t -> string -> term -> cooked
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

let _ = Env_arg.bool "raw"    false "Use the raw filesystem"  set_raw
let _ = Env_arg.bool "file"   false "Use the term filesystem" set_file
let _ = Env_arg.bool "lib5"   true  "Use the Nuprl5 library"  set_lib

let set_raw () =
   nofile := true;
   nolib := true;
   noraw := false

let set_file () =
   nofile := false;
   nolib := true;
   noraw := true

let set_lib () =
   nofile := true;
   nolib := false;
   noraw := true

(************************************************************************
 * IMPLEMTATION                                                         *
 ************************************************************************)

(*
 * Term signatures.
 *)
module FSummary = Filter_summary.FilterSummaryTerm (Refiner.Refiner);;
(* unused
module FTerm = Refiner.Refiner.Term
*)
module FTermCopy = Term_copy2_weak.TermCopy2Weak (Refiner.Refiner) (Refiner_io)
let term_of_meta_term = FSummary.term_of_meta_term

(*
 * Raw versions to file marshal the Filter_summary.info directly to the
 * file, but terms are converted to term_io, since that seems pretty stable.
 *)
let identity x = x

module MakeRawSigInfo (Convert : ConvertInternalSig) =
struct
   type select  = select_type
   type raw     = (term_io, meta_term_io, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
   type cooked  = Convert.cooked summary_type
   type arg     = Convert.t

   let select   = InterfaceType
   let suffix   = Convert.interface_suffix
   let magics   = [0x73ac6be6; int_raw_sig_magic]
   let versions = raw_versions
   let disabled = noraw

   let marshal arg = function
      Interface info ->
         let convert =
            { term_f  = FTermCopy.convert;
              meta_term_f = FTermCopy.convert_meta;
              proof_f = (fun _ pf -> pf);
              resource_f = identity;
              ctyp_f  = identity;
              expr_f  = identity;
              item_f  = identity
            }
         in
            summary_map convert info
    | Implementation _ ->
         raise (Failure "RawStrInfo.marshal")

   let unmarshal arg info =
      let convert =
         { term_f  = FTermCopy.revert;
           meta_term_f = FTermCopy.revert_meta;
           proof_f = (fun _ pf -> pf);
           resource_f = identity;
           ctyp_f  = identity;
           expr_f  = identity;
           item_f  = identity
         }
      in
         Interface (summary_map convert info)
end

module MakeRawStrInfo (Convert : ConvertInternalSig) =
struct
   type select  = select_type
   type raw     = (term_io, meta_term_io, Convert.term proof_type,
                   (MLast.ctyp, MLast.expr) resource_str, MLast.ctyp, MLast.expr, MLast.str_item) module_info
   type cooked  = Convert.cooked summary_type
   type arg    = Convert.t

   let select   = ImplementationType
   let suffix   = Convert.implementation_suffix
   let magics   = [0x73ac6be8; int_raw_str_magic]
   let versions = raw_versions
   let disabled = noraw

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

   let marshal arg = function
      Implementation info ->
         let convert =
            { term_f  = FTermCopy.convert;
              meta_term_f = FTermCopy.convert_meta;
              proof_f = (fun name proof -> interactive_proof (Convert.to_term arg) name proof);
              resource_f = identity;
              ctyp_f  = identity;
              expr_f  = identity;
              item_f  = identity
            }
         in
            summary_map convert info
    | Interface _ ->
         raise (Failure "RawStrInfo.marshal")

   let unmarshal arg info =
      let convert =
         { term_f  = FTermCopy.revert;
           meta_term_f = FTermCopy.revert_meta;
           proof_f = (fun name proof -> interactive_proof (Convert.of_term arg) name proof);
           resource_f = identity;
           ctyp_f  = identity;
           expr_f  = identity;
           item_f  = identity
         }
      in
         Implementation (summary_map convert info)
end

(*
 * Save terms to files.
 *)
module type MagicInfo =
sig
   val versions : int list
   val sig_magics : int list
   val str_magics : int list
end

module MakeInfo (Magic : MagicInfo) (ToTerm : RefinerSig) =
struct
   module TTerm = ToTerm.Term
   module TTermOp = ToTerm.TermOp
   module TTermMan = ToTerm.TermMan

   module TOCaml = Filter_ocaml.FilterOCaml (ToTerm);;
   module TSummary = Filter_summary.FilterSummaryTerm (ToTerm);;
   module TTermCopy = Term_copy2_weak.TermCopy2Weak (Refiner.Refiner) (ToTerm);;

   open TOCaml

   (*
    * Identity used for term normalization.
    *)
(* unused
   let identity x = x (* external identity : 'a -> 'a = "%identity" *)
*)

   (*
    * Unit term used for interfaces.
    *)
   let unit_term = TTerm.mk_simple_term nil_opname []

   let term_of_expr = TOCaml.term_of_expr []
   let term_of_str_item = TOCaml.term_of_str_item []

   let str_item_of_term = TOCaml.str_item_of_term_nofail

   (*
    * Marshaling proofs.
    *)
   let summary_opname = mk_opname "Summary"     nil_opname
   let prim_op        = mk_opname "prim"        summary_opname
   let derived_op     = mk_opname "derived"     summary_opname
   let incomplete_op  = mk_opname "incomplete"  summary_opname
   let interactive_op = mk_opname "interactive" summary_opname

   let incomplete_pf  = TTerm.mk_simple_term incomplete_op []

   let marshal_proof name to_term = function
      Primitive t ->
         TTerm.mk_simple_term prim_op [TTermCopy.convert t]
    | Derived expr ->
         TTerm.mk_simple_term derived_op [term_of_expr expr]
    | Incomplete ->
         incomplete_pf
    | Interactive expr ->
         TTerm.mk_simple_term interactive_op [to_term name expr]

   let unmarshal_proof name of_term t =
      let opname = TTerm.opname_of_term t in
         if Opname.eq opname prim_op then
            Primitive (TTermCopy.revert (TTermOp.one_subterm t))
         else if Opname.eq opname derived_op then
            Derived (TOCaml.expr_of_term (TTermOp.one_subterm t))
         else if Opname.eq opname incomplete_op then
            Incomplete
         else if Opname.eq opname interactive_op then
            Interactive (of_term name (TTermOp.one_subterm t))
         else
            raise (Failure "Filter_cache.unmarshal")

   module MakeSigInfo (Convert : ConvertInternalSig with type term = ToTerm.TermType.term) =
   struct
      type select = select_type
      type raw    = ToTerm.TermType.term
      type cooked = Convert.cooked summary_type
      type arg    = Convert.t

      let select   = InterfaceType
      let suffix   = Convert.interface_suffix
      let magics   = Magic.sig_magics
      let versions = Magic.versions
      let disabled = nofile

      let marshal arg = function
         Interface info ->
            let convert =
               { term_f = TTermCopy.convert;
                 meta_term_f = (fun t -> TTermCopy.convert (term_of_meta_term t));
                 proof_f = (fun _ t -> unit_term);
                 resource_f = TOCaml.term_of_resource_sig resource_op;
                 ctyp_f = term_of_type;
                 expr_f = term_of_expr;
                 item_f = term_of_sig_item
               }
            in
               TTermMan.mk_xlist_term (TSummary.term_list convert info)
       | Implementation _ ->
            raise (Failure "TermSigInfo.unmarshal")

      let unmarshal arg info =
         let convert =
            { term_f = TTermCopy.revert;
              meta_term_f = (fun t -> TTermCopy.revert_meta (TSummary.meta_term_of_term t));
              proof_f = (fun _ t -> ());
              resource_f = TOCaml.resource_sig_of_term;
              ctyp_f = type_of_term;
              expr_f = expr_of_term;
              item_f = sig_item_of_term
            }
         in
            Interface (TSummary.of_term_list convert (TTermMan.dest_xlist info))
   end

   (*
    * Term implementations.
    *)
   module MakeStrInfo (Convert : ConvertInternalSig with type term = ToTerm.TermType.term) =
   struct
      type select = select_type
      type raw    = ToTerm.TermType.term
      type cooked = Convert.cooked summary_type
      type arg    = Convert.t

      let select   = ImplementationType
      let suffix   = Convert.implementation_suffix
      let magics   = Magic.str_magics
      let versions = Magic.versions
      let disabled = nofile

      let marshal arg = function
         Implementation info ->
            let convert =
               { term_f = TTermCopy.convert;
                 meta_term_f = (fun t -> TTermCopy.convert (term_of_meta_term t));
                 proof_f = (fun name pf -> marshal_proof name (Convert.to_term arg) pf);
                 resource_f = TOCaml.term_of_resource_str resource_op;
                 ctyp_f = term_of_type;
                 expr_f = term_of_expr;
                 item_f = term_of_str_item
               }
            in
               TTermMan.mk_xlist_term (TSummary.term_list convert info)
       | Interface _ ->
            raise (Failure "TermStrInfo.marshal")

      let unmarshal arg info =
         let convert =
            { term_f = TTermCopy.revert;
              meta_term_f = (fun t -> TTermCopy.revert_meta (TSummary.meta_term_of_term t));
              proof_f = (fun name pf -> unmarshal_proof name (Convert.of_term arg) pf);
              resource_f = TOCaml.resource_str_of_term resource_op;
              ctyp_f = type_of_term;
              expr_f = expr_of_term;
              item_f = str_item_of_term
            }
         in
            Implementation (TSummary.of_term_list convert (TTermMan.dest_xlist info))
   end
end

module StandardMagic =
struct
   let versions = term_versions
   let sig_magics = [0x73ac6be1; int_term_sig_magic]
   let str_magics = [0x73ac6be3; int_term_str_magic]
end

(*
 * These magic numbers should be implemented in the Ascii_io module.
 *)
module AsciiMagic =
struct
   let versions = ascii_versions
   let sig_magics = [0]
   let str_magics = [0]
end

(*
 * Marshaler to get interfaces.
 *)
module SigMarshal (Convert : ConvertProofSig) =
struct
   type proof = unit
   type ctyp  = MLast.ctyp
   type expr  = MLast.expr
   type item  = MLast.sig_item
   type resource = MLast.ctyp resource_sig
(* unused
   type arg   = Convert.t

   type select = select_type
 *)
   let select = InterfaceType

   type cooked = Convert.cooked summary_type

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
   type proof = Convert.cooked proof_type
   type ctyp  = MLast.ctyp
   type expr  = MLast.expr
   type item  = MLast.str_item
(* unused
   type arg   = Convert.t
*)
   type resource = (ctyp, expr) resource_str

(* unused
   type select = select_type
*)
   let select = ImplementationType

   type cooked = Convert.cooked summary_type
   let marshal info =
      Implementation info
   let unmarshal = function
      Implementation info ->
         info
    | Interface _ ->
         raise (Failure "StrMarshal.unmarshal")
end

(*
 * ASCII IO module.
 *)
module AsciiIO =
struct
   module PreAsciiIO = Ascii_io.AsciiIO

   (*
    * Terms are written in old format.
    *)
   type t = term

   (*
    * Read the term, checking the file format.
    *)
   let read_table magics versions filename =
      let inx = open_in filename in
      let magic, version =
         try
            match List.map String.uppercase (Lm_string_util.parse_args (input_line inx)) with
               "#PRL" :: "VERSION" :: code :: _ ->
                  let major, minor, rev =
                     match List.map int_of_string (Lm_string_util.split "." code) with
                        [] ->
                           raise (Failure "read_table")
                      | [major] ->
                           major, 0, 0
                      | [major; minor] ->
                           major, minor, 0
                      | major :: minor :: rev :: _ ->
                           major, minor, rev
                  in
                     0, pack_version major minor rev

             | _ ->
                  -1, 0
         with
            End_of_file
          | Failure _ ->
               -1, 0
          | exn ->
               close_in inx;
               raise exn
      in
      let magic =
         try Lm_list_util.find_index magic magics with
            Not_found ->
               close_in inx;
               raise (Bad_magic filename)
      in
         if not (List.mem version versions) then begin
            close_in inx;
            raise (Bad_version(filename, versions, version))
         end;
         let table = PreAsciiIO.read_table inx in
            close_in inx;
            table, magic

   (*
    * Read a term from the file.
    *)
   let read magics versions filename =
      let table, magic = read_table magics versions filename in
         PreAsciiIO.get_term table, magic

   (*
    * Write a term to the file.
    * First read the table, then write the new term.
    *)
   let write magics magic versions filename term =
      (* First read the table *)
      let table =
         try fst (read_table magics versions filename) with
            Failure _
          | Sys_error _ ->
               PreAsciiIO.initialize ()
      in

      (* Now write the term *)
      let newname = filename ^ ".new" in
      let outx = Stdlib.open_out_bin newname in
      let major, minor, rev = unpack_version (List.hd versions) in
         Printf.fprintf outx "#PRL version %d.%d.%d ASCII term\n" major minor rev;
         PreAsciiIO.write_term outx table term;
         Stdlib.close_out outx;
         Unix.rename newname filename
end

(*
 * Build up the cache.
 *)
module MakeCaches (Convert : ConvertProofSig) =
struct
   type t = Convert.t
   type cooked = Convert.cooked

   module FileTypes =
   struct
      type select = select_type
      type cooked = Convert.cooked summary_type
      type arg = Convert.t
   end

   module ConvertRaw =
   struct
      type t = Convert.t
      type cooked = Convert.cooked
      type term = Convert.raw
      let interface_suffix = "cmiz"
      let implementation_suffix = "cmoz"
      let of_term = Convert.of_raw
      let to_term = Convert.to_raw
   end

   module ConvertStd =
   struct
      type t = Convert.t
      type cooked = Convert.cooked
      type term = term_io
      let interface_suffix = "cmit"
      let implementation_suffix = "cmot"
      let of_term = Convert.of_term_io
      let to_term = Convert.to_term_io
   end

   module ConvertRef =
   struct
      type t = Convert.t
      type cooked = Convert.cooked
      type term = Refiner.Refiner.TermType.term
      let interface_suffix = "cmil"
      let implementation_suffix = "cmol"
      let of_term = Convert.of_term
      let to_term = Convert.to_term
   end

   (*
    * Allow reading of prlb from 4 formats: raw, term, term_io, and ASCII.
    * Use raw format for writing.
    *)
   module ConvertPrlRaw =
   struct
      type t = Convert.t
      type cooked = Convert.cooked
      type term = Convert.raw
      let interface_suffix = "prlbi"
      let implementation_suffix = "prlb"
      let of_term = Convert.of_raw
      let to_term = Convert.to_raw
   end

   module ConvertPrlTerm =
   struct
      type t = Convert.t
      type cooked = Convert.cooked
      type term = Refine.term
      let interface_suffix = "prlbi"
      let implementation_suffix = "prlb"
      let of_term = Convert.of_term
      let to_term = Convert.to_term
   end

   module ConvertPrlTermIO =
   struct
      type t = Convert.t
      type cooked = Convert.cooked
      type term = term_io
      let interface_suffix = "prlbi"
      let implementation_suffix = "prlb"
      let of_term = Convert.of_term_io
      let to_term = Convert.to_term_io
   end

   module ConvertPrlASCII =
   struct
      type t = Convert.t
      type cooked = Convert.cooked
      type term = TermType.term
      let interface_suffix = "prlai"
      let implementation_suffix = "prla"
      let of_term = Convert.of_term
      let to_term = Convert.to_term
   end

   module TermInfo     = MakeInfo (StandardMagic) (Refiner_io)
   module AsciiInfo    = MakeInfo (AsciiMagic) (Refiner.Refiner)
   module LibInfo      = MakeInfo (StandardMagic) (Refiner.Refiner)

   module RawSigInfo1  = MakeRawSigInfo       (ConvertRaw)
   module RawStrInfo1  = MakeRawStrInfo       (ConvertRaw)
   module TermSigInfo1 = TermInfo.MakeSigInfo (ConvertStd)
   module TermStrInfo1 = TermInfo.MakeStrInfo (ConvertStd)
   module LibSigInfo1  = LibInfo.MakeSigInfo  (ConvertRef)
   module LibStrInfo1  = LibInfo.MakeStrInfo  (ConvertRef)
   module PrlStrInfo1  = MakeRawStrInfo       (ConvertPrlRaw)
   module PrlStrInfo2  = MakeRawStrInfo       (ConvertPrlTerm)
   module PrlStrInfo3  = TermInfo.MakeStrInfo (ConvertPrlTermIO)
   module PrlStrInfo4  = AsciiInfo.MakeStrInfo (ConvertPrlASCII)

   module RawSigCombo  = MakeSingletonCombo   (RawSigInfo1)
   module RawStrCombo  = MakeSingletonCombo   (RawStrInfo1)
   module TermSigCombo = MakeSingletonCombo   (TermSigInfo1)
   module TermStrCombo = MakeSingletonCombo   (TermStrInfo1)
   module LibSigCombo  = MakeIOSingletonCombo (Library_type_base.IO) (LibSigInfo1)
   module LibStrCombo  = MakeIOSingletonCombo (Library_type_base.IO) (LibStrInfo1)
   module PrlStrCombo1 = MakeSingletonCombo   (PrlStrInfo1)
   module PrlStrCombo2 = MakeSingletonCombo   (PrlStrInfo2)
   module PrlStrCombo3 = MakeSingletonCombo   (PrlStrInfo3)
   module PrlStrCombo4 = MakeIOSingletonCombo (AsciiIO) (PrlStrInfo4)
   module Combo1  = CombineCombo (FileTypes) (RawStrCombo) (RawSigCombo)
   module Combo2  = CombineCombo (FileTypes) (TermStrCombo) (TermSigCombo)
   module Combo3  = CombineCombo (FileTypes) (LibStrCombo) (LibSigCombo)
   module Combo4a = CombineCombo (FileTypes) (PrlStrCombo1) (PrlStrCombo2)
   module Combo4b = CombineCombo (FileTypes) (PrlStrCombo3) (PrlStrCombo4)
   module Combo4  = CombineCombo (FileTypes) (Combo4a) (Combo4b)
   module Combo5  = CombineCombo (FileTypes) (Combo1) (Combo2)
   module Combo6  = CombineCombo (FileTypes) (Combo3) (Combo4)
   module Combo7  = CombineCombo (FileTypes) (Combo5) (Combo6)
   module SigMarshal1 = SigMarshal (Convert)
   module SigAddress1 = SigAddress (SigMarshal1)
   module StrMarshal1 = StrMarshal (Convert)
   module FileBase =  MakeFileBase (FileTypes) (Combo7)
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
