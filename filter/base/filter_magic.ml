(*
 * This file defines version numbers and magic numbers for some
 * of the MetaPRL file formats.
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
 * Copyright (C) 1999-2006 MetaPRL Group, Cornell University and California
 * Institute of Technology
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

open File_type_base

(************************************************************************
 * FILE FORMAT VERSIONS SUPPORTED                                       *
 ************************************************************************
 *
 * Current MD5 hash of the summary-related types (as computed by OMake):
 * FILTER_MD5: 644f9a34f03bcf152728b2a2c54bfe7a
 *
 * The arguments for pack_version are:
 *    major version number, minor sub-version number, revision number
 * In the list of versions, the first version is the version number of what we write,
 * and the full list gives the versions we are capable of reading.
 *)

(*
 * Raw data structures revision history
 *
 * Rev 0: whatever was around when the versioning was added
 * Rev 1: added PRL bindings to several summary items (many types have chaged)
 * Rev 2: variables are now implemented as symbols.
 * Rev 3: got rid of MVar parameters
 * Rev 4: added context args to SO variables and contexts
 * Rev 5: count second--order variables as free variables
 * Rev 6: added a "suffix" summary item;
 *        removed the "parent_opens" field from the parent_info type
 * Rev 7: removed the "ref_parent" fields from the tactic_arg and related types.
 * Rev 8: removed the Hypothesis/HypBinding distinction
 * Rev 9: removed the "internal" display form flag.
 * Rev 10: added input/optput types to resource definitions (bug 168).
 * Rev 11: added the "Quote" parameter
 * Rev 12: added the "Grammar" choice to the summary_item type
 * Rev 13: switched to single-conclusion sequents
 * Rev 14: address arguments to rules and rewrites
 * Rev 15: tokens now use opnames instead of strings
 * Rev 16: removed "Opname" and "Definition" from the summary_item type
 * Rev 17: changed the Filter_grammar definition
 * Rev 18: changed the mseq_vars field in the msequent type definition
 * Rev 19: in Term_ds, added Context choice to the term_core type
 * Rev 20: iforms no longer have proofs and resources
 * Rev 21: added "shape" parameters
 * Rev 22: added iform/normal classification to opname declarations
 * Rev 23: changed the string comparison function, this affects how the string and
 *         variable sets are represented in binary format.
 * Rev 24: added "Operator" term parameters.
 * Rev 25: added "Annotate" proof terms.
 * Rev 26: added "RWAvoidBindings" to term rewrites (raw I/O only)
 * Rev 27: added "const" annotation to 0-arity "declare"/"define"
 * Rev 28: added "private" resource annotations
 * Rev 29: added "RawOption" raw attribute
 * Rev 30: changed the options_table type; changed the string encoding of option_info
 * Rev 31: updated the variable name handling in the rewriter
 * Rev 32: moved the type of rule parameters into Filter_base_type
 * Rev 33: added "opaque" qualifier to the "define" statements
 * Rev 34: there were a number changes in Lm_hash, some are probably incompatible
 *)
let raw_versions = List.map (pack_version 1 0) [34]

let term_versions = List.map (pack_version 1 0) [34;33;32;31;30;29;28;27;25;24]

(*
 * ASCII IO format revision history:
 *
 * Rev 0: whatever was around when the versioning was added
 * Rev 1: added new hypothesis syntax (hyps with and w/o bindings)
 * Rev 2: removed support for variable name (string) arguments to rules/rewrites
 * Rev 3: added a real summary item for "define" directives (instead of declare + prim_rw implementation)
 * Rev 4: corrected term representation for prec_rel
 * Rev 5: added PRL bindings to several summary items
 * Rev 6: got rid of MVar parameters
 * Rev 7: worked around having to split sequents into two ("S" and "G") lines by adding support for the \\ separator
 * Rev 8: added context args to SO variables and contexts
 * Rev 9: removed the "parent_opens" field from the parent_info type
 * Rev 10: removed the "ref_parent" fields from the tactic_arg and related types
 * Rev 11: removed the Hypothesis/HypBinding distinction
 * Rev 12: removed the "internal_df" operator from the list of the valid dform options
 * Rev 13: added input/optput types to resource definitions (bug 168)
 * Rev 14: added a Quote parameter
 * Rev 15: switched to single-conclusion sequents
 * Rev 16: address arguments to rules and rewrites
 * Rev 17: tokens now use opnames instead of strings
 * Rev 18: removed "Opname" and "Definition" from the summary_item type
 * Rev 19: iforms no longer have proofs and resources
 * Rev 20: added "shape" parameters
 * Rev 21: added iform/normal classification to opname declarations
 * Rev 22: added "Operator" term parameters.
 * Rec 23: added "Annotate" proof terms.
 * Rev 24: added "const" annotation to 0-arity "declare"/"define"
 * Rev 25: added "private" resource annotations
 * Rev 26: added options to the tactic_arg
 * Rev 27: changed the stging encoding of option_info
 * Rev 28: added "opaque" qualifier to the "define" statements
 *
 * Ascii_io has a HACK needed to read some rev 0-5 files
 * Ascii_io has another set of HACKs to read sequents in rev 0-6 files
 * Ascii_io has yet another set of HACKs to try to guess bound contexts for rev 0-7 files
 * Term_man_minimal_sig has extra stuff for rev 0-7 file support.
 * Filter_summary has a HACK needed to read rev 0-8 files.
 * Proof_term_boot has a HACK needed to read rev 0-9 files.
 * Ascii_io has another HACK needed to read some rev 0-10 files.
 * Filter_summary has another HACK needed to read some rev 0-11 files.
 * Filter_ocaml has a HACK needed to read some rev 0-12 files.
 * Ascii_io has another HACK needed to read some rev 0-14 files.
 * Filter_summary has another HACK needed to read some rev 0-15 files.
 * Filter_summary has another HACK needed to read some rev 0-17 files.
 * Filter_summary has another HACK needed to read some rev 0-18 files.
 * Filter_summary has another HACK needed to read some rev 0-20 files.
 * Filter_summary has another HACK needed to read some rev 21-23 files.
 * Filter_summary has another HACK needed to read some rev 0-24 files.
 * Options_boot has a HACK needed to read some rev 25-26 files.
 * Filter_summary has another HACK needed to read some rev ??-27 files.
 *)
let ascii_versions = List.map (pack_version 1 0) [28;27;26;25;24;23;22;21;20;19;18;17;16;15;14;13]

(************************************************************************
 * Magic numbers for interactive files                                  *
 ************************************************************************)

(* Previously used somewhere: 0x09ac12bd 0x63ac6be3, 0x63ac6be7, 0x63ac6be9, 0x73ac6be2, 0x73ac6be4, 0x73ac6be5, 0x73ac6be7 *)
(* Currently in use in Filter_cache: 0x73ac6be6, 0x73ac6be8, 0x73ac6be1, 0x73ac6be3 *)

let int_term_sig_magic = 0x63ac6be1
let int_term_str_magic = 0x63ac6be3
let int_lib_sig_magic  = 0x63ac6be5
let int_lib_str_magic  = 0x63ac6be6
let int_raw_sig_magic  = 0x63ac6be8
let int_raw_str_magic  = 0x63ac6bea
let interactive_magics =
   [int_term_sig_magic;
    int_raw_sig_magic;
    int_term_str_magic;
    int_raw_str_magic;
    int_lib_sig_magic;
    int_lib_str_magic]

(*
 * See if the file is interactive by checking the magic number.
 *)
let file_interactive file =
   try
      let inx = open_in_bin file in
         try
            let magic = input_binary_int inx in
               close_in inx;
               List.mem magic interactive_magics
         with
            _ ->
               close_in inx;
               false
   with
      Sys_error _ ->
         false

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
