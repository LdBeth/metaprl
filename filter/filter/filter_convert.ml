(*
 * Process a binary file.
 * This file is taken from a previously parsed file,
 * or from the library.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Lm_debug

open File_base_type

open Filter_summary_type
open Filter_cache

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Convert%t"

(*
 * Include directories.
 *)
let include_path = ref ["."]

let add_include dir =
   include_path := !include_path @ [dir]

(*
 * Output suffix.
 *)
let output_suffix = ref (OnlySuffixes ["prla"])

let set_raw () =
   output_suffix := OnlySuffixes ["prlb"];
   set_raw ()

(* unused
let set_file () =
   output_suffix := OnlySuffixes ["prlb"];
   set_file ()

let set_lib () =
   output_suffix := OnlySuffixes ["prlb"];
   set_lib ()
*)

let set_ascii () =
   output_suffix := OnlySuffixes ["prla"]

(*
 * Suffix lists.
 *)
let interface_suffixes = ["cmiz"; "cmit"; "prlbi"; "prlai"]
let implementation_suffixes = ["cmoz"; "cmot"; "prlb"; "prla"]

let rec remove_suffix name = function
   suffix :: tl ->
      let suffix' = "." ^ suffix in
         if Filename.check_suffix name suffix' then
            Some (Filename.chop_suffix name suffix', suffix)
         else
            remove_suffix name tl
 | [] ->
      None

(*
 * Compile from a pre-parsed file.
 *)
module MakeConvert (FilterCache : SummaryCacheSig
                                  with type arg = unit) =
struct
   let convert kind name input_suffix =
      let cache = FilterCache.create !include_path in
      let info = FilterCache.load cache () name kind input_suffix in
         FilterCache.save info () !output_suffix
end

(*
 * The two caches. Interactive proofs are handled as raw objects.
 *)
module ProofCaches = MakeCaches (Proof_convert.Convert)
module SigConvert = MakeConvert (ProofCaches.SigFilterCache)
module StrConvert = MakeConvert (ProofCaches.StrFilterCache)

(*
 * Convert a file.
 *)
let process_file file =
   match remove_suffix file interface_suffixes with
      Some (name, suffix) ->
         SigConvert.convert InterfaceType name AnySuffix
    | None ->
         match remove_suffix file implementation_suffixes with
            Some (name, suffix) ->
               StrConvert.convert ImplementationType name AnySuffix
          | None ->
               raise (Invalid_argument "convert: file has a bogus suffix")

(*
 * Argument specification.
 *)
let spec =
   ["-I", Arg.String add_include, "add an directory to the path for include files";
    "-raw", Arg.Unit set_raw, "produce a file in raw format";
    "-term", Arg.Unit set_raw, "produce a file in term format";
    "-lib", Arg.Unit set_raw, "send the file to Nuprl FDL";
    "-ascii", Arg.Unit set_ascii, "produce a file in ASCII format";
    "-intf", Arg.String process_file, "compile an interface file";
    "-impl", Arg.String process_file, "compile an implementation file"]

(*
 * Everything standard is taken care of.
 * There should be a single file argument, so
 * process it.
 *)
let _ =
   Pcaml.input_file := "";
   Arg.parse spec process_file "Convert a MetaPRL binary file"

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
