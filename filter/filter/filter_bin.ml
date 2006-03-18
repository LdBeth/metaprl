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
 * Copyright (C) 1998-2006 Mojave Group, Caltech, Cornell University.
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
 * jyh@cs.caltech.edu
 *)
open Arg
open Lm_debug

open Lm_printf

open File_base_type

open Refiner.Refiner.TermType

open Filter_type
open Filter_summary
open Filter_summary_type
open Filter_prog
open Filter_magic
open Proof_convert

open Filter_prog.ProofCaches

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_bin%t"

(*
 * string -> path commands
 *)
let set_path doc var path =
   let path' = Lm_string_util.split ":" path in
      var := path'

let set_path_arg doc var =
   Arg.String (set_path doc var)

(*
 * This is a list of hosts to use for database lookup.
 *)
let include_path = Env_arg.general "include" ["."] "Include directories" set_path set_path_arg

let add_include path =
   include_path := !include_path @ [path]

(*
 * Reflection prefix, in case we are generating reflected versions
 * of the file.
 *)
let reflect_flag = ref false
let reflect_name = ref None

(*
 * Theory description.
 *)
let theory_group, theory_groupdsc = Filter_util.make_groupdsc_opts ()

(*
 * Compile a signature to ML.
 *)
let compile_sig name =
   let path = Lm_filename_util.root name in
   let cache = SigFilterCache.create !include_path in
   let info = SigFilterCache.load cache () path InterfaceType AnySuffix in

   (* Get the ML items *)
   let items =
      Filter_prog.extract_sig (**)
         ()
         (SigFilterCache.info info)
         (SigFilterCache.resources info)
         (Filename.basename path)
         (theory_group ())
         (theory_groupdsc ())
   in
      (* Print them *)
      (!Pcaml.print_interf) items

(*
 * Compile an implementation to ML.
 *)
let compile_str name =
   let path = Lm_filename_util.root name in
   let cache = StrFilterCache.create !include_path in
   let info = StrFilterCache.load cache () path ImplementationType AnySuffix in

   (* Get the ML items *)
   let check = StrFilterCache.check info () InterfaceType in
   let items =
      Filter_prog.extract_str (**)
         ()
         check
         (StrFilterCache.info info)
         (StrFilterCache.resources info)
         (Filename.basename path)
         (theory_group ())
         (theory_groupdsc ())
   in
      (* Print them *)
      (!Pcaml.print_implem) items

(*
 * Generate a reflected signature.
 *)
let compile_reflect_sig name =
   let cache = SigFilterCache.create !include_path in
   let orig_path = Lm_filename_util.root name in
   let orig_base = Lm_filename_util.basename orig_path in
   let orig_info = SigFilterCache.load cache () orig_path InterfaceType AnySuffix in

   (* Create the new cache *)
   let new_base, new_path = Filter_reflect.reflect_filename !reflect_name orig_path in
   let new_info = SigFilterCache.create_cache cache new_path InterfaceType in
   let () = SigFilterCache.reset_hack new_info in

   (* Process the file *)
   let () =
      Filter_reflect.compile_sig new_info orig_base (SigFilterCache.info orig_info);
      SigFilterCache.save new_info () AnySuffix
   in

   (* Get the ML items *)
   let items =
      Filter_prog.extract_sig (**)
         ()
         (SigFilterCache.info new_info)
         (SigFilterCache.resources new_info)
         new_base
         (theory_group ())
         (theory_groupdsc ())
   in
      (* Print the ML part *)
      (!Pcaml.print_interf) items

(*
 * Generate a reflected implementation.
 *)
let copy_proof proof1 proof2 =
      match proof1, proof2 with
         (Incomplete | Interactive _), Interactive _ ->
            proof2
       | _ ->
            proof1

let compile_reflect_str name =
   let cache = StrFilterCache.create !include_path in
   let orig_path = Lm_filename_util.root name in
   let orig_base = Lm_filename_util.basename orig_path in
   let orig_info = StrFilterCache.load cache () orig_path ImplementationType AnySuffix in

   (* Create the new cache *)
   let new_base, new_path = Filter_reflect.reflect_filename !reflect_name orig_path in
   let new_info = StrFilterCache.create_cache cache new_path ImplementationType in
   let () = StrFilterCache.reset_hack new_info in

   (* Process the file *)
   let () =
      Filter_reflect.compile_str new_info orig_base (StrFilterCache.info orig_info)
   in

   (*
    * Check that implementation matches interface.
    * This will also copy part of the interface into the implementation.
    *)
   let sig_info = StrFilterCache.check new_info () InterfaceType in

   (*
    * Proof copying.
    *)
   let () =
      (* Also copy old proofs if they exist *)
      if file_interactive (new_path ^ ".prlb") || Sys.file_exists (new_path ^ ".prla") then begin
         StrFilterCache.set_mode new_info InteractiveSummary;
         StrFilterCache.copy_proofs new_info () copy_proof
      end;
      StrFilterCache.set_mode new_info InteractiveSummary;
      StrFilterCache.save new_info () (OnlySuffixes ["cmoz"])
   in

   (* Get the ML items *)
   let items =
      Filter_prog.extract_str (**)
         ()
         sig_info
         (StrFilterCache.info new_info)
         (StrFilterCache.resources new_info)
         new_base
         (theory_group ())
         (theory_groupdsc ())
   in
      (* Print them *)
      (!Pcaml.print_implem) items

(*
 * Compile a file.
 *)
let process_file file =
   let compile =
      if Filename.check_suffix file ".cmiz" || Filename.check_suffix file ".cmit" then
         if !reflect_flag then
            compile_reflect_sig
         else
            compile_sig
      else if Filename.check_suffix file ".cmoz" || Filename.check_suffix file ".cmot" then
         if !reflect_flag then
            compile_reflect_str
         else
            compile_str
      else
         raise (Bad "Filter_bin.main: file has a bogus suffix")
   in
      compile file

(*
 * Argument specification.
 *)
let spec =
   ["-I",        Arg.String add_include,                            "add an directory to the path for include files";
    "-r",        Arg.Set reflect_flag,                              "generate a reflected file";
    "--reflect", Arg.Set reflect_flag,                              "generate a reflected file";
    "--name",    Arg.String (fun s -> reflect_name := Some s),      "specify the name of the reflected file";
    "-o",        Arg.String (fun s -> Pcaml.output_file := Some s), "specify the output file (defaults to stdout)"]

(*
 * Everything standard is taken care of.
 * There should be a single file argument, so
 * process it.
 *)
let main () =
   Pcaml.input_file := "";
   Arg.parse spec process_file "Compile a MetaPRL binary file"

let () = Filter_exn.print_exn Dform.null_base None main ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
