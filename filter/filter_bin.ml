(*
 * Process a binary file.
 * This file is taken from a previously parsed file,
 * or from the library.
 *)

open Arg
open Printf

open Debug

open Filter_type
open Filter_summary
open Filter_summary_type
open Filter_cache_type
open Filter_cache
open Filter_prog

(*
 * Include directories.
 *)
let include_path = ref ["."]
   
let add_include dir =
   include_path := !include_path @ [dir]

(*
 * Need some info about types and extraction.
 *)
module type CompileInfoSig =
sig
   type proof
   type expr
   type ctyp
   type item

   val extract : (proof, ctyp, expr, item) module_info ->
      (module_path * ctyp resource_info) list ->
      string -> (item * (int * int)) list
   val compile : (item * (int * int)) list -> unit
end

(*
 * Compile from a pre-parsed file.
 *)
module MakeCompile (**)
   (Info : CompileInfoSig)
   (Cache : FilterCacheSig)
   (FilterCache : SummaryCacheSig
    with type sig_ctyp  = Info.ctyp
    with type str_proof = Info.proof
    with type str_expr  = Info.expr
    with type str_ctyp  = Info.ctyp
    with type str_item  = Info.item
    with type select    = Cache.select_type) =
struct
   let compile name =
      let inline_hook cache (path, info) () =
         ()
      in
      let kind, path =
         if Filename.check_suffix name ".cmiz" then
            Cache.InterfaceType, Filename.chop_suffix name ".cmiz"
         else if Filename.check_suffix name ".cmit" then
            Cache.InterfaceType, Filename.chop_suffix name ".cmit"
         else if Filename.check_suffix name ".cmoz" then
            Cache.ImplementationType, Filename.chop_suffix name ".cmoz"
         else if Filename.check_suffix name ".cmot" then
            Cache.ImplementationType, Filename.chop_suffix name ".cmot"
         else
            raise (Failure "Filter_parse.compile: invalid suffix")
      in
      let cache = FilterCache.create !include_path in
      let info, _ = FilterCache.load cache path kind Cache.InterfaceType inline_hook () in
      let items = Info.extract (FilterCache.info info) (FilterCache.resources info) path in
         Info.compile items
end

(*
 * The two caches.
 *)
module Proof =
struct
   type proof = Proof_type.proof
end

module Cache = MakeFilterCache (Proof)
module Extract = MakeFilterProg (Cache)

module SigCompileInfo =
struct
   type proof = unit
   type expr  = MLast.expr
   type ctyp  = MLast.ctyp
   type item  = MLast.sig_item

   let extract = Extract.extract_sig
   let compile items =
      (!Pcaml.print_interf) items
end

module StrCompileInfo =
struct
   type proof = Cache.proof_type
   type expr  = MLast.expr
   type ctyp  = MLast.ctyp
   type item  = MLast.str_item

   let extract = Extract.extract_str
   let compile items =
      (!Pcaml.print_implem) items;
      eprintf "Writing output file%t" eflush;
      flush stdout
end

module SigCompile = MakeCompile (SigCompileInfo) (Cache) (Cache.SigFilterCache)
module StrCompile = MakeCompile (StrCompileInfo) (Cache) (Cache.StrFilterCache)
                    
(*
 * Compile a file.
 *)
let process_file file =
   if Filename.check_suffix file ".cmiz" or
      Filename.check_suffix file ".cmit"
   then
      (* An interface file *)
      SigCompile.compile file
   else if Filename.check_suffix file ".cmoz" or
           Filename.check_suffix file ".cmot"
   then
      (* An implementation file *)
      StrCompile.compile file
   else
      raise (Bad "Filter_bin.main: file has a bogus suffix")

(*
 * Argument specification.
 *)
let spec =
   ["-I", String add_include, "add an directory to the path for include files";
    "-intf", String process_file, "compile an interface file";
    "-impl", String process_file, "compile an implementation file"]

(*
 * Everything standard is taken care of.
 * There should be a single file argument, so
 * process it.
 *)
let main () =
   Pcaml.input_file := "-";
   Arg.parse spec process_file "Compile a Nuprl-Light binary file"

let _ = Printexc.catch main ()

(*
 * $Log$
 * Revision 1.3  1998/04/13 17:08:26  jyh
 * Adding interactive proofs.
 *
 * Revision 1.2  1998/04/09 18:25:47  jyh
 * Working compiler once again.
 *
 * Revision 1.1  1998/03/03 04:05:59  jyh
 * Added filter bin.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
