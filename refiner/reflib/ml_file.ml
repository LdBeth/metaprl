(*
 * Module for printing terms to an ML module.
 *
 *)

open Printf

open Nl_debug
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Ml_print_sig
open Ml_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Ml_file%t" eflush

(************************************************************************
 * FILE                                                                 *
 ************************************************************************)

(*
 * Header string printed to the files.
 *)
let header =
"(*
 * Term definitions.
 *)

open Term

"

(*
 * Named file.
 *)
module MLFile =
struct
   type t =
      { (* Interface definition *)
         mli_file : out_channel;

         (* Definition *)
         ml_file : out_channel
      }

   type name = string
   type out = unit

   (* Creation *)
   let create name =
      let ml_name = name ^ ".ml" in
      let mli_name = name ^ ".mli" in
      let file = open_out ml_name in
      let ifile =
         try open_out mli_name with
            Sys_error x ->
               close_out file;
               raise (Sys_error x)
      in
         (* Headers *)
         output_string ifile header;
         output_string file header;

         (* Return pointers *)
         { mli_file = ifile; ml_file = file }

   (* Closing *)
   let close { ml_file = file; mli_file = ifile } =
      close_out file;
      close_out ifile

   (* Printing *)
   let put { ml_file = file } s = output_string file s
   let puti { mli_file = ifile } s = output_string ifile s
   let get _ = ()
end

(*
 * Channel file.
 *)
module IOFile =
struct
   type t = out_channel
   type name = string
   type out = unit

   let create name = open_out name
   let close file = close_out file
   let puti _ _ = ()
   let put = output_string
   let get _ = ()
end

(*
 * Printers.
 *)
module MLPrint = MakePrinter (MLFile)
module IOPrint = MakePrinter (IOFile)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
