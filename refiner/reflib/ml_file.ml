(*
 * Module for printing terms to an ML module.
 *
 *)

open Printf

open Debug
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
 * $Log$
 * Revision 1.2  1998/06/01 13:54:54  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:00:49  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.3  1998/05/27 15:13:45  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.2  1998/04/24 02:42:38  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/04/28 15:51:23  jyh
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
 * Revision 1.8  1996/05/21 02:13:56  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.7  1996/04/07 18:24:48  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.6  1996/03/25 20:50:41  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.5  1996/03/05 19:48:30  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.4  1996/02/25 15:16:15  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.3  1996/02/18 23:32:28  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.2  1996/02/13 21:32:22  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.1  1996/02/10 20:19:54  jyh
 * Initial checkin of filter (prlcomp).
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
