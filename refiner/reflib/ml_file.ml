(*
 * Module for printing terms to an ML module.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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
 *
 *)

open Printf

open Mp_debug
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Ml_print_sig
open Ml_print

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Ml_file%t"

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
