(*
 * This is a typed interface to the FileBase,
 * where the contents of the files are just typed,
 * marshaled objects.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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
 *)

open Printf
open Nl_debug
open Nl_pervasives
open File_base_type

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading File_type_base%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Type used for common info.
 *)
type ('select, 'cooked) common_info = ('select, 'cooked) file_info list

(************************************************************************
 * MODULE IMPLEMENTATIONS                                               *
 ************************************************************************)

(*
 * The Combo contans all the data to construct a FileBaseInfoSig.
 *)
module MakeIOSingletonCombo (IO : IOSig) (Info : FileTypeInfoSig
                                          with type raw = IO.t) :
   (FileTypeComboSig
    with type cooked = Info.cooked
    with type select = Info.select
    with type info = (Info.select, Info.cooked) common_info) =
struct
   type select = Info.select
   type cooked = Info.cooked
   type info = (select, cooked) common_info

   let marshal magics magic filename info =
      IO.write magics magic filename (Info.marshal info)

   let unmarshal magics filename =
      let t, magic = IO.read magics filename in
         Info.unmarshal t, magic

   let info =
      [{ info_marshal = marshal;
         info_unmarshal = unmarshal;
         info_disabled = Info.disabled;
         info_suffix = Info.suffix;
         info_magics = Info.magics;
         info_select = Info.select
       }]
end

(*
 * The Combo contans all the data to construct a FileBaseInfoSig.
 * We won't overwrite a file if it has the wrong magic number.
 *)
module MakeSingletonCombo (Info : FileTypeInfoSig) :
   (FileTypeComboSig
    with type cooked = Info.cooked
    with type select = Info.select
    with type info = (Info.select, Info.cooked) common_info) =
   MakeIOSingletonCombo (**)
      (struct
         type t = Info.raw

         let check magics magic filename =
            try
               let inx = open_in_bin filename in
                  try
                     let magic' = input_binary_int inx in
                        if List_util.find_index magic' magics > magic then
                           raise (Failure (sprintf "File %s has been modified, write operation failed" filename))
                  with
                     End_of_file ->
                        close_in inx
                   | exn ->
                        close_in inx;
                        raise exn
            with
               Sys_error _ ->
                  ()

         let write magics magic filename info =
            let _ = check magics magic filename in
            let outx = open_out_bin filename in
               try
                  output_binary_int outx (List.nth magics magic);
                  output_value outx (info : t);
                  close_out outx
               with
                  exn ->
                     close_out outx;
                     raise exn

         let read magics filename =
            let inx = open_in_bin filename in
               try
                  let magic = input_binary_int inx in
                     (input_value inx : t), List_util.find_index magic magics
               with
                  exn ->
                     close_in inx;
                     raise (Sys_error "load_file")
      end)
      (Info)

(*
 * Extend a Combo with new data.
 *)
module CombineCombo (**)
   (Types : FileTypeSummarySig)
   (Info : FileTypeComboSig
    with type cooked = Types.cooked
    with type select = Types.select
    with type info = (Types.select, Types.cooked) common_info)
   (Combo : FileTypeComboSig
    with type cooked = Types.cooked
    with type select = Types.select
    with type info = (Types.select, Types.cooked) common_info) :
   (FileTypeComboSig
    with type cooked = Types.cooked
    with type select = Types.select
    with type info = (Types.select, Types.cooked) common_info) =
struct
   type select = Types.select
   type cooked = Types.cooked
   type info = (select, cooked) common_info

   let info = Info.info @ Combo.info
end

(*
 * Create an info from a combo.
 *)
module MakeFileBaseInfo (Types : FileTypeSummarySig)
    (Combo : FileTypeComboSig
     with type select = Types.select
     with type cooked = Types.cooked
     with type info = (Types.select, Types.cooked) common_info) :
       (FileBaseInfoSig
        with type select = Types.select
        with type cooked = Types.cooked) =
struct
   type select = Types.select
   type cooked = Types.cooked
   let info = Combo.info
end

(*
 * Create a summary base for a specific format.
 *)
module MakeFileBase (Types : FileTypeSummarySig)
(Combo : FileTypeComboSig
         with type select = Types.select
         with type cooked = Types.cooked
         with type info = (Types.select, Types.cooked) common_info) :
   (FileBaseSig
    with type select = Types.select
    with type cooked = Types.cooked) =
   File_base.MakeFileBase (MakeFileBaseInfo (Types) (Combo))

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
