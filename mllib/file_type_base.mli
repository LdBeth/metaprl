(*
 * This is a typed interface to the FileBase,
 * where the contents of the files are just typed,
 * marshaled objects.
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
 *)

open File_base_type

(*
 * Type used for common info.
 *)
type ('select, 'cooked) common_info

(*
 * Combo construction.
 *)
module MakeIOSingletonCombo (IO : IOSig) (Info : FileTypeInfoSig
                                          with type raw = IO.t) :
   FileTypeComboSig
   with type cooked = Info.cooked
   with type select = Info.select
   with type info = (Info.select, Info.cooked) common_info

module MakeSingletonCombo (Info : FileTypeInfoSig) :
   FileTypeComboSig
   with type cooked = Info.cooked
   with type select = Info.select
   with type info = (Info.select, Info.cooked) common_info

module CombineCombo (Types : FileTypeSummarySig)
   (Info : FileTypeComboSig
            with type cooked = Types.cooked
            with type select = Types.select
            with type info = (Types.select, Types.cooked) common_info)
   (Combo : FileTypeComboSig
            with type cooked = Types.cooked
            with type select = Types.select
            with type info = (Types.select, Types.cooked) common_info) :
   FileTypeComboSig
   with type cooked = Types.cooked
   with type select = Types.select
   with type info = (Types.select, Types.cooked) common_info

(*
 * Create a summary base for a specific format.
 *)
module MakeFileBase (Types : FileTypeSummarySig)
   (Combo : FileTypeComboSig
            with type select = Types.select
            with type cooked = Types.cooked
            with type info = (Types.select, Types.cooked) common_info) :
   FileBaseSig
   with type select = Types.select
   with type cooked = Types.cooked

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
