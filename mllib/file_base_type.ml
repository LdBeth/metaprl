(*
 * This module defines an interface for treating the filesystem
 * as a database of objects stored in files.  The database has a
 * search path, and file types are identified by their suffix, and
 * a magic number stored as the initial binary int in the file.
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

(*
 * Abbreviations.
 *)
type dir_name = string
type file_name = string

(*
 * Files can loaded with an alternate suffix.
 *)
type alt_suffix =
   AnySuffix
 | OnlySuffixes of string list

(*
 * File type selection info.
 *)
type ('arg, 'select, 'cooked) file_info =
   { info_marshal   : int list -> int -> int list -> 'arg -> string -> 'cooked -> unit;
     info_unmarshal : int list -> int list -> 'arg -> string -> 'cooked * int;
     info_disabled  : bool ref;
     info_select    : 'select;
     info_suffix    : string;
     info_magics    : int list;
     info_versions  : int list;
   }

(*
 * This is the info that is needed to read a file.
 * Modules are classified according to their suffix and magic number.
 * Each file type provides a function to read from the
 * file into a cooked object.
 *)
module type FileBaseInfoSig =
sig
   type select
   type cooked
   type arg

   val info : (arg, select, cooked) file_info list
end

(*
 * This is a module base that loads modules automatically
 * given a directory path to look for module files.
 * The proof type must be specified (so we know what kind
 * of proofs are in the file).
 *)
module type FileBaseSig =
sig
   (*
    * select: a type of tags for typecase on the info
    * info: an abstract type for representing modules in this base
    * t: the database type
    *)
   type cooked
   type select
   type info
   type arg
   type t

   (* Creation *)
   val create   : string list -> t
   val clear    : t -> unit
   val set_path : t -> string list -> unit

   (* Loading and saving *)
   val find            : t -> arg -> file_name -> select -> alt_suffix -> info
   val find_file       : t -> arg -> file_name -> select -> alt_suffix -> info
   val find_match      : t -> arg -> info -> select -> alt_suffix -> info
   val save            : t -> arg -> info -> alt_suffix -> unit
   val save_if_newer   : t -> arg -> info -> alt_suffix -> unit
   val save_if_missing : t -> arg -> info -> alt_suffix -> unit
   val magic           : t -> info -> int
   val set_magic       : t -> info -> int -> unit
   val save_as         : t -> arg -> cooked -> select -> dir_name -> file_name -> alt_suffix -> info
   val create_info     : t -> cooked -> select -> dir_name -> file_name -> info
   val remove_info     : t -> info -> unit

   (* Info about the objects *)
   val info            : t -> info -> cooked
   val set_info        : t -> info -> cooked -> unit
   val file_name       : t -> info -> file_name
   val full_name       : t -> info -> file_name
   val type_of         : t -> info -> select
end

(************************************************************************
 * TYPED INTERFACE                                                      *
 ************************************************************************)

(*
 * This is a module that contains just the types,
 * so we can specify sharing constraints.
 *)
module type FileTypeSummarySig =
sig
   type select
   type cooked
   type arg
end

(*
 * This module is a typed representation of an entry
 * in a FileBaseInfoSig.
 *)
module type FileTypeInfoSig =
sig
   type select
   type raw
   type cooked
   type arg

   (* File type selection info *)
   val select    : select
   val suffix    : string
   val magics    : int list
   val versions  : int list
   val disabled  : bool ref

   (* Conversion functions *)
   val marshal   : arg -> cooked -> raw
   val unmarshal : arg-> raw -> cooked
end

(*
 * Info structures are combined into objects of this type.
 *)
module type FileTypeComboSig =
sig
   (*
    * Type of common proofs.
    *)
   type select
   type cooked
   type arg
   type info

   (*
    * Marshallers.
    *)
   val info : info
end

(*
 * Allow arbitrary IO.
 *)
module type IOSig =
sig
   type t

   val write : int list -> int -> int list -> string -> t -> unit
   val read  : int list -> int list -> string -> t * int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
