(*
 * This module defines an interface for treating the filesystem
 * as a database of objects stored in files.  The database has a
 * search path, and file types are identified by their suffix, and
 * a magic number stored as the initial binary int in the file.
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

(*
 * Abbreviations.
 *)
type dir_name = string
type file_name = string

(*
 * Files can loaded with an alternate suffix:
 * never, always, or if the suffixed file is newer.
 *)
type alt_suffix =
   AlwaysSuffix of string
 | NewerSuffix of string
 | NeverSuffix

(*
 * File type selection info.
 *)
type ('select, 'cooked) file_info =
   { info_marshal : int list -> int -> string -> 'cooked -> unit;
     info_unmarshal : int list -> string -> 'cooked * int;
     info_disabled : bool ref;
     info_select : 'select;
     info_suffix : string;
     info_magics : int list
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

   val info : (select, cooked) file_info list
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
   type t

   (* Creation *)
   val create : string list -> t
   val set_path : t -> string list -> unit

   (* Loading and saving *)
   val find : t -> file_name -> select -> alt_suffix -> info
   val find_file : t -> file_name -> select -> alt_suffix -> info
   val find_match : t -> info -> select -> alt_suffix -> info
   val save : t -> info -> alt_suffix -> unit
   val magic : t -> info -> int
   val set_magic : t -> info -> int -> unit
   val save_as : t -> cooked -> select -> dir_name -> file_name -> alt_suffix -> info
   val create_info : t -> cooked -> select -> dir_name -> file_name -> info

   (* Info about the objects *)
   val info : t -> info -> cooked
   val set_info : t -> info -> cooked -> unit
   val file_name : t -> info -> file_name
   val full_name : t -> info -> file_name
   val type_of : t -> info -> select
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

   (* File type selection info *)
   val select : select
   val suffix : string
   val magics : int list
   val disabled : bool ref

   (* Conversion functions *)
   val marshal : cooked -> raw
   val unmarshal : raw -> cooked
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

   val write : int list -> int -> string -> t -> unit
   val read : int list -> string -> t * int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
