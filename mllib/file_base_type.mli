(*
 * This module defines an interface for treating the filesystem
 * as a database of objects stored in files.  The database has a
 * search path, and file types are identified by their suffix, and
 * a magic number stored as the initial binary int in the file.
 *)

(*
 * Abbreviations.
 *)
type dir_name = string
type file_name = string

(*
 * File type selection info.
 *)
type ('select, 'cooked) file_info =
   { info_marshal : int -> string -> 'cooked -> unit;
     info_unmarshal : int -> string -> 'cooked;
     info_select : 'select;
     info_suffix : string;
     info_magic : int
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
   val find : t -> file_name -> select -> info
   val find_match : t -> info -> select -> info
   val save : t -> info -> unit
   val save_as : t -> cooked -> select -> dir_name -> file_name -> info
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
   val magic : int
   
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
 * $Log$
 * Revision 1.2  1998/02/12 23:35:17  jyh
 * Generalized file base to allow the library.
 *
 * Revision 1.1  1997/08/06 16:17:56  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
