(*
 * This module defines an interface for treating the filesystem
 * as a database of objects stored in files.  The database has a
 * search path, and file types are identified by their suffix, and
 * a magic number stored as the initial binary int in the file.
 *)

open File_base_type

(*
 * Build a base from the info.
 *)
module MakeFileBase (Info : FileBaseInfoSig) :
   FileBaseSig
   with type select = Info.select
   with type cooked = Info.cooked

(*
 * $Log$
 * Revision 1.1  1997/08/06 16:17:55  jyh
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
