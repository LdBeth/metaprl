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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
