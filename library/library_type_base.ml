(*
 * This implements a filesystem interface to the library.
 *)

open File_base_type
open File_type_base

open Term

(*
 * Save a file to th library.
 * "Magic" is a magic number that is sued to identify the
 * version of the file.
 *)
let library_set magic filename =
   raise (Failure "storing a file to the library is not implemented")

(*
 * Get a file from the library.
 *)
let library_get magic filename =
   raise (Failure "retriving a file from the library is not implemented")

(*
 * This "combo" is the module that defines how to fetch
 * an object from the library.  We are passed an argument
 * that describes how to marshal and unmarshal objects to terms.
 *)
module MakeSingletonCombo (Info : FileTypeInfoSig
                                  with type raw = term list) :
   (FileTypeComboSig
    with type cooked = Info.cooked
    with type select = Info.select
    with type info = (Info.select, Info.cooked) common_info) =
struct
   type select = Info.select
   type cooked = Info.cooked
   type info = (select, cooked) common_info
   
   (*
    * If there is an error, raise
    * Sys_error "description"
    *    Like raise (Sys_error "file does not exist")
    *)
   let marshal magic filename info =
      library_set magic filename (Info.marshal info)

   let unmarshal magic filename =
      Info.unmarshal (library_get magic filename)
   
   let info =
      [{ info_marshal = marshal;
         info_unmarshal = unmarshal;
         info_disabled = Info.disabled;
         info_suffix = Info.suffix;
         info_magic = Info.magic;
         info_select = Info.select
       }]
end

(*
 * $Log$
 * Revision 1.2  1998/02/18 18:46:37  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.1  1998/02/12 23:35:44  jyh
 * Added base Nuprl-Light interface to the library.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
