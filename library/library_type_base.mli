(*
 * Make a combo to read from the library.
 *)

open Term

open File_base_type
open File_type_base

module MakeSingletonCombo (Info : FileTypeInfoSig
                                  with type raw = term list) :
   (FileTypeComboSig
    with type cooked = Info.cooked
    with type select = Info.select
    with type info = (Info.select, Info.cooked) common_info)

(*
 * $Log$
 * Revision 1.1  1998/02/12 23:35:45  jyh
 * Added base Nuprl-Light interface to the library.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
