(*
 * This is a typed interface to the FileBase,
 * where the contents of the files are just typed,
 * marshaled objects.
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
