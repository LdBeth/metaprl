(*
 * This is a typed interface to the FileBase,
 * where the contents of the files are just typed,
 * marshaled objects.
 *)

open File_base_type
         
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
module MakeSingletonCombo (Info : FileTypeInfoSig) :
   (FileTypeComboSig
    with type cooked = Info.cooked
    with type select = Info.select
    with type info = (Info.select, Info.cooked) common_info) =
struct
   type select = Info.select
   type cooked = Info.cooked
   type info = (select, cooked) common_info

   let marshal out info =
      output_value out (Info.marshal info)
   
   let unmarshal inx =
      Info.unmarshal (input_value inx : Info.raw)
   
   let info =
      [{ info_marshal = marshal;
         info_unmarshal = unmarshal;
         info_suffix = Info.suffix;
         info_magic = Info.magic;
         info_select = Info.select
       }]
end

(*
 * Extend a Combo with new data.
 *)
module ExtendCombo (Info : FileTypeInfoSig)
(Combo : FileTypeComboSig
         with type cooked = Info.cooked
         with type select = Info.select
         with type info = (Info.select, Info.cooked) common_info) :
   (FileTypeComboSig
    with type cooked = Info.cooked
    with type select = Info.select
    with type info = (Info.select, Info.cooked) common_info) =
struct
   type select = Info.select
   type cooked = Info.cooked
   type info = (select, cooked) common_info
      
   module SingletonCombo = MakeSingletonCombo (Info)
                              
   let info = SingletonCombo.info @ Combo.info
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
 * $Log$
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
