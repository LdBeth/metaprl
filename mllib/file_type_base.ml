(*
 * This is a typed interface to the FileBase,
 * where the contents of the files are just typed,
 * marshaled objects.
 *)

open Printf
open Debug
open File_base_type

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading File_type_base%t" eflush

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
module MakeIOSingletonCombo (IO : IOSig) (Info : FileTypeInfoSig
                                          with type raw = IO.t) :
   (FileTypeComboSig
    with type cooked = Info.cooked
    with type select = Info.select
    with type info = (Info.select, Info.cooked) common_info) =
struct
   type select = Info.select
   type cooked = Info.cooked
   type info = (select, cooked) common_info

   let marshal magic filename info =
      IO.write magic filename (Info.marshal info)

   let unmarshal magic filename =
      Info.unmarshal (IO.read magic filename)

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
 * The Combo contans all the data to construct a FileBaseInfoSig.
 *)
module MakeSingletonCombo (Info : FileTypeInfoSig) :
   (FileTypeComboSig
    with type cooked = Info.cooked
    with type select = Info.select
    with type info = (Info.select, Info.cooked) common_info) =
   MakeIOSingletonCombo (**)
      (struct
         type t = Info.raw

         let write magic filename info =
            let outx = open_out_bin filename in
               try
                  output_binary_int outx magic;
                  output_value outx (info : t);
                  close_out outx
               with
                  exn ->
                     close_out outx;
                     raise exn

         let read magic filename =
            let inx = open_in_bin filename in
               try
                  let magic' = input_binary_int inx in
                     if magic = magic' then
                        (input_value inx : t)
                     else
                        begin
                           close_in inx;
                           raise (Sys_error "load_file")
                        end
               with
                  exn ->
                     close_in inx;
                     raise (Sys_error "load_file")
      end)
      (Info)

(*
 * Extend a Combo with new data.
 *)
module CombineCombo (Types : FileTypeSummarySig)
(Info : FileTypeComboSig
        with type cooked = Types.cooked
        with type select = Types.select
        with type info = (Types.select, Types.cooked) common_info)
(Combo : FileTypeComboSig
         with type cooked = Types.cooked
         with type select = Types.select
         with type info = (Types.select, Types.cooked) common_info) :
   (FileTypeComboSig
    with type cooked = Types.cooked
    with type select = Types.select
    with type info = (Types.select, Types.cooked) common_info) =
struct
   type select = Types.select
   type cooked = Types.cooked
   type info = (select, cooked) common_info

   let info = Info.info @ Combo.info
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
 * Revision 1.7  1998/06/01 13:54:37  jyh
 * Proving twice one is two.
 *
 * Revision 1.6  1998/04/24 19:38:53  jyh
 * Updated debugging.
 *
 * Revision 1.5  1998/02/23 14:46:36  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.4  1998/02/19 17:13:21  jyh
 * Splitting filter_parse.
 *
 * Revision 1.3  1998/02/18 18:46:49  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.2  1998/02/12 23:35:18  jyh
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
