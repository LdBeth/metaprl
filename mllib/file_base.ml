(*
 * This module defines an interface for treating the filesystem
 * as a database of objects stored in files.  The database has a
 * search path, and file types are identified by their suffix, and
 * a magic number stored as the initial binary int in the file.
 *)

open Printf

open Debug
open File_base_type

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading File_base%t" eflush

(*
 * Make the summary from the info in the Combo.
 *)
module MakeFileBase (Info : FileBaseInfoSig) :
   (FileBaseSig
    with type select = Info.select
    with type cooked = Info.cooked) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Save the common types.
    *)
   type cooked = Info.cooked
   type select = Info.select

   (*
    * This is the info we keep about modules.
    *)
   type info =
      { mutable info_info : cooked;
        info_type : select;
        info_dir : string;
        info_file : string
      }

   (*
    * The base is a hashtable mapping the root
    * module names to the module info.
    *)
   type t =
      { io_table : (file_name, info list ref) Hashtbl.t;
        mutable io_path : string list
      }

   (************************************************************************
    * BASE OPERATIONS                                                      *
    ************************************************************************)

   (*
    * Create a new base from the path.
    *)
   let create path =
      { io_table = Hashtbl.create 97;
        io_path = path
      }

   let set_path base path =
      base.io_path <- path

   (************************************************************************
    * LOAD/STORE                                                           *
    ************************************************************************)

   (*
    * Save an info in the hashtable.
    *)
   let add_info base info =
      let { io_table = table } = base in
      let { info_file = name } = info in
         try
            let bucket = Hashtbl.find table name in
               bucket := info :: !bucket
         with
            Not_found ->
               Hashtbl.add table name (ref [info])

   (*
    * Load a file given the directory, the filename, and the spec.
    *)
   let load_file base spec dir name =
      let { info_unmarshal = unmarshal;
            info_suffix = suffix;
            info_magic = magic;
            info_select = select
          } = spec
      in
      let filename = sprintf "%s/%s.%s" dir name suffix in
      let info = unmarshal magic filename in
      let _ =
         if !debug_file_base then
            eprintf "File_base.load_file: loaded file %s%t" filename eflush
      in
      let info' =
         { info_info = info;
           info_file = name;
           info_type = select;
           info_dir = dir
         }
      in
         add_info base info';
         info'

   (*
    * Find an existing root module.
    * If it doesn't exist in the base, search for it
    * in the filesystem and load it.  The type preference is
    * given by the ordering of Combo info items.
    *)
   let load_specific base spec name =
      if !debug_file_base then
         eprintf "File_base.load_specific: %s: begin%t" name eflush;
      let rec search = function
         [] ->
            if !debug_file_base then
               eprintf "File_base.load_specific: %s: not found%t" name eflush;
            raise Not_found
       | dir::path' ->
            if !debug_file_base then
               eprintf "File_base.load_specific: try %s/%s%t" dir name eflush;
            try load_file base spec dir name with
               Sys_error _ ->
                  search path'
      in
         search base.io_path

   (*
    * Find the specification corresponding to the select.
    *)
   let find_spec select =
      let rec search = function
         io::tl ->
            let { info_select = select'; info_disabled = disabled } = io in
               if select' = select & not !disabled then
                  io
               else
                  search tl
       | [] ->
            raise (Invalid_argument "File_base.find_spec")
      in
         search Info.info

   (*
    * Find a root module.
    * Check if it exists, otherwise load it.
    *)
   let find base name select =
      let { io_table = table } = base in
         if !debug_file_base then
            eprintf "File_base.find: %s%t" name eflush;
         try
            let rec search = function
               info::tl ->
                  let { info_type = select'; info_file = file; info_dir = dir } = info in
                     if !debug_file_base then
                        eprintf "File_base.find: checking %s/%s%t" dir file eflush;
                     if select' = select then
                        info
                     else
                        search tl
             | [] ->
                  raise Not_found
            in
            let info = search !(Hashtbl.find table name) in
               if !debug_file_base then
                  eprintf "File_base.find: %s: found%t" name eflush;
               info
         with
            Not_found ->
               if !debug_file_base then
                  eprintf "File_base.find: %s: loading%t" name eflush;
               load_specific base (find_spec select) name

   (*
    * Find a "matching" module.
    * This means the root with the same name, but different suffix.
    *)
   let find_match base info select =
      let { io_table = table } = base in
      let { info_dir = dir; info_file = file } = info in
      let rec search = function
         info'::tl ->
            let { info_dir = dir'; info_file = file'; info_type = select' } = info' in
               if dir' = dir & file' = file & select' = select then
                  info'
               else
                  search tl
       | [] ->
            raise Not_found
      in
         try search !(Hashtbl.find table file) with
            Not_found ->
               load_file base (find_spec select) dir file

   (*
    * Save a module specification.
    * Try saving in all the valid formats until one of them succeeds.
    *)
   let save base info =
      let { info_dir = dir; info_file = file; info_type = select; info_info = data } = info in
      let { info_magic = magic; info_marshal = marshal; info_suffix = suffix } = find_spec select in
      let filename = sprintf "%s/%s.%s" dir file suffix in
         marshal magic filename data

   (*
    * Inject a new module.
    * First, check that the module does not already exist.
    *)
   let create_info base data select dir file =
      let _ =
         if !debug_file_base then
            eprintf "File_base.create_info: %s/%s%t" dir file eflush
      in
      let { io_table = table } = base in
      let rec search = function
         { info_dir = dir'; info_file = file'; info_type = select' }::tl ->
            if dir' = dir & file' = file & select' = select then
               raise (Invalid_argument "File_base.save_as")
            else
               search tl
       | [] ->
            ()
      in
      let info =
         { info_info = data;
           info_type = select;
           info_dir = dir;
           info_file = file
         }
      in
      let _ =
         try
            let bucket = Hashtbl.find table file in
               search !bucket;
               bucket := info :: !bucket
         with
            Not_found ->
               Hashtbl.add table file (ref [info])
      in
         info

   let save_as base data select dir file =
      let info = create_info base data select dir file in
         save base info;
         info

   (************************************************************************
    * MODULE INFO                                                          *
    ************************************************************************)

   (*
    * Projections.
    *)
   let info base { info_info = data } = data

   let set_info base info data =
      info.info_info <- data

   let file_name base { info_file = file } = file

   let full_name base { info_dir = dir; info_file = file; info_type = select } =
      let { info_suffix = suffix } = find_spec select in
         sprintf "%s/%s.%s" dir file suffix

   let type_of base { info_type = select } = select
end

(*
 * $Log$
 * Revision 1.7  1998/06/01 13:54:34  jyh
 * Proving twice one is two.
 *
 * Revision 1.6  1998/04/24 19:38:52  jyh
 * Updated debugging.
 *
 * Revision 1.5  1998/04/24 02:42:32  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.4  1998/02/23 14:46:34  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.3  1998/02/18 18:46:43  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.2  1998/02/12 23:35:15  jyh
 * Generalized file base to allow the library.
 *
 * Revision 1.1  1997/08/06 16:17:54  jyh
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
