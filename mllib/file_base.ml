(*
 * This module defines an interface for treating the filesystem
 * as a database of objects stored in files.  The database has a
 * search path, and file types are identified by their suffix, and
 * a magic number stored as the initial binary int in the file.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
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

open Printf

open Lm_debug
open File_base_type

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading File_base%t"

let debug_file_base =
   create_debug (**)
      { debug_name = "file_base";
        debug_description = "display file operations on logic files";
        debug_value = false
      }

(*
 * Make the summary from the info in the Combo.
 *)
module MakeFileBase (Info : FileBaseInfoSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Save the common types.
    *)
   type cooked = Info.cooked
   type select = Info.select
   type arg = Info.arg

   (*
    * This is the info we keep about modules.
    *)
   type info =
      { mutable info_info : cooked;
        info_type : select;
        info_dir : string;
        info_file : string;
        mutable info_magic : int;
        info_time : float
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
    * Filename for a spec.
    *)
   let spec_filename spec dir name =
      let { info_suffix = suffix } = spec in
         sprintf "%s/%s.%s" dir name suffix

   (*
    * find the newest file in the spec list.
    *)
   let newest_spec specs dir name =
      match specs with
         [] ->
            raise(Invalid_argument("File_base.newest_spec"))
       | [spec] ->
            let time =
               try
                  let filename = spec_filename spec dir name in
                  let stat = Unix.stat filename in
                     stat.Unix.st_mtime
               with
                  Unix.Unix_error _ ->
                     0.0
            in
               spec, time
       | spec :: specs ->
            let rec search spec time = function
               spec' :: specs ->
                  let filename = spec_filename spec' dir name in
                  let time' =
                     try
                        let stat = Unix.stat filename in
                           stat.Unix.st_mtime
                     with
                        Unix.Unix_error _ ->
                           0.0
                  in
                     if time' > time then
                        search spec' time' specs
                     else
                        search spec time specs
             | [] ->
                  spec, time
            in
            let filename = spec_filename spec dir name in
            let time =
               try
                  let stat = Unix.stat filename in
                     stat.Unix.st_mtime
               with
                  Unix.Unix_error _ ->
                     0.0
            in
               search spec time specs

   (*
    * Load a file given the directory, the filename, and a list
    * of specs.
    *)
   let load_file save_flag base specs arg dir name =
      let spec, time = newest_spec specs dir name in
      let filename = spec_filename spec dir name in
      let _ =
         if !debug_file_base then
            eprintf "File_base.load_file: trying to load file %s%t" filename eflush
      in
      let info, magic = spec.info_unmarshal spec.info_magics spec.info_versions arg filename in
      let _ =
         if !debug_file_base then
            eprintf "File_base.load_file: loaded file %s%t" filename eflush
      in
      let info' =
         { info_info = info;
           info_file = name;
           info_type = spec.info_select;
           info_dir = dir;
           info_magic = magic;
           info_time = time
         }
      in
         if save_flag then
            add_info base info';
         info'

   (*
    * Find an existing root module.
    * If it doesn't exist in the base, search for it
    * in the filesystem and load it.  The type preference is
    * given by the ordering of Combo info items.
    *)
   let load_specific save_flag base specs arg name =
      let rec search = function
         [] ->
            raise(Failure("Failed to find the specified format of " ^ name))
       | dir::path' ->
            if !debug_file_base then
               eprintf "File_base.load_specific: try %s/%s%t" dir name eflush;
            try load_file save_flag base specs arg dir name with
               Sys_error _
             | Not_found ->
                  search path'
      in
         search base.io_path

   (*
    * Find the specification corresponding to the select.
    *)
   let find_specs select suffix =
      let rec search infos = function
         io::tl ->
            let { info_select = select'; info_suffix = suffix'; info_disabled = disabled } = io in
               if select' = select & not !disabled then
                  let infos =
                     match suffix with
                        AnySuffix ->
                           io :: infos
                      | OnlySuffixes suffixes ->
                           if List.mem suffix' suffixes then
                              io :: infos
                           else
                              infos
                  in
                     search infos tl
               else
                  search infos tl
       | [] ->
            if infos = [] then
               raise (Invalid_argument "File_base.find_spec");
            List.rev infos
      in
         search [] Info.info

   let find_spec select suffix =
      List.hd (find_specs select suffix)

   (*
    * Find a root module.
    * Check if it has already been loaded, otherwise load it.
    *)
   let find base arg name select suffix =
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
                  eprintf "File_base.find: %s: found %s%t" name info.info_file eflush;
               info
         with
            Not_found ->
               if !debug_file_base then
                  eprintf "File_base.find: %s: loading%t" name eflush;
               load_specific true base (find_specs select suffix) arg name

   let find_file base arg name select suffix =
      load_specific false base (find_specs select suffix) arg name

   (*
    * Find a "matching" module.
    * This means the root with the same name, but different suffix.
    *)
   let find_match base arg info select suffix =
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
               load_file true base (find_specs select suffix) arg dir file

   (*
    * Set the magic number.
    *)
   let magic _ { info_magic = magic } =
      magic

   let set_magic _ info magic =
      let { info_type = select } = info in
      let { info_magics = magics } = find_spec select AnySuffix in
         if magic < List.length magics then
            info.info_magic <- magic
         else
            raise (Invalid_argument "set_magic")

   (*
    * Save a module specification.
    * Try saving in all the valid formats until one of them succeeds.
    *)
   let save base arg info suffix =
      let spec = find_spec info.info_type suffix in
      let filename = sprintf "%s/%s.%s" info.info_dir info.info_file spec.info_suffix in
         spec.info_marshal spec.info_magics info.info_magic spec.info_versions arg filename info.info_info

   (*
    * Save a module specification.
    * Try saving in all the valid formats until one of them succeeds.
    *)
   let save_if_newer base arg info suffix =
      let spec = find_spec info.info_type suffix in
      let filename = sprintf "%s/%s.%s" info.info_dir info.info_file spec.info_suffix in
      let time' =
         try
            let stat = Unix.stat filename in
               stat.Unix.st_mtime
         with
            Unix.Unix_error _ ->
               0.0
      in
         if time' < info.info_time then
            spec.info_marshal spec.info_magics info.info_magic spec.info_versions arg filename info.info_info

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
           info_file = file;
           info_magic = 0;
           info_time = Unix.gettimeofday ()
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

   let save_as base arg data select dir file suffix =
      let info = create_info base data select dir file in
         save base arg info suffix;
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
      let { info_suffix = suffix } = find_spec select AnySuffix in
         sprintf "%s/%s.%s" dir file suffix

   let type_of base { info_type = select } = select
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
