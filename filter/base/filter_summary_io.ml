(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
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
open Lm_debug
open Lm_printf

open File_base_type

open Filter_type
open Filter_summary
open Filter_summary_type

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_summary_io%t"

(*
 * Make the summary from the file base.
 * This just improves the FileBase so we can have
 * nested modules.
 *)
module MakeSummaryBase
   (Address : AddressSig)
   (FileBase : FileBaseSig with type cooked = Address.t) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Save the proof and tag types.
    *)
   type cooked = FileBase.cooked
   type select = FileBase.select
   type arg = FileBase.arg

   type info =
      { info_root : FileBase.info;
        info_path : module_path;
        mutable info_info : cooked
      }

   type t = FileBase.t

   (************************************************************************
    * INHERITED OPERATIONS                                                 *
    ************************************************************************)

   (*
    * Create a new base from the path.
    *)
   let create   = FileBase.create
   let clear    = FileBase.clear
   let set_path = FileBase.set_path

   (************************************************************************
    * LOAD/STORE                                                           *
    ************************************************************************)

   (*
    * Find a specific module given a full pathname.
    *)
   let find_aux find base arg name select suffix =
      if !debug_summary then
         eprintf "Filter_summary_io.find: %a%t" print_string_list name eflush;
      match name with
         [] ->
            raise (EmptyModulePath "Filter_summary_io.find")
       | name'::path ->
            let info = find base arg (String.uncapitalize name') select suffix in
            let info' = Address.find_sub_module (FileBase.info base info) path in
               { info_root = info;
                 info_path = name;
                 info_info = info'
               }

   let find = find_aux FileBase.find

   let find_file = find_aux FileBase.find_file

   (*
    * Find the matching module info.
    *)
   let find_match base arg info select suffix =
      let { info_root = root; info_path = path } = info in
      let root' = FileBase.find_match base arg root select suffix in
      let info = Address.find_sub_module (FileBase.info base root') (List.tl info.info_path) in
         { info_root = root';
           info_path = path;
           info_info = info
         }

   (*
    * Set the new magic number.
    *)
   let set_magic base { info_root = root } magic =
      FileBase.set_magic base root magic

   (*
    * Create an empty info.
    *)
   let create_info base select dir file =
      let data = Address.create () in
         { info_root = FileBase.create_info base data select dir file;
           info_path = [String.capitalize file];
           info_info = data
         }

   (*
    * Remove some info.
    *)
   let remove_info base info =
      FileBase.remove_info base info.info_root

   (*
    * Save a module specification.
    * This can only be called at a root.
    *)
   let save base arg info suffix =
      match info with
         { info_info = info; info_path = [_]; info_root = root } ->
            FileBase.set_info base root info;
            FileBase.save base arg root suffix
       | _ ->
            raise (Invalid_argument "Filter_summary_io.save")

   let save_if_newer base arg info suffix =
      match info with
         { info_info = info; info_path = [_]; info_root = root } ->
            FileBase.set_info base root info;
            FileBase.save_if_newer base arg root suffix
       | _ ->
            raise (Invalid_argument "Filter_summary_io.save_if_newer")

   let save_if_missing base arg info suffix =
      match info with
         { info_info = info; info_path = [_]; info_root = root } ->
            FileBase.set_info base root info;
            FileBase.save_if_missing base arg root suffix
       | _ ->
            raise (Invalid_argument "Filter_summary_io.save_if_missing")

   (************************************************************************
    * MODULE INFO                                                          *
    ************************************************************************)

   (*
    * Projections.
    *)
   let info base { info_info = data } =
      data

   let sub_info base { info_info = info; info_path = path; info_root = root } name =
      let path' = path @ [name] in
      let info' = Address.find_sub_module info path' in
         { info_info = info';
           info_path = path';
           info_root = root
         }

   let set_info base info data =
      info.info_info <- data

   let name base { info_path = path } =
      Lm_list_util.last path

   let pathname base { info_path = path } =
      path

   let root base { info_root = root; info_path = path } =
      { info_root = root;
        info_path = [Lm_list_util.last path];
        info_info = FileBase.info base root
      }

   let file_name base { info_root = root } =
      FileBase.full_name base root

   let type_of base { info_root = root } =
      FileBase.type_of base root
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
