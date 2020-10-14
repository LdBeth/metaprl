(*
 * Create an ediable rewrite object.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2004 MetaPRL Group, Cornell University and Caltech
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
extends Summary

open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError

open Shell_sig

(************************************************************************
 * SHELL INTERFACE                                                      *
 ************************************************************************)

declare "package"[name:s] : Dform
declare "packages"[name:s, dsc:s]{'pl : Dform} : Dform
declare "theory"[name:s, dsc:s] : Dform
declare "theories"{'tl : Dform} : Dform

(*
 * Packages.
 *)
declare packages_df{'t : Dform} : Dform
declare theories_df{'t : Dform} : Dform

dform packages_df1 : packages[name,dsc]{'packages} =
   szone pushm[0] pushm[4] info[dsc] info[" ("] info[name] info[") contains:"]
       packages_df{'packages} popm newline
   info["end"] popm ezone

dform packages_df2 : packages_df{xcons{package[name:s]; 'next}} =
   newline info["Module "] cd_begin[name] slot[name:s] cd_end packages_df{'next}

dform packages_df3 : packages_df{xnil} = `""

dform theories_df1: theories{'tl} =
   szone pushm[0] pushm[4] info["Root Theories Listing:"]
      theories_df{'tl} popm newline
   info["end"] popm ezone

dform packages_df2 : theories_df{xcons{theory[name:s,dsc:s]; 'next}} =
   newline
   info["Theory \""] cd_begin[name] slot[name:s] cd_end info["\": "] cd_begin[name] slot[dsc:s] cd_end
   theories_df{'next}

dform packages_df3 : theories_df{xnil} = `""

(*
 * Error handler.
 *)
let raise_edit_error s =
   raise (RefineError ("Shell_root", StringError s))

(*
 * Build the shell interface.
 *)
let rec edit pack get_dfm =
   let edit_check_addr = function
      [] -> ()
    | [ name ] when Package_info.group_exists pack name -> ()
    | _ ->
         raise (Failure "Shell_root.edit_check_addr: internal error")
   in
   let edit_display addr _ =
      edit_check_addr addr;
      match addr with
         [] ->
            let groups = Package_info.groups pack @ ["fs", "Browse MetaPRL Source Code"] in
            let thys = List.map (fun (name, dsc) -> <:con< theory[$name$:s,$dsc$:s] >>) groups in
               Proof_edit.display_term_newline (get_dfm ()) <:con< theories{$mk_xlist_term thys$} >>
       | [ name ] ->
            let dsc, packs = Package_info.group_packages pack name in
            let packs = List.map (fun name -> <:con< package[$name$:s] >>) packs in
               Proof_edit.display_term_newline (get_dfm ()) <:con< packages[$name$:s,$dsc$:s]{$mk_xlist_term packs$} >>
       | _ ->
            raise (Invalid_argument "Shell_root.edit_display: internal error")
   in
   let edit_copy () =
      edit pack get_dfm
   in
   let not_a_rule _ =
      raise_edit_error "this is not a rule or rewrite"
   in
   let edit_save () =
      raise_edit_error "list of packages can't be saved"
   in
   let edit_check _ =
      raise_edit_error "check the complete set of packages? Use check_all."
   in
   let edit_undo addr =
      addr
   in
   let edit_redo addr =
      addr
   in
   let edit_info addr =
      raise_edit_error "no info for the root packages"
   in
   let edit_interpret command =
      raise_edit_error "this is not a proof"
   in
   let edit_get_contents addr =
      raise_edit_error "can only retrieve contents of an individual item, not of a root package"
   in

   (*
    * This function always returns false.
    * However, it is wise to keep it because
    * we may add more methods.
    *)
   let edit_is_enabled _ = function
      MethodRefine
    | MethodPaste _
    | MethodUndo
    | MethodRedo
    | MethodExpand ->
         false
    | MethodApplyAll ->
         true
   in
      { edit_display = edit_display;
        edit_get_contents = edit_get_contents;
        edit_get_terms = not_a_rule;
        edit_copy = edit_copy;
        edit_set_goal = not_a_rule;
        edit_set_redex = not_a_rule;
        edit_set_contractum = not_a_rule;
        edit_set_assumptions = not_a_rule;
        edit_set_params = not_a_rule;
        edit_get_extract = not_a_rule;
        edit_save = edit_save;
        edit_check = edit_check;
        edit_check_addr = edit_check_addr;
        edit_info = edit_info;
        edit_undo = edit_undo;
        edit_redo = edit_redo;
        edit_interpret = edit_interpret;
        edit_find = not_a_rule;
        edit_is_enabled = edit_is_enabled
      }

let create = edit

(*
 * Note: in this particular case, view is the same as create.
 * However, in general, view is for viewing existing objects,
 * and create is used for creating new objects.  So we keep them
 * separate here.
 *)
let view = create

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
