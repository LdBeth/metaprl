(*
 * Create an ediable rewrite object.
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
extends Shell_sig
extends Package_info
extends Summary

open Refiner.Refiner.RefineError
open Dform

open Summary
open Shell_sig

(************************************************************************
 * SHELL INTERFACE                                                      *
 ************************************************************************)

(*
 * Error handler.
 *)
let raise_edit_error s =
   raise (RefineError ("Shell_root", StringError s))

(*
 * Build the shell interface.
 *)
let rec edit pack get_dfm =
   let edit_check_addr addr =
      if addr <> [] then
         raise (Failure "Shell_root.edit_check_addr: internal error")
   in
   let edit_display addr _ =
      edit_check_addr addr;
      (* Display the roots of the package *)
      let packs = Package_info.packages pack in
      let terms = List.map (fun root -> mk_package_term (Package_info.name root)) packs in
      let terms = mk_package_term "fs" :: terms in
      let term = mk_packages_term terms in
         Proof_edit.display_term (get_dfm ()) term
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
      []
   in
   let edit_redo addr =
      []
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
   let edit_is_enabled = function
      MethodRefine
    | MethodPaste _
    | MethodUndo ->
         false
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
