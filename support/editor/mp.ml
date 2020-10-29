(*
 * This is the main file for MetaPRL.
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
module Shell = Shell.Shell (Shell_mp.ShellP4)

open Shell
open Lm_printf

module Nuprl = Nuprl_eval.Nuprl (Shell.Edit)
module NuprlRun = Nuprl_run.NuprlRun (Nuprl)

let run_nuprl = NuprlRun.run_connection
let run_nuprljp = NuprlRun.run_jprover

(*
 * BUG JYH: reenable these?

(*
 * The possible objects in a package.
 *)
let create_rw = create_rw (get_current_shell ())
let create_axiom = create_axiom (get_current_shell ())
let create_thm = create_thm (get_current_shell ())
let create_opname = create_opname (get_current_shell ())
let create_condition = create_condition (get_current_shell ())
let create_parent = create_parent (get_current_shell ())
let create_dform = create_dform (get_current_shell ())
let create_prec = create_prec (get_current_shell ())
let create_prec_rel = create_prec_rel (get_current_shell ())
let create_resource = create_resource (get_current_shell ())
let create_infix = create_infix (get_current_shell ())
let create_ml = create_ml (get_current_shell ())

(*
 * Editing commands.
 *)
let set_goal = set_goal (get_current_shell ())
let set_redex = set_redex (get_current_shell ())
let set_contractum = set_contractum (get_current_shell ())
let set_assumptions = set_assumptions (get_current_shell ())
let set_params = set_params (get_current_shell ())

(*
 * Proof editing.
 *)
let goal () = goal (get_current_shell ())
 *)

(*
 * Nuprl5 interface.
 *)
let edit_list_modules = Edit.list_modules
let edit_list_module_all = Edit.list_module_all
let edit_list_module = Edit.list_module
let edit_list_module_rw = Edit.list_module_rw
let edit_list_parents = Edit.list_parents
let edit_list_dforms = Edit.list_dforms
let edit_list_precs = Edit.list_precs
let edit_list_prec_rels = Edit.list_prec_rels
let edit_cd_list_contents = Edit.cd_list_contents
let edit_create_thm = Edit.create_thm
let edit_create_rw = Edit.create_rw
let edit_cd_thm = Edit.cd_thm
let edit_set_goal = Edit.set_goal
let edit_set_redex = Edit.set_redex
let edit_set_contractum = Edit.set_contractum
let edit_set_assumptions = Edit.set_assumptions
(* let edit_set_params = Edit.set_params *)
let edit_refine = Edit.refine
let edit_node = Edit.node
let edit_save = Edit.save
let edit_undo = Edit.undo

let shell_get_term = Shell_state.get_term

(* let _ = Shell.Main.main () *)

let _ =
   eprintf "
!!                     ***************                           !!
!!                     *** WARNING ***                           !!
!!                     ***************                           !!
!!                                                               !!
!! This version of MetaPRL with OCaml toploop is only meant      !!
!! to be used as a tactic development aid.                       !!
!! Proofs developed in such toploop might not replay later       !!
!! due to the differences in namespace management between        !!
!! the different toploops.                                       !!
!! Please always use MetaPRL with MetaPRL toploop                !!
!!   (run mpopt for native code version, mptop for bytecode one) !!
!! for proof development.                                        !!

%t" eflush

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
