(*
 * This is the main file for MetaPRL.
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

module Shell = Shell.Shell (Shell_p4.ShellP4)

open Shell
open Lm_printf

module Nuprl = Nuprl_eval.Nuprl(Shell)
module NuprlRun = Nuprl_run.NuprlRun(Nuprl)

let run_nuprl = NuprlRun.run_connection
let run_nuprljp = NuprlRun.run_jprover

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

(*
 * Nuprl5 interface.
 *)
let edit_list_modules () = edit_list_modules (get_current_shell ())
let edit_list_module_all = edit_list_module_all (get_current_shell ())
let edit_list_module = edit_list_module (get_current_shell ())
let edit_list_module_rw = edit_list_module_rw (get_current_shell ())
let edit_list_parents = edit_list_parents (get_current_shell ())
let edit_list_dforms = edit_list_dforms (get_current_shell ())
let edit_list_precs = edit_list_precs (get_current_shell ())
let edit_list_prec_rels = edit_list_prec_rels (get_current_shell ())
let edit_cd_list_contents = edit_cd_list_contents (get_current_shell ())
let edit_create_thm = edit_create_thm (get_current_shell ())
let edit_create_rw = edit_create_rw (get_current_shell ())
let edit_cd_thm = edit_cd_thm (get_current_shell ())
let edit_set_goal = edit_set_goal (get_current_shell ())
let edit_set_redex = edit_set_redex (get_current_shell ())
let edit_set_contractum = edit_set_contractum (get_current_shell ())
let edit_set_assumptions = edit_set_assumptions (get_current_shell ())
let edit_set_params = edit_set_params (get_current_shell ())
let edit_refine = edit_refine (get_current_shell ())
let edit_node = edit_node (get_current_shell ())
let edit_save = edit_save (get_current_shell ())
let edit_undo () = edit_undo (get_current_shell ())

let shell_get_term = Shell_state.get_term

let _ = Shell.main ()

let _ = eprintf "\n!!                     ***************                           !!\n!!                     *** WARNING ***                           !!\n!!                     ***************                           !!\n!!                                                               !!\n!! This version of MetaPRL with OCaml toploop is only meant      !!\n!! to be used as a tactic development aid.                       !!\n!! Proofs developed in such toploop might not replay later       !!\n!! due to the differences in namespace management between        !!\n!! the different toploops.                                       !!\n!! Please always use MetaPRL with MetaPRL toploop                !!\n!!   (run mpopt for native code version, mptop for bytecode one) !!\n!! for proof development.                                        !!\n\n%t" eflush


(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
