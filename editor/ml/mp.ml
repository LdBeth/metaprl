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

module Shell = Shell.Shell (Shell_p4.ShellP4 (Shell_state.ShellState))
module ShellHTTP = Shell_http.ShellHTTP (Shell)

open Shell

module Nuprl = Nuprl_eval.Nuprl 
module NuprlRun = Nuprl_run.NuprlRun 

let run_nuprl = NuprlRun.run_connection

(*
 * Job control.
 *)
let fork = fork (get_current_shell ())
let pid = pid (get_current_shell ())
let jobs = jobs (get_current_shell ())
let fg = fg (get_current_shell ())

(*
 * Navigation and display.
 *)
let cd = cd (get_current_shell ())
let pwd () = pwd (get_current_shell ())
let set_window_width = set_window_width (get_current_shell ())

(*
 * Module commands.
 *)
let load = load (get_current_shell ())
let create_pkg = create_pkg (get_current_shell ())
let set_writeable () = set_writeable (get_current_shell ())
let save () = save (get_current_shell ())
let export () = export (get_current_shell ())
let save_all () = save_all (get_current_shell ())

(*
 * The possible objects in a package.
 *)
let create_rw = create_rw (get_current_shell ())
let create_axiom = create_axiom (get_current_shell ())
let create_thm = create_thm (get_current_shell ())
let create_ax_statement = create_ax_statement (get_current_shell ())
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
 * View, close, check object.
 * An object is not installed until it is checked.
 *)
let view = view (get_current_shell ())
let ls s = ls (get_current_shell ()) s

(*
 * Editing commands.
 *)
let set_goal = set_goal (get_current_shell ())
let set_redex = set_redex (get_current_shell ())
let set_contractum = set_contractum (get_current_shell ())
let set_assumptions = set_assumptions (get_current_shell ())
let set_params = set_params (get_current_shell ())
let check () = check (get_current_shell ())
let expand () = expand (get_current_shell ())

(*
 * Proof editing.
 *)
let root () = root (get_current_shell ())
let up = up (get_current_shell ())
let down = down (get_current_shell ())
let goal () = goal (get_current_shell ())
let refine = refine (get_current_shell ())
let undo () = undo (get_current_shell ())
let redo () = redo (get_current_shell ())
let nop () = nop (get_current_shell ())
let unfold () = unfold (get_current_shell ())
let copy s = copy (get_current_shell ()) s
let paste s = paste (get_current_shell ()) s
let kreitz () = kreitz (get_current_shell ())
let clean () = clean (get_current_shell ())
let squash () = squash (get_current_shell ())
let make_assum () = make_assum (get_current_shell ())

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

let shell_get_term = Shell_state.ShellState.get_term

let _ = ShellHTTP.main ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
