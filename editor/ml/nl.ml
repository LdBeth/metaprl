(*
 * This is the main file for Nuprl-Light.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

(*
 * Navigation and display.
 *)
let cd = cd
let pwd = pwd
let set_window_width = set_window_width

(*
 * Module commands.
 *)
let load = load
let create_pkg = create_pkg
let set_writeable = set_writeable
let save = save
let save_all = save_all

(*
 * The possible objects in a package.
 *)
let create_rw = create_rw
let create_axiom = create_axiom
let create_thm = create_thm
let create_tptp = create_tptp
let create_opname = create_opname
let create_condition = create_condition
let create_parent = create_parent
let create_dform = create_dform
let create_prec = create_prec
let create_prec_rel = create_prec_rel
let create_resource = create_resource
let create_infix = create_infix
let create_ml = create_ml

(*
 * View, close, check object.
 * An object is not installed until it is checked.
 *)
let view = view
let ls = ls

(*
 * Editing commands.
 *)
let set_goal = set_goal
let set_redex = set_redex
let set_contractum = set_contractum
let set_assumptions = set_assumptions
let set_params = set_params
let check = check
let expand = expand

(*
 * Proof editing.
 *)
let root = root
let up = up
let down = down
let goal = goal
let refine = refine
let undo = undo
let fold = fold
let fold_all = fold_all

let _ = Shell.main ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
