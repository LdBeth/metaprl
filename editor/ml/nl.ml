(*
 * This is the main file for Nuprl-Light.
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
let refine = refine
let undo = undo
let fold = fold
let fold_all = fold_all

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
