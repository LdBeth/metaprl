(*
 * Main routine.
 *
 *)

open Debug
open Util
open Refine

include Shell
include Package_info

(* set_debug_level DebugOff *)

primrw foldGuard : 'x <--> guard{'x}

let _ = set_path ["."; "../../theories/itt"; "../../theories/base"]

let _ = cd "/x"
let _ = view "test_thm"

(* Ask for loading *)
let _ = output_string stdout "# Nuprl-Light: directory \"/usr/u/jyh/nuprl/src/nuprl-light/refiner\"\n"
let _ = output_string stdout "# Nuprl-Light: directory \"/usr/u/jyh/nuprl/src/nuprl-light/theories/tactic\"\n"
let _ = output_string stdout "# Nuprl-Light: directory \"/usr/u/jyh/nuprl/src/nuprl-light/theories/base\"\n"
let _ = output_string stdout "# Nuprl-Light: directory \"/usr/u/jyh/nuprl/src/nuprl-light/theories/itt\"\n"
let _ = output_string stdout "# Nuprl-Light: directory \".\"\n"
let _ = output_string stdout "# Nuprl-Light: include \"editor_init\"\n"

let goal = (| sequent { >> P: univ[i:l] -> ('P => 'P) } |)
let arg = { ref_label = "main"; ref_args = [] }

let _ = Refiner.refine (lambdaFormation 0 "P") (goal, arg)

(*
 * $Log$
 * Revision 1.1  1997/08/06 16:17:14  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.6  1996/10/23 15:17:48  jyh
 * First working version of dT tactic.
 *
 * Revision 1.5  1996/09/02 19:33:01  jyh
 * Semi-working package management.
 *
 * Revision 1.4  1996/06/11 18:22:30  jyh
 * Demo version 0.0
 *
 * Revision 1.3  1996/05/21 02:25:21  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 16:59:44  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:14  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
