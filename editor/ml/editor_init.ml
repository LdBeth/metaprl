(*
 * This creates the initialization file for the editor.
 * It opens lots of modules, and creates a new refiner
 * an display form base.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:08  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/10/23 15:17:47  jyh
 * First working version of dT tactic.
 *
 * Revision 1.3  1996/09/02 19:32:53  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:14  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:42  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 *)

open Toplevel;;

(*
 * Extra modules for debugging.
 *)
open Util;;
open Rewrite;;
open Proof;;
open ProofEdit;;
open Shell;;
open Main;;

(*
 * Collect the resources.
 *)
let resources =
   { ref_d = d_resource.extract d_resource;
     ref_eqcd = eqcd_resource.extract eqcd_resource;
     ref_typeinf = typeinf_resource.extract typeinf_resource;
     ref_squash = squash_resource.extract squash_resource;
     ref_subtype = subtype_resource.extract subtype_resource
   };;

(*
 * Collect the chain.
 *)
let cache = TermChain.new_cache ();;
let extract = TermChain.extract cache;;

postlog;;

(*
 * Initialization.
 *)
directory "/usr/u/jyh/nuprl/src/nuprl-light/refiner";;
directory "/usr/u/jyh/nuprl/src/nuprl-light/theories/tactic";;
directory "/usr/u/jyh/nuprl/src/nuprl-light/theories/base";;
directory "/usr/u/jyh/nuprl/src/nuprl-light/theories/itt";;
directory "/usr/u/jyh/nuprl/src/nuprl-light/editor/ml";;

let print_term =
   Shell.print_term $dformer;;

install_printer "print_term";;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
