(*
 * Some utilities for working on modules.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:20  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:33:26  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:33  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 17:00:01  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 *)

open Term;;
open Refine;;
open PackageInfo;;

(*
 * Compute the name of an item in a theory.
 *)
let item_name = function
   RIAxiom { ri_axiom_name = n } -> n
 | RIRule { ri_rule_name = n } -> n
 | RIRewrite { ri_rw_name = n } -> n
 | RICondRewrite { ri_crw_name = n } -> n
 | RIPrimRewrite _ -> raise (Invalid_argument "item_name")
 | RIMLRewrite { ri_ml_rw_name = n } -> n
 | RIMLRule { ri_ml_rule_name = n } -> n
 | RIMLCondition _ -> raise (Invalid_argument "item_name")
 | RIPrimTheorem _ -> raise (Invalid_argument "item_name")
 | RIParent _ -> raise (Invalid_argument "item_name")
 | RILabel _ -> raise (Invalid_argument "item_name");;

let unknown_item_name item =
   try item_name item with
      _ -> "unknown";;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
