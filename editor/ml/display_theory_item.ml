(*
 * Display an item in a module.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:05  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.3  1996/05/21 02:25:08  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 16:59:36  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:02  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

open Term;;
open TermUtil;;
open Rformat;;
open Dform;;
open Refine;;

(*
 * Display an axiom.
 *)
let display_axiom db buf name t =
   format_string buf "Axiom: ";
   format_string buf name;
   format_string buf "\n--\n";
   format_term db buf t;;

(*
 * A rule is a meta term.
 *)
let display_rule db buf name t =
   let rec check_rule = function
       MetaImplies (MetaTheorem _, t) ->
          check_rule t
     | MetaTheorem _ ->
          true
     | _ -> false
   in
   let rec dest_rule = function
       MetaImplies (MetaTheorem x, t) ->
          x::(dest_rule t)
     | MetaTheorem t ->
          [t]
     | _ ->
          raise (Invalid_argument "dest_rule")
   in
   let rec do_term i t =
      format_szone buf;
      format_int buf i;
      format_string buf ". ";
      format_pushm buf 0;
      format_term db buf t;
      format_popm buf;
      format_ezone buf;
      format_newline buf
   in
   let rec do_term_list i = function
      [t] ->
         format_szone buf;
         format_string buf ">> ";
         format_pushm buf 0;
         format_term db buf t;
         format_popm buf;
         format_ezone buf;
         format_newline buf
    | h::t ->
         do_term i h;
         do_term_list (i + 1) t
    | [] ->
         ()
   in
      format_string buf "Rule: ";
      format_string buf name;
      format_string buf "\n--\n";
      if check_rule t then
         do_term_list 1 (dest_rule t)
      else
         format_mterm db buf t;;

(*
 * For a rewrite, display the redex and contractum.
 *)
let display_rewrite db buf name redex contractum =
   (* Title *)
   format_string buf "Rewrite: ";
   format_string buf name;
   format_string buf "\n--\n";

   (* Redex *)
   format_string buf "-- Redex:\n";
   format_szone buf;
   format_pushm buf 0;
   format_term db buf redex;
   format_popm buf;
   format_ezone buf;
   format_newline buf;

   (* Contractum *)
   format_string buf "-- Contractum:\n";
   format_szone buf;
   format_pushm buf 0;
   format_term db buf contractum;
   format_popm buf;
   format_ezone buf;;

(*
 * A conditional rewrite has extra info.
 *)
let display_cond_rewrite db buf name conds redex contractum =
   let format_cond i t =
      format_szone buf;
      format_int buf i;
      format_string buf ". ";
      format_pushm buf 0;
      format_term db buf t;
      format_popm buf;
      format_ezone buf;
      format_newline buf;
      (i + 1)
   in
      (* Title *)
      format_string buf "Conditional Rewrite: ";
      format_string buf name;
      format_string buf "\n--\n";

      (* Conditions *)
      format_string buf "-- Conditions:\n";
      it_list format_cond 1 conds;

      (* Redex *)
      format_string buf "-- Redex:\n";
      format_szone buf;
      format_pushm buf 0;
      format_term db buf redex;
      format_popm buf;
      format_ezone buf;
      format_newline buf;

      (* Contractum *)
      format_string buf "-- Contractum:\n";
      format_szone buf;
      format_pushm buf 0;
      format_term db buf contractum;
      format_popm buf;
      format_ezone buf;;   

(*
 * A condition is just a term.
 *)
let display_condition db buf t =
   format_string buf "-- Condition:\n";
   format_szone buf;
   format_pushm buf 0;
   format_term db buf t;
   format_popm buf;
   format_ezone buf;;

(*
 * Just case over all the types.
 *)
let rec display_theory_item db buf = function
   RIAxiom { ri_axiom_name = name; ri_axiom_term = t } ->
      display_axiom db buf name t
 | RIRule { ri_rule_name = name; ri_rule_rule = t } ->
      display_rule db buf name t
 | RIRewrite { ri_rw_name = name; ri_rw_redex = r; ri_rw_contractum = c } ->
      display_rewrite db buf name r c
 | RICondRewrite { ri_crw_name = name;
                   ri_crw_conds = conds;
                   ri_crw_redex = redex;
                   ri_crw_contractum = contractum
                 } ->
      display_cond_rewrite db buf name conds redex contractum
 | RIPrimRewrite { ri_prw_rewrite = r } ->
      let item, _ = dest_refiner r in
         format_string buf "Primitive rewrite:";
         format_newline buf;
         display_theory_item db buf item
 | RIMLRewrite { ri_ml_rw_name = name } ->
      format_string buf "ML Rewrite: ";
      format_string buf name;
      format_string buf "\n--";
 | RIMLRule { ri_ml_rule_name = name } ->
      format_string buf "ML Rule: ";
      format_string buf name;
      format_string buf "\n--";
 | RIMLCondition { ri_ml_cond_arg = t } ->
      display_condition db buf t
 | RIPrimTheorem { ri_pthm_axiom = r } ->
      let item, _ = dest_refiner r in
         format_string buf "Primitive theorem:";
         format_newline buf;
         display_theory_item db buf item
 | RIParent r ->
      format_string buf "Parent\n--"
 | RILabel s ->
      format_string buf "Label: ";
      format_string buf s;
      format_string buf "\n--";;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
