(*
 * Display all the elements in a particular theory.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:02  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:32:45  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:24:52  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:25  jyh
 * This is an intermediate form of the editor with packages
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:03:53  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

open Util;;
open Rformat;;
open Refine;;
open Theory;;
open DformPrint;;
open Dform;;
open Proof;;
open ProofEdit;;
open PackageInfo;;
open PackageUtil;;

(*
 * Width to print a name.
 *)
let spread_width = 24;;

(*
 * Spread out a string.
 *)
let spread_string size s =
   let len = String.length s in
   let padding = size - len in
      if padding > 0 then
         s ^ (make_string padding ' ')
      else
         s;;

(*
 * Record the items that need to be recorded.
 *)
let record_item sbase refiner = function
   RIPrimRewrite { ri_prw_rewrite = r } ->
      push r sbase
   
 | RIPrimTheorem { ri_pthm_axiom = r } ->
      push r sbase

 | _ ->
      ();;

(*
 * Display a refiner item.
 *)
let display_refiner_item dbase buf sbase refiner item =
   let spread = spread_string spread_width in
   begin
      match item with
	 RIAxiom { ri_axiom_name = n; ri_axiom_term = t } ->
	    format_lzone buf;
	    format_string buf (if memq refiner !sbase then "* R " else "? R ");
	    format_string buf (spread n);
	    format_string buf " ";
	    format_term dbase buf t;
	    format_ezone buf;
	    format_newline buf
   
       | RIRule { ri_rule_name = n; ri_rule_rule = rule } ->
	    format_lzone buf;
	    format_string buf (if memq refiner !sbase then "* R " else "? R ");
	    format_string buf (spread n);
	    format_string buf " ";
	    format_mterm dbase buf rule;
	    format_ezone buf;
	    format_newline buf
   
       | RIRewrite { ri_rw_name = n; ri_rw_redex = redex } ->
	    format_lzone buf;
	    format_string buf (if memq refiner !sbase then "* W " else "? W ");
	    format_string buf (spread n);
	    format_string buf " ";
	    format_term dbase buf redex;
	    format_ezone buf;
	    format_newline buf
   
       | RICondRewrite { ri_crw_name = n; ri_crw_redex = redex } ->
	    format_lzone buf;
	    format_string buf (if memq refiner !sbase then "* W " else "? W ");
	    format_string buf (spread n);
	    format_string buf " ";
	    format_term dbase buf redex;
	    format_ezone buf;
	    format_newline buf
   
       | RIMLRewrite { ri_ml_rw_name = n } ->
	    format_lzone buf;
	    format_string buf "M W ";
	    format_string buf (spread n);
	    format_ezone buf;
	    format_newline buf
   
       | RIMLRule { ri_ml_rule_name = n } ->
	    format_lzone buf;
	    format_string buf "M R ";
	    format_string buf (spread n);
	    format_ezone buf;
	    format_newline buf
   
       | RIMLCondition { ri_ml_cond_arg = t } ->
	    format_lzone buf;
	    format_string buf "M C ";
	    format_term dbase buf t;
	    format_ezone buf;
	    format_newline buf

       | RIParent r ->
	    format_lzone buf;
	    begin
	       match dest_refiner r with
		  RILabel name, _ ->
		     format_string buf "* P ";
		     format_string buf (spread name)
                                 
		| _ ->
		     format_string buf "? P"
	    end;
	    format_ezone buf;
	    format_newline buf
   
       | RILabel name ->
	    format_lzone buf;
	    format_string buf "* L ";
	    format_string buf (spread name);
	    format_ezone buf;
	    format_newline buf

       | RIPrimRewrite _ ->
	    ()
   
       | RIPrimTheorem _ ->
	    ()
   end;;

(*
 * String printer.
 *)
let format_strings buf =
   let rec aux = function
      [l] -> format_string buf l
    | h::t -> format_string buf h;
	      format_string buf ",";
	      aux t
    | [] -> ()
   in
      aux;;

(*
 * Display a package item.
 *)
let display_package_thm db buf { thm_name = name; thm_ped = ped } =
   let pf = proof_of_ped ped in
   let goal = proof_goal pf in
   let status = proof_status pf in
   let code =
      match status with
         (StatusBad, _) :: _ -> '-'
       | (StatusPartial, _) :: _ -> '#'
       | (StatusAsserted, _) :: _ -> '!'
       | (StatusComplete, _) :: _ -> '*'
       | [] -> '?'
   in
      format_lzone buf;
      format_char buf code;
      format_string buf " T ";
      format_string buf (spread_string spread_width name);
      format_term db buf goal;
      format_ezone buf;
      format_newline buf;;

(*
 * Display the entire package.
 *)
let display_package mode buf pack =
   (* First display the items *)
   let thms = package_thms pack in
   let sbase = ref [] in
   let refiner = package_refiner pack in
   let dbase = package_dforms pack in
   let db = get_mode_base dbase mode in
      
   (* Record all the proved items in the refiner *)
   let rec record_refiner refiner =
      if is_null_refiner refiner then
         ()
      else
         let item, refiner' = dest_refiner refiner in
            record_item sbase refiner item;
            record_refiner refiner'
   in
      
   (* Display the items in the refiner *)
   let rec display_refiner refiner =
      if is_null_refiner refiner then
         ()
      else
         let item, refiner' = dest_refiner refiner in
	 let name = unknown_item_name item in
	    display_refiner refiner';
            display_refiner_item db buf sbase refiner item
   in
      
   (* Display all the display forms *)
   let display_dforms buf dbase =
      let rec display_dfbase base =
         if is_null_dfbase base then
            ()
         else
            let info, base' = dest_dfbase base in
               display_dfbase base';
               begin
                  match info with
                     DFormEntry { dform_pattern = t } ->
                        format_lzone buf;
                        format_string buf "D ";
                        format_quoted_term db buf t;
                        format_ezone buf;
                        format_newline buf;
                   | _ ->
                        ()
               end
      in
      let display_mode_base (name, base) =
         format_string buf "-- Display forms for \"";
         format_string buf name;
         format_string buf "\" mode";
         format_newline buf;
         display_dfbase base
      in
      let xall, bases = dest_mode_base dbase in
	 format_string buf "-- General display forms:";
	 format_newline buf;
	 display_dfbase xall;
         do_list display_mode_base bases
   in
      format_string buf "-- Theorems:\n";
      do_list (display_package_thm db buf) thms;
      format_string buf "-- Refiner objects:\n";
      record_refiner refiner;
      display_refiner refiner;
      format_string buf "-- Display forms:\n";
      display_dforms buf dbase;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
