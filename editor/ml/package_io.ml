(*
 * Saving/restoring packages from files.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:18  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:33:19  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:26  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:52  jyh
 * This is an intermediate form of the editor with packages
 * before debugging.  Will be removing theoryGraph files next.
 *
 *)

open Util;;
open Printf;;
open Term;;
open MlPrint;;
open Dform;;
open Proof;;
open ProofEdit;;
open EditUtil;;
open FileUtil;;
open FilterCache;;
open PackageInfo;;

(************************************************************************
 * UTILITIES                                                            *
 ************************************************************************)

let rec print_string_list ofile = function
   [h] -> output_string ofile h
 | h::t -> output_string ofile h;
	   output_string ofile "; ";
	   print_string_list ofile t
 | [] -> ();;

let print_term optable ofile t =
   let dformer = $dformer in
   let base = get_mode_base dformer "src" in
      Dform.print_short_term_fp base optable ofile t;;

let print_mterm optable =
   let rec aux ofile = function
      MetaTheorem t ->
         print_term optable ofile t
    | MetaImplies (a, b) ->
         fprintf ofile "(%a --> %a)" aux a aux b
    | MetaFunction (v, a, b) ->
         fprintf ofile "('%s : %a --> %a)" v aux a aux b
    | MetaIff (a, b) ->
         fprintf ofile "(%a <--> %a)" aux a aux b
   in
      aux;;

let print_conds optable ofile conds =
   let print_cond t =
      fprintf ofile "\t%a -->\n" (print_term optable) t
   in
      do_list print_cond conds;;

let print_dfoption ofile = function
   DFormInheritPrec ->
      output_string ofile "inherit :: "
 | DFormParens ->
      output_string ofile "parens :: "
 | DFormPrec p ->
      output_string ofile "prec[] :: ";;

let print_dfoptions ofile options =
   do_list (print_dfoption ofile) options;;

let print_dfexpansion = print_term;;

(************************************************************************
 * ITEM SAVERS                                                          *
 ************************************************************************)

(*
 * File header.
 *)
let save_copyright file =
   output_string file "\
(*
 * This file is generated automatically.
 *
 * $Header$
 *)\n\n";;

let save_int_header ifile =
   save_copyright ifile;
   fprintf ifile "open Term;;\n";;

let save_imp_header ofile =
   save_copyright ofile;
   output_string ofile "open Term;;
open ProofStep;;
open Proof;;
open ProofEdit;;
open PackageInfo;;
open Shell;;
";;

let save_header (ifile, ofile) =
   save_int_header ifile;
   save_imp_header ofile;;

(*
 * Save the items in the refiner.
 *)
let save_refiner_item (ifile, ofile) optable = function
   RIAxiom { ri_axiom_name = name; ri_axiom_term = t } ->
      fprintf ifile "axiom %s : %a;;\n\n"
      name (print_term optable) t
      
 | RIRule { ri_rule_name = name; ri_rule_rule = t } ->
      fprintf ifile "axiom %s : %a;;\n\n"
      name (print_mterm optable) t
      
 | RIPrimTheorem { ri_pthm_axiom = refiner } ->
      let rule, _ = dest_refiner refiner in
         begin
            match rule with
               RIAxiom { ri_axiom_name = name; ri_axiom_term = t } ->
                  fprintf ofile "prim %s : %a;;\n\n"
                  name (print_term optable) t
             | RIRule { ri_rule_name = name; ri_rule_rule = t } ->
                  fprintf ofile "prim %s : %a;;\n\n"
                  name (print_mterm optable) t
             | _ ->
                  raise (BadCommand "illegal theorem")
         end
          
 | RIRewrite { ri_rw_name = name; ri_rw_redex = redex; ri_rw_contractum = con } ->
      fprintf ifile "rewrite %s : %a <--> %a;;\n\n"
      name
      (print_term optable) redex
      (print_term optable) con
      
 | RICondRewrite { ri_crw_name = name;
                   ri_crw_conds = conds;
                   ri_crw_redex = redex;
                   ri_crw_contractum = con
   } ->
      fprintf ifile "rewrite %s :\n%a\t(%a <--> %a);;\n\n"
      name
      (print_conds optable) conds
      (print_term optable) redex
      (print_term optable) con
      
 | RIPrimRewrite { ri_prw_rewrite = refiner } ->
      let rw, _ = dest_refiner refiner in
         begin
            match rw with
               RIRewrite { ri_rw_name = name; ri_rw_redex = redex; ri_rw_contractum = con } ->
                  fprintf ofile "primrw %s : %a <--> %a;;\n\n"
                  name
                  (print_term optable) redex
                  (print_term optable) con
             | RICondRewrite { ri_crw_name = name;
                               ri_crw_conds = conds;
                               ri_crw_redex = redex;
                               ri_crw_contractum = con
               } ->
                  fprintf ofile "primrw %s :\n%a--\t(%a <--> %a);;\n\n"
                  name
                  (print_conds optable) conds
                  (print_term optable) redex
                  (print_term optable) con
             | _ ->
                  raise (BadCommand "illegal rewrite theorem")
         end
         
 | RIParent refiner ->
      (* Look for a label describing the parent refiner *)
      let lab, _ = dest_refiner refiner in
         begin
            match lab with
               RILabel s ->
                  fprintf ifile "include %s;;\n" (uppercase s)
             | _ ->
                  raise (BadCommand "bad parent")
         end
         
 | RILabel _ ->
      ()
      
 | RIMLRewrite _ ->
      raise (BadCommand "can't save MLRewrites")
      
 | RIMLRule _ ->
      raise (BadCommand "can't save ML rules")
      
 | RIMLCondition _ ->
      raise (BadCommand "can't save ML condition");;

let save_refiner files optable refiner =
   let rec aux refiner =
      if is_null_refiner refiner then
         ()
      else
         let item, next = dest_refiner refiner in
            save_refiner_item files optable item;
            aux next
   in
   let head = "\n(* Refiner items *)\n" in
   let ifile, ofile = files in
      output_string ifile head;
      output_string ofile head;
      aux refiner;;

(*
 * Save the dforms in the module.
 *)
let save_dform ofile optable mode = function
   DFormEntry { dform_pattern = t;
                dform_options = opts;
                dform_print = DFormExpansion t'
   } ->
      output_string ofile "dform ";
      begin
         match mode with
            Some m ->
               fprintf ofile "mode[%s : s] :: " m
          | None ->
               ()
      end;
      fprintf ofile "%a %a = %a;;\n\n"
      print_dfoptions opts
      (print_term optable) t
      (print_dfexpansion optable) t'
      
 | DFormEntry _ ->
      raise (BadCommand "can't save dform")
      
 | DFormBase _ ->
      ();;

let save_dforms (_, ofile) optable dformer =
   let rec save_dfbase mode base =
      if is_null_dfbase base then
         ()
      else
         let info, base' = dest_dfbase base in
            save_dfbase mode base';
            save_dform ofile optable mode info
   in
   let amode, modes = dest_mode_base dformer in
   let save_mode (mode, base) =
      save_dfbase (Some mode) base
   in
   let head = "\n(* Display forms *)\n" in
      output_string ofile head;
      save_dfbase None amode;
      do_list save_mode modes;;

(*
 * Theorems.
 *)
let thms_name = "dont_use_this_thm_info_name";;

let save_thm (ifile, ofile) optable { thm_name = name; thm_ped = ped } =
   let pf = proof_main (proof_of_ped ped) in
   let goal = proof_goal pf in
   let subgoals = proof_subgoals pf in
      (* Interface *)
      fprintf ifile "axiom %s :\n%a\n\t%a;;\n"
      name
      (print_conds optable) subgoals
      (print_term optable) goal;
      
      (* Prove it *)
      fprintf ofile "let %s_proof =\n%a;;\n\n"
      name
      print_inline_proof pf;
      
      fprintf ofile "let %s =\n\t{ thm_name = \"%s\";\n\t  thm_ped = new_ped (proof_of_io_node %s_proof)\n\t} :: %s;;\n"
      thms_name (string_for_read name) name thms_name;;

let save_thms files optable thms =
   let ifile, ofile = files in
   let head = "\n(* Theorems *)\n" in
      output_string ifile head;
      output_string ofile head;
      fprintf ofile "\nlet %s = [];;\n" thms_name;
      do_list (save_thm files optable) thms;;

let save_trailer (ifile, ofile) filename =
   let name = path_file filename in
      fprintf ofile "postlog;;\n";

      fprintf ofile "let %s_pack = make_package $theory %s \"%s\";;\n" %
	 name
	 thms_name
	 (string_for_read filename);
      
      fprintf ofile "install_package %s_pack;;\n" name;;
   
(*
 * Save the current package info a file.
 *)
let save_package pack filename =
   let ifile = open_out (filename ^ ".prli") in
   let ofile =
      try open_out (filename ^ ".prl") with
         x -> close_out ifile;
              raise x
   in
   let files = ifile, ofile in
   let refiner = package_refiner pack in
   let dforms = package_dforms pack in
   let thms = package_thms pack in
   let optable = get_optable (package_cache pack) in
   let save_it () =
      save_header files;
      save_refiner files optable refiner;
      save_dforms files optable dforms;
      save_thms files optable thms;
      save_trailer files filename
   in
   let close_files () = 
      close_out ofile;
      close_out ifile
   in
      begin
         try save_it () with
            x ->
               close_files ();
               raise x
      end;
      close_files ();;

(************************************************************************
 * MAIN FUNCTIONS                                                       *
 ************************************************************************)

let save pack =
   if package_status pack = PackageModified then
      match package_file pack with
         Some filename ->
            save_package pack filename
       | None ->
            raise (BadCommand ("package \""  ^ (package_name pack) ^ "\" is not associated with a file"));;

(*
 * Change the filename.
 *)
let save_as mod_info name =
   package_set_file mod_info name;
   save mod_info;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
