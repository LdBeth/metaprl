(*
 * Create an ediable rewrite object.
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

include Shell_type
include Package_info
include Package_df
include Proof_type

open Printf
open Mp_debug

open Rformat
open Dform
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Opname

open Filter_type
open Filter_summary
open Filter_cache
open Filter_ocaml

open Tacticals
open Tactic_cache
open Shell_type
open Package_info
open Proof_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Shell_rewrite%t" eflush

(*
 * This is the actual rewrite object.
 *)
type rw =
   { mutable rw_params : term param list;
     mutable rw_assums : term list;
     mutable rw_redex : term;
     mutable rw_contractum : term;
     mutable rw_proof : Package.proof proof_type;
     mutable rw_ped : Proof_edit.t proof_type;
     mutable rw_resources : MLast.expr resource_def
   }

(*
 * Make a rewrite goal from the assumptions,
 * and the rewrite.
 *)
let seq = << sequent ['ext] { 'H >- 'rw } >>

let mk_goal redex contractum =
   let rw = TermMan.replace_goal seq (mk_xrewrite_term redex contractum) in
      mk_msequent rw []

let mk_cond_goal assums redex contractum =
   let rw = replace_goal seq (mk_xrewrite_term redex contractum) in
   let assums = List.map (replace_goal seq) assums in
      mk_msequent rw assums

let mk_rw_goal assums redex contractum =
   if assums = [] then
      mk_goal redex contractum
   else
      mk_cond_goal assums redex contractum

let mk_goal sentinal arg assums redex contractum =
   let { ref_label = label; ref_fcache = cache; ref_args = args } = arg in
      Sequent.create sentinal label (mk_rw_goal assums redex contractum) cache args

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Commenting function.
 *)
let comment _ _ t =
   t

(*
 * Format the tactic text.
 *)
let format_tac db buf arg =
   let seq = Sequent.msequent arg in
   let rw, hyps = dest_msequent seq in
   let format_hyp hyp =
      format_term db buf hyp;
      format_newline buf
   in
      format_string buf "Hyps:";
      format_newline buf;
      List.iter format_hyp hyps;
      format_string buf "Goal: ";
      format_term db buf rw;
      format_newline buf

(*
 * A primtive rewrite.
 *)
let format_rewrite_aux s db buf arg sentinal params assums redex contractum =
   let tac = Refine_exn.print db (mk_goal sentinal arg assums redex) contractum in
      format_tac db buf tac;
      format_newline buf;
      format_string buf s;
      format_newline buf

let format_prim_rewrite = format_rewrite_aux "Primitive"
let format_incomplete_rewrite = format_rewrite_aux "Incomplete"

let format_cond_rewrite db buf arg sentinal params assums redex contractum expr =
   let tac = Refine_exn.print db (mk_goal sentinal arg assums redex) contractum in
      format_tac db buf tac;
      format_newline buf;
      format_string buf "Derived> ";
      format_term db buf (term_of_expr [] comment expr);
      format_newline buf

(*
 * Build an item from the object.
 *)
let item_of_obj pack name
    { rw_params = params;
      rw_assums = assums;
      rw_redex = redex;
      rw_contractum = contractum;
      rw_proof = proof;
      rw_resources = res
    } =
   if params = [] & assums = [] then
      Filter_type.Rewrite (**)
         { Filter_type.rw_name = name;
           Filter_type.rw_redex = redex;
           Filter_type.rw_contractum = contractum;
           Filter_type.rw_proof = proof;
           Filter_type.rw_resources = res
         }
   else
      Filter_type.CondRewrite (**)
         { Filter_type.crw_name = name;
           Filter_type.crw_params = params;
           Filter_type.crw_args = assums;
           Filter_type.crw_redex = redex;
           Filter_type.crw_contractum = contractum;
           Filter_type.crw_proof = proof;
           Filter_type.crw_resources = res
         }

(*
 * The object has a package in scope.
 *)
let unit_term = mk_simple_term nil_opname []

let edit pack sentinal arg name obj =
   let update_ped () =
      obj.rw_ped <- Primitive unit_term
   in
   let save_ped () =
      let item = item_of_obj pack name obj in
         Package.set pack item
   in
   let edit_format db buf =
      (* Convert to a term *)
      let { rw_params = params;
            rw_assums = assums;
            rw_redex = redex;
            rw_contractum = contractum;
            rw_ped = ped
          } = obj
      in
         match ped with
            Primitive t ->
               format_prim_rewrite db buf arg sentinal params assums redex contractum
          | Derived expr ->
               format_cond_rewrite db buf arg sentinal params assums redex contractum expr
          | Incomplete ->
               format_incomplete_rewrite db buf arg sentinal params assums redex contractum
          | Interactive ped ->
               Proof_edit.format db buf ped
   in
   let edit_set_goal t =
      raise (Failure "Shell_rewrite.edit_set_goal: bogus edit")
   in
   let edit_set_redex t =
      obj.rw_redex <- t;
      update_ped ()
   in
   let edit_set_contractum t =
      obj.rw_contractum <- t;
      update_ped ()
   in
   let edit_set_assumptions tl =
      obj.rw_assums <- tl;
      update_ped ()
   in
   let edit_set_params pl =
      obj.rw_params <- pl;
      update_ped ()
   in
   let edit_save () =
      match obj.rw_ped with
         Interactive ped ->
            save_ped ()
       | Incomplete
       | Primitive _
       | Derived _ ->
            ()
   in
   let edit_check () =
      match obj.rw_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rewrite.check", StringError "can't check primitive rules"))
       | Derived _ ->
            raise (RefineError ("Shell_rewrite.check", StringError "can't check noninteractive proofs"))
       | Incomplete ->
            raise (RefineError ("Shell_rewrite.check", StringError "proof is incomplete"))
       | Interactive ped ->
            try Proof_edit.check_ped ped with
               RefineError (name', err) ->
                  raise (RefineError (name, GoalError (name', err)))
   in
   let get_ped obj =
      match obj.rw_ped with
         Primitive _
       | Derived _
       | Incomplete ->
            raise (RefineError ("Shell_rewrite.get_ped", StringError "proof is not interactive"))
       | Interactive ped ->
            ped
   in
   let edit_expand df =
      Proof_edit.expand_ped df (get_ped obj)
   in
   let edit_root () =
      Proof_edit.root_ped (get_ped obj)
   in
   let edit_up i =
      Proof_edit.up_ped (get_ped obj) i
   in
   let edit_down i =
      Proof_edit.down_ped (get_ped obj) i
   in
   let edit_undo () =
      Proof_edit.undo_ped (get_ped obj)
   in
   let edit_nop () =
      Proof_edit.nop_ped (get_ped obj)
   in
   let edit_fold () =
      Proof_edit.fold_ped (get_ped obj)
   in
   let edit_fold_all () =
      Proof_edit.fold_all_ped (get_ped obj)
   in
   let edit_kreitz () =
      Proof_edit.kreitz_ped (get_ped obj)
   in
   let get_ped () =
      match obj.rw_ped with
         Primitive _
       | Derived _
       | Incomplete ->
            (* Convert to a ped *)
            let { rw_params = params;
                  rw_assums = assums;
                  rw_redex = redex;
                  rw_contractum = contractum
                } = obj
            in
            let assums' = List.map (replace_goal seq) assums in
            let goal' = replace_goal seq (mk_xrewrite_term redex contractum) in
            let proof = Package.new_proof pack name assums goal' in
            let ped = Package.ped_of_proof pack proof in
               obj.rw_proof <- Interactive proof;
               obj.rw_ped <- Interactive ped;
               Proof_edit.set_params ped params;
               save_ped ();
               ped
       | Interactive ped ->
            ped
   in
   let edit_goal () =
      Proof_edit.ped_arg (get_ped ())
   in
   let edit_addr addr =
      Proof_edit.addr_ped (get_ped ()) addr
   in
   let edit_tactic () =
      Proof_edit.ped_tactic (get_ped ())
   in
   let edit_children () =
      Proof_edit.ped_children (get_ped ())
   in
   let edit_extras () =
      Proof_edit.ped_extras (get_ped ())
   in
   let edit_refine text ast tac =
      Proof_edit.refine_ped (get_ped ()) text ast tac
   in
      { edit_format = edit_format;
        edit_set_goal = edit_set_goal;
        edit_set_redex = edit_set_redex;
        edit_set_contractum = edit_set_contractum;
        edit_set_assumptions = edit_set_assumptions;
        edit_set_params = edit_set_params;
        edit_save = edit_save;
        edit_check = edit_check;
        edit_expand = edit_expand;
        edit_root = edit_root;
        edit_up = edit_up;
        edit_down = edit_down;
        edit_addr = edit_addr;
        edit_goal = edit_goal;
        edit_tactic = edit_tactic;
        edit_children = edit_children;
        edit_extras = edit_extras;
        edit_refine = edit_refine;
        edit_undo = edit_undo;
        edit_fold = edit_fold;
        edit_fold_all = edit_fold_all;
        edit_kreitz = edit_kreitz
      }

let create pack name =
   let proof = Package.new_proof pack name [] unit_term in
   let ped = Package.ped_of_proof pack proof in
   let rw =
      { Filter_type.rw_name = name;
        Filter_type.rw_redex = unit_term;
        Filter_type.rw_contractum = unit_term;
        Filter_type.rw_proof = Interactive proof;
        Filter_type.rw_resources = []
      }
   in
   let obj =
      { rw_assums = [];
        rw_params = [];
        rw_redex = unit_term;
        rw_contractum = unit_term;
        rw_proof = Interactive proof;
        rw_ped = Interactive ped;
        rw_resources = []
      }
   in
   let sentinal = Package.sentinal pack in
   let arg = Package.argument pack in
      Package.set pack (Filter_type.Rewrite rw);
      edit pack sentinal arg name obj

let ped_of_proof pack = function
   Primitive proof ->
      Primitive proof
 | Derived expr ->
      Derived expr
 | Incomplete ->
      Incomplete
 | Interactive proof ->
      Interactive (Package.ped_of_proof pack proof)

let view_rw pack
    { Filter_type.rw_name = name;
      Filter_type.rw_redex = redex;
      Filter_type.rw_contractum = contractum;
      Filter_type.rw_proof = proof;
      Filter_type.rw_resources = res
    } =
   let obj =
      { rw_assums = [];
        rw_params = [];
        rw_redex = redex;
        rw_contractum = contractum;
        rw_proof = proof;
        rw_ped = ped_of_proof pack proof;
        rw_resources = res
      }
   in
   let sentinal = Package.sentinal_object pack name in
   let arg = Package.argument pack in
      edit pack sentinal arg name obj

let view_crw pack
    { crw_name = name;
      crw_params = params;
      crw_args = args;
      crw_redex = redex;
      crw_contractum = contractum;
      crw_proof = proof;
      crw_resources = res
    } =
   let obj =
      { rw_assums = args;
        rw_params = params;
        rw_redex = redex;
        rw_contractum = contractum;
        rw_proof = proof;
        rw_ped = ped_of_proof pack proof;
        rw_resources = res
      }
   in
   let sentinal = Package.sentinal_object pack name in
   let arg = Package.argument pack in
      edit pack sentinal arg name obj

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
