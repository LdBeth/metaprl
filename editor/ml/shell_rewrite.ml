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

include Shell_sig
include Package_info

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

open Tactic_type
open Tactic_type.Tacticals

open Shell_sig
open Package_sig
open Package_info

open Filter_type
open Filter_summary
open Filter_cache
open Filter_ocaml

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Shell_rewrite%t"

let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "Display shell operations";
        debug_value = false
      }

(*
 * This is the actual rewrite object.
 *)
type rw =
   { mutable rw_params : term param list;
     mutable rw_assums : term list;
     mutable rw_redex : term;
     mutable rw_contractum : term;
     mutable rw_proof : Package.proof proof_type;
     mutable rw_ped : Proof_edit.ped proof_type;
     mutable rw_resources : MLast.expr resource_def
   }

(*
 * Make a rewrite goal from the assumptions,
 * and the rewrite.
 * HACK!!! This is here only because we do not do rewrites (esp.
 * the conditional ones) properly. Once that is fixed,
 * this code should go away (and, for that matter, shell_rule
 * and shell_rewrite should probably be eventually merged).
 *)
include Base_trivial
let seq = << sequent [squash] { 'H >- 'rw } >>

let mk_rw_goal assums redex contractum =
   let rw = replace_goal seq (mk_xrewrite_term redex contractum) in
   let assums = List.map (replace_goal seq) assums in
      if !debug_shell then begin
         eprintf "Shell_rewrite.mk_rw_goal: [... --> ] %a <--> %a%t"
            print_term redex print_term contractum eflush;
      end;
      mk_msequent rw assums

let mk_goal sentinal arg assums redex contractum =
   let { ref_label = label; ref_args = args } = arg in
      Tactic_type.Tactic.create sentinal label (mk_rw_goal assums redex contractum) args

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Commenting function.
 *)
let comment _ _ t =
   t

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

let rec edit pack parse_arg sentinal arg name window obj =
   (* Copy the edit object *)
   let edit_copy () =
      let { rw_params = params;
            rw_assums = assums;
            rw_redex = redex;
            rw_contractum = contractum;
            rw_proof = proof;
            rw_ped = ped;
            rw_resources = resources
          } = obj
      in
      let obj =
         { rw_params = params;
           rw_assums = assums;
           rw_redex = redex;
           rw_contractum = contractum;
           rw_proof = proof;
           rw_ped = ped;
           rw_resources = resources
         }
      in
         edit pack parse_arg sentinal arg name (Proof_edit.new_window window) obj
   in
   let update_ped () =
      obj.rw_ped <- Primitive unit_term
   in
   let save_ped () =
      let item = item_of_obj pack name obj in
         Package.set pack parse_arg item
   in
   let edit_display _ =
      (* Convert to a term *)
      let { rw_assums = assums;
            rw_redex = redex;
            rw_contractum = contractum;
            rw_ped = ped
          } = obj
      in
         match ped with
            Primitive t ->
               let goal = mk_goal sentinal arg assums redex contractum in
                  Proof_edit.format_incomplete window (Proof_edit.Primitive goal)
          | Derived expr ->
               let goal = mk_goal sentinal arg assums redex contractum in
                  Proof_edit.format_incomplete window (Proof_edit.Derived (goal, expr))
          | Incomplete ->
               let goal = mk_goal sentinal arg assums redex contractum in
                  Proof_edit.format_incomplete window (Proof_edit.Incomplete goal)
          | Interactive ped ->
               Proof_edit.format window ped
   in
   let edit_set_goal t =
      raise (Failure "Shell_rewrite.edit_set_goal: use set_redex or set_contractum")
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
   let edit_redo () =
      Proof_edit.redo_ped (get_ped obj)
   in
   let edit_nop () =
      Proof_edit.nop_ped (get_ped obj)
   in
   let get_ped () =
      match obj.rw_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rewrite.get_ped", StringError "Editing of primitive rewrites is not currently allowed"))
       | Derived _
       | Incomplete ->
            (* Convert to a ped *)
            let { rw_params = params;
                  rw_assums = assums;
                  rw_redex = redex;
                  rw_contractum = contractum
                } = obj
            in
            let mseq = mk_rw_goal assums redex contractum in
            let goal', assums' = dest_msequent mseq in
            let proof = Package.new_proof pack parse_arg name assums' goal' in
            let ped = Package.ped_of_proof pack parse_arg proof mseq in
               obj.rw_proof <- Interactive proof;
               obj.rw_ped <- Interactive ped;
               Proof_edit.set_params ped params;
               save_ped ();
               ped
       | Interactive ped ->
            ped
   in
   let edit_info () =
      Proof_edit.edit_info_of_ped (get_ped ())
   in
   let edit_addr addr =
      Proof_edit.addr_ped (get_ped ()) addr
   in
   let edit_refine text ast tac =
      Proof_edit.refine_ped (get_ped ()) text ast tac
   in
   let edit_interpret command =
      Proof_edit.interpret (get_ped ()) command
   in
      { edit_display = edit_display;
        edit_copy = edit_copy;
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
        edit_info = edit_info;
        edit_refine = edit_refine;
        edit_undo = edit_undo;
        edit_redo = edit_redo;
        edit_interpret = edit_interpret
      }

let create_window = function
   DisplayText (base, mode) ->
      Proof_edit.create_text_window base mode
 | DisplayTex base ->
      Proof_edit.create_tex_window base
 | DisplayGraphical (port, base) ->
      Proof_edit.create_proof_window port base

let create pack parse_arg window name =
   let proof = Package.new_proof pack parse_arg name [] unit_term in
   let ped = Package.ped_of_proof pack parse_arg proof (mk_msequent unit_term []) in
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
   let arg = Package.argument pack parse_arg name in
      Package.set pack parse_arg (Filter_type.Rewrite rw);
      edit pack parse_arg sentinal arg name (create_window window) obj

let ped_of_proof pack parse_arg goal = function
   Primitive proof ->
      Primitive proof
 | Derived expr ->
      Derived expr
 | Incomplete ->
      Incomplete
 | Interactive proof ->
      Interactive (Package.ped_of_proof pack parse_arg proof goal)

let view_rw pack parse_arg window
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
        rw_ped = ped_of_proof pack parse_arg (mk_rw_goal [] redex contractum) proof;
        rw_resources = res
      }
   in
   let sentinal = Package.sentinal_object pack name in
   let arg = Package.argument pack parse_arg name in
      edit pack parse_arg sentinal arg name (create_window window) obj

let view_crw pack parse_arg window
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
        rw_ped = ped_of_proof pack parse_arg (mk_rw_goal args redex contractum) proof;
        rw_resources = res
      }
   in
   let sentinal = Package.sentinal_object pack name in
   let arg = Package.argument pack parse_arg name in
      edit pack parse_arg sentinal arg name (create_window window) obj

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
