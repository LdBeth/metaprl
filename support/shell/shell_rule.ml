(*
 * Create an ediable rewrite object.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Lm_debug
open Lm_printf

open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermTy
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Opname

open Tactic_type

open Shell_sig

open Filter_base_type
open Filter_type
open Filter_util
open Filter_summary_type

(*
 * Show that the file is loading.
 *)
let () =
   show_loading "Loading Shell_rule"

let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "Display shell operations";
        debug_value = false
      }

(* unused
let debug_refine = load_debug "refine"
*)

type goal =
   GRule of term
 | GRewrite of term*term * msequent (*XXX: HACK: the third part is the cached mk_rw_goal *)

(*
 * This is the actual rule/rewrite object.
 *)
module Info = struct
   type info =
      { rule_params    : term_param list;
        rule_assums    : term list;
        rule_goal      : goal;
        rule_proof     : Package_info.proof proof_type;
        rule_ped       : Proof_edit.ped proof_type;
        rule_resources : (MLast.expr, term) resource_def;
        rule_name      : string
      }
end

(*
 * Make a rule/rewrite goal from the assumptions, and the info goal.
 *)
let mk_rw_goal assums redex contractum =
   let rw = mk_rewrite_hack (mk_xrewrite_term redex contractum) in
   let assums = List.map mk_rewrite_hack assums in
      if !debug_shell then begin
         eprintf "Shell_rule.mk_rw_goal: [... --> ] %a <--> %a%t"
            print_term redex print_term contractum eflush;
      end;
      mk_msequent rw assums

let mk_bare_goal assums = function
   GRule goal ->
      Tactic.create Tactic.null_sentinal (mk_msequent goal assums) (Mp_resource.find Mp_resource.top_bookmark)
 | GRewrite (_, _, mseq) ->
      Tactic.create Tactic.null_sentinal mseq (Mp_resource.find Mp_resource.top_bookmark)

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Commenting function.
 *)
(* unused
let comment _ _ t =
   t
*)


(*
 * The object has a package in scope.
 *)
let unit_term = mk_simple_term nil_opname []

let mk_addr addr =
   try List.map int_of_string addr with
      Failure _ ->
         raise (Failure "Invalid proof node address (contains non-numerical elements)")

class info w obj =
   let { Info.rule_params = params;
         Info.rule_assums = assums;
         Info.rule_proof = proof;
         Info.rule_ped = ped;
         Info.rule_goal = goal;
         Info.rule_resources = res;
         Info.rule_name = name } = obj in
 object (self)
    val window = w

    val mutable rule_params = params
    val mutable rule_assums = assums
    val mutable rule_goal = goal
    val mutable rule_proof = proof
    val mutable rule_ped = ped
    val rule_resources = res
    val rule_name = name

(*
 * Build an item from the object.
 *)
    method private to_item =
       (match rule_goal with
           GRule goal ->
              Filter_type.Rule {
                 Filter_type.rule_name = rule_name;
                 Filter_type.rule_params = rule_params;
                 Filter_type.rule_stmt = zip_mimplies rule_assums goal;
                 Filter_type.rule_proof = rule_proof;
                 Filter_type.rule_resources = rule_resources
              }
         | GRewrite (redex, contractum, _) when rule_params = [] && rule_assums = [] ->
              Filter_type.Rewrite {
                 rw_name = rule_name;
                 rw_redex = redex;
                 rw_contractum = contractum;
                 rw_proof = rule_proof;
                 rw_resources = rule_resources
              }
         | GRewrite (redex, contractum, _) ->
              Filter_type.CondRewrite {
                 crw_name = rule_name;
                 crw_params = rule_params;
                 crw_assums = rule_assums;
                 crw_redex = redex;
                 crw_contractum = contractum;
                 crw_proof = rule_proof;
                 crw_resources = rule_resources
              } : ('a, 'b, 'c,
    (MLast.ctyp, MLast.expr) resource_str,
    MLast.ctyp,
    MLast.expr,
    MLast.str_item) Filter_summary.summary_item)

    method private update_ped = rule_ped <- Primitive unit_term

    method edit_get_terms =
      match rule_goal with
         GRule goal ->
            goal :: rule_assums
       | GRewrite (_, _, mseq) ->
            let goal, assums = dest_msequent mseq in goal :: assums
    method edit_set_goal t =
       rule_goal <- GRule t;
       self#update_ped
    method edit_set_redex t =
       match rule_goal with
          GRule _ ->
             raise (RefineError ("Shell_rule.set_redex", StringError "can't set redex in rules"))
        | GRewrite (_, contractum, _) ->
             rule_goal <- GRewrite (t, contractum, mk_rw_goal rule_assums t contractum);
             self#update_ped
    method edit_set_contractum t =
       match rule_goal with
          GRule _ ->
             raise (RefineError ("Shell_rule.set_contractum", StringError "can't set contractum in rules"))
        | GRewrite (redex, _, _) ->
             rule_goal <- GRewrite (redex, t, mk_rw_goal rule_assums redex t);
             self#update_ped

    method edit_set_assumptions tl =
       rule_assums <- tl;
       self#update_ped
    method edit_set_params pl =
       rule_params <- pl;
       self#update_ped

    method edit_get_extract =
      match rule_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check primitive rules"))
       | Derived _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check noninteractive proofs"))
       | Incomplete ->
            raise (RefineError ("Shell_rule.check", StringError "proof is incomplete"))
       | Interactive ped ->
            try Proof_edit.refiner_extract_of_ped window ped with
               RefineError (name', err) ->
                  raise (RefineError (name, StringWrapError (name', err)))

    method private get_ped =
       match rule_ped with
          Primitive _
        | Derived _
        | Incomplete ->
             raise (RefineError ("Shell_rule.get_ped", StringError "proof is not interactive"))
        | Interactive ped ->
             ped

    method edit_undo addr =
       List.map string_of_int (Proof_edit.undo_ped self#get_ped (mk_addr addr))
    method edit_redo addr =
       List.map string_of_int (Proof_edit.redo_ped self#get_ped (mk_addr addr))
 end

class edit p a w (obj : Info.info) =
 (object (self)
   val pack = p
   val parse_arg = a
   inherit info w obj

   method edit_copy = ({< >} :> edit_object)
   method edit_check =
      match rule_ped with
         Primitive _ ->
            false, RefPrimitive
       | Derived _
       | Incomplete ->
            false, RefIncomplete (0, 0)
       | Interactive ped ->
            Proof_edit.check_ped window (Package_info.get_refiner pack) (make_opname [rule_name; Package_info.name pack]) ped
   method edit_save =
      let item = self#to_item in
         Package_info.set pack parse_arg item

   method edit_display addr _ =
      (* Convert to a term *)
      let addr = mk_addr addr in
      let { Info.rule_assums = assums; Info.rule_goal = goal; _ } = obj in
         match obj.Info.rule_ped with
            Primitive t ->
               let goal = mk_bare_goal assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Primitive goal)
          | Derived expr ->
               let goal = mk_bare_goal assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Derived (goal, expr))
          | Incomplete ->
               let goal = mk_bare_goal assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Incomplete goal)
          | Interactive ped ->
               Proof_edit.format window ped addr

   method edit_get_contents addr =
      if addr <> [] then
         raise (Invalid_argument "Shell_rule.edit_get_contents: only implemented for the root of the proof");
      let goal =
         match rule_goal with
            GRule goal -> MetaTheorem goal
          | GRewrite (redex, contractum, _) -> MetaIff(MetaTheorem redex, MetaTheorem contractum)
      in
         rule_name,
         Proof_edit.ped_status rule_ped,
         List.fold_right (fun x y -> MetaImplies (MetaTheorem x, y)) rule_assums goal,
         rule_params

   method! private get_ped =
      match rule_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rule.get_ped", StringError "Editing of primitive rules is not currently allowed"))
       | Derived _
       | Incomplete ->
            let goal, assums =
               match rule_goal with
                  GRule goal ->
                     goal, rule_assums
                | GRewrite (_, _, mseq) ->
                     dest_msequent mseq
            in
            let proof = Package_info.new_proof pack parse_arg rule_name assums goal in
            let ped = Package_info.ped_of_proof pack parse_arg proof (mk_msequent goal assums) in
               rule_proof <- Interactive proof;
               rule_ped <- Interactive ped;
               self#edit_save;
               ped
       | Interactive ped ->
            ped

   method edit_get_names addr _ = (* get all pending subgoals *)
	    List.map (fun l -> String.concat "/" (List.map string_of_int l))
      (Proof.find_all_subgoals (Proof_edit.proof_of_ped self#get_ped) (mk_addr addr))

   method edit_info addr =
      Proof_edit.edit_info_of_ped self#get_ped (mk_addr addr)

   method edit_check_addr addr =
      if addr <> [] then Proof_edit.check_addr_ped self#get_ped (mk_addr addr)

   method edit_interpret addr command =
      Proof_edit.interpret window self#get_ped (mk_addr addr) command

   method edit_find addr i =
      List.map string_of_int (Proof.find_subgoal (Proof_edit.proof_of_ped self#get_ped) (mk_addr addr) i)

   method edit_is_enabled addr name =
      match rule_ped with
         Primitive _ -> false
       | Derived _
       | Incomplete
       | Interactive _ ->
            Proof_edit.is_enabled_ped self#get_ped (mk_addr addr) name
 end : edit_object)

let create pack parse_arg window name =
   let obj =
      { Info.rule_assums = [];
        Info.rule_params = [];
        Info.rule_goal = GRewrite (unit_term, unit_term, mk_msequent unit_term []);
        Info.rule_proof = Incomplete;
        Info.rule_ped = Incomplete;
        Info.rule_resources = no_resources;
        Info.rule_name = name
      }
   in
      new edit pack parse_arg window obj

let ped_of_proof pack parse_arg goal = function
   Primitive proof ->
      Primitive proof
 | Derived expr ->
      Derived expr
 | Incomplete ->
      Incomplete
 | Interactive proof ->
      Interactive (Package_info.ped_of_proof pack parse_arg proof goal)

let view_rule pack parse_arg window
    { Filter_type.rule_name = name;
      Filter_type.rule_params = params;
      Filter_type.rule_stmt = stmt;
      Filter_type.rule_proof = proof;
      Filter_type.rule_resources = res
    } =
   let assums, goal = unzip_mfunction stmt in
   let assums = List.map (fun (_, _, t) -> t) assums in
   let obj =
      { Info.rule_assums = assums;
        Info.rule_params = params;
        Info.rule_goal = GRule goal;
        Info.rule_proof = proof;
        Info.rule_ped = ped_of_proof pack parse_arg (mk_msequent goal assums) proof;
        Info.rule_resources = res;
        Info.rule_name = name
      }
   in
      new edit pack parse_arg window obj

let view_rw pack parse_arg window
    { rw_name = name;
      rw_redex = redex;
      rw_contractum = contractum;
      rw_proof = proof;
      rw_resources = res;
    } =
   let obj =
      { Info.rule_assums = [];
        Info.rule_params = [];
        Info.rule_goal = GRewrite (redex, contractum, mk_rw_goal [] redex contractum);
        Info.rule_proof = proof;
        Info.rule_ped = ped_of_proof pack parse_arg (mk_rw_goal [] redex contractum) proof;
        Info.rule_resources = res;
        Info.rule_name = name;
      }
   in
      new edit pack parse_arg window obj

let view_crw pack parse_arg window
    { crw_name = name;
      crw_params = params;
      crw_assums = args;
      crw_redex = redex;
      crw_contractum = contractum;
      crw_proof = proof;
      crw_resources = res;
    } =
   let mseq = mk_rw_goal args redex contractum in
   let obj =
      { Info.rule_assums = args;
        Info.rule_params = params;
        Info.rule_goal = GRewrite (redex, contractum, mseq);
        Info.rule_proof = proof;
        Info.rule_ped = ped_of_proof pack parse_arg mseq proof;
        Info.rule_resources = res;
        Info.rule_name = name;
      }
   in
      new edit pack parse_arg window obj

let view_def pack parse_arg window ty_term def =
   let redex = term_of_ty ty_term in
   let contractum = def.term_def_value in
   let goal = mk_rw_goal [] redex contractum in
   let proof = Primitive (mk_xrewrite_term redex contractum) in
      new edit pack parse_arg window {
         Info.rule_assums = [];
         Info.rule_params = [];
         Info.rule_goal = GRewrite (redex, contractum, goal);
         Info.rule_proof = proof;
         Info.rule_ped = ped_of_proof pack parse_arg goal proof;
         Info.rule_resources = def.term_def_resources;
         Info.rule_name = def.term_def_name;
      }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
