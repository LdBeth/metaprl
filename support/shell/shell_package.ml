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

extends Shell_sig
extends Package_info
extends Summary

open Printf

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError

open Dform_print

open Filter_ocaml
open Filter_type
open Filter_summary_type

open Tactic_type

open Summary

open Shell_sig
open Package_info

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A window is either a text window or an HTML window.
 *)
type proof_window =
   { pw_port : Mux_channel.session;
     pw_base : dform_mode_base;
     pw_menu : Display_term.t
   }

type text_window =
   { df_base : dform_mode_base;
     df_mode : string;
     df_width : int
   }

type window =
   ProofWindow of proof_window
 | TextWindow of text_window
 | TexWindow of text_window

(************************************************************************
 * WINDOWS                                                              *
 ************************************************************************)

(*
 * Create a new window.
 *)
let create_text_window base mode =
   TextWindow { df_base = base;
                df_mode = mode;
                df_width = 80
   }

let create_tex_window base =
   TexWindow { df_base = base;
               df_mode = "tex";
               df_width = 80
   }

let create_proof_window port dfbase =
   let menu = Display_term.create_term port dfbase in
   let window =
      { pw_port = port;
        pw_base = dfbase;
        pw_menu = menu
      }
   in
      ProofWindow window

(*
 * Create a window from the description.
 *)
let create_window = function
   DisplayText (base, mode) ->
      TextWindow { df_base = base; df_mode = mode; df_width = 80 }
 | DisplayTex base ->
      TexWindow { df_base = base; df_mode = "tex"; df_width = 70 }
 | DisplayGraphical (port, base) ->
      let menu = Display_term.create_term port base in
         ProofWindow { pw_port = port; pw_base = base; pw_menu = menu }

(*
 * Copy the window.
 *)
let new_window = function
   ProofWindow { pw_port = port; pw_base = base } ->
      ProofWindow { pw_port = port; pw_base = base; pw_menu = Display_term.create_term port base }
 | (TextWindow _ | TexWindow _) as window ->
      window

(*
 * Display a term in the window.
 *)
let display_term pack window term =
   match window with
      TextWindow { df_base = base; df_mode = mode; df_width = width } ->
         let df = get_mode_base base mode in
         let buf = Rformat.new_buffer () in
            Dform.format_term df buf term;
            Rformat.print_to_channel width buf stdout;
            flush stdout
    | TexWindow { df_base = base; df_mode = mode; df_width = width } ->
         let df = get_mode_base base mode in
         let buf = Rformat.new_buffer () in
         let out = Shell_tex.open_file () in
            Dform.format_term df buf term;
            Rformat.print_to_tex width buf out;
            Shell_tex.close_file out
    | ProofWindow { pw_menu = menu } ->
         Display_term.set_dir menu ("/" ^ Package.name pack);
         Display_term.set menu term

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Standard FilterOCaml module.
 *)
open Filter_summary
module FilterOCaml = Filter_ocaml.FilterOCaml (Refiner.Refiner)
module FilterSummaryTerm = FilterSummaryTerm (Refiner.Refiner)

open FilterOCaml
open FilterSummaryTerm

(*
 * This comment function removes expressions.
 *)
let comment _ _ t =
   t

let identity x       = x
let term_of_expr     = term_of_expr [] comment
let term_of_type     = term_of_type comment
let term_of_sig_item = term_of_sig_item comment
let term_of_str_item = term_of_str_item [] comment
let term_of_resource = FilterOCaml.term_of_resource_sig resource_op

let convert_intf =
   let null_term    = mk_xstring_term "..." in
      { term_f      = identity;
        meta_term_f = term_of_meta_term;
        proof_f     = (fun _ _ -> null_term);
        resource_f  = term_of_resource;
        ctyp_f      = term_of_type;
        expr_f      = term_of_expr;
        item_f      = term_of_sig_item
      }

let convert_impl =
   let convert_proof _ = function
      Primitive t ->
         <:con<status_primitive{$t$}>>
    | Derived expr ->
         <<status_asserted>>
    | Incomplete ->
         <<status_partial>>
    | Interactive proof ->
         let status =
            match Package.status_of_proof proof with
               Proof.StatusBad ->
                  <<status_bad>>
             | Proof.StatusPartial ->
                  <<status_partial>>
             | Proof.StatusIncomplete ->
                  <<status_asserted>>
             | Proof.StatusComplete ->
                  <<status_complete>>
         in
         let rcount, ncount = Package.node_count_of_proof proof in
            <:con<status_interactive[$int:rcount$, $int:ncount$]{$status$}>>
   in
      { term_f      = identity;
        meta_term_f = term_of_meta_term;
        proof_f     = convert_proof;
        resource_f  = term_of_expr;
        ctyp_f      = term_of_type;
        expr_f      = term_of_expr;
        item_f      = term_of_str_item
      }

(*
 * Display the entire package.
 *)
let term_of_interface pack filter parse_arg =
   let tl = term_list convert_intf (Filter_summary.filter filter (Package.sig_info pack parse_arg)) in
      mk_interface_term tl

(*
 * Display the entire package.
 *)
let term_of_implementation pack filter parse_arg =
   let tl = term_list convert_impl (Filter_summary.filter filter (Package.info pack parse_arg)) in
      mk_implementation_term tl

(*
 * Filter the entries for ls.
 *)
let is_not_summary_item = function
   SummaryItem _ | Improve _ | Resource _ -> false
 | _ -> true

let is_rewrite_item = function
   Rewrite _ | CondRewrite _ | MLRewrite _ | Definition _ -> true
 | _ -> false

let is_rule_item = function
   Rule _ | MLAxiom  _ -> true
 | _ -> false

let is_formal_item = function
   Rewrite _ | CondRewrite _ | MLRewrite _ | Rule _ | MLAxiom  _ | Definition _ | Parent _ | Opname _ ->
      true
 | SummaryItem _ | Improve _ | Resource _ | InputForm _ | Comment _ | MagicBlock _
 | ToploopItem _ | Infix _ | Prec _ | DForm _ | Module _ | Id _ | PrecRel _ ->
      false

let is_unjustified_item item =
   let proof =
      match item with
         Rewrite { rw_proof = proof }
       | CondRewrite { crw_proof = proof }
       | Rule { rule_proof = proof } ->
            proof
       | _ ->
            Primitive xnil_term
   in
      match proof with
         Primitive _
       | Derived _ ->
            false
       | Incomplete ->
            true
       | Interactive proof ->
            match Package.status_of_proof proof with
               Proof.StatusComplete ->
                  false
             | _ ->
                  true

(*
 * Conjoin all the predicates.
 *)
let compile_ls_predicate = function
   [] ->
      (fun _ -> true)
 | predicate ->
      (fun (item, _) -> Lm_list_util.allp (fun pred -> pred item) predicate)

let rec mk_ls_filter predicate = function
   LsAll :: tl ->
      mk_ls_filter [] tl
 | LsRewrites :: tl ->
      mk_ls_filter (is_rewrite_item :: predicate) tl
 | LsRules :: tl ->
      mk_ls_filter (is_rule_item :: predicate) tl
 | LsUnjustified :: tl ->
      mk_ls_filter (is_unjustified_item :: predicate) tl
 | LsFormal :: tl ->
      mk_ls_filter (is_formal_item :: predicate) tl
 | LsNoSummaryItems :: tl ->
      mk_ls_filter (is_not_summary_item :: predicate) tl
 | [] ->
      compile_ls_predicate predicate

(************************************************************************
 * SHELL INTERFACE                                                      *
 ************************************************************************)

(*
 * Error handler.
 *)
let raise_edit_error s =
   raise (RefineError ("Shell_package", StringError s))

(*
 * Build the shell interface.
 *)
let rec edit pack_info parse_arg window =
   let edit_display options =
      display_term pack_info window (term_of_implementation pack_info (mk_ls_filter [is_not_summary_item] options) parse_arg)
   in
   let edit_copy () =
      edit pack_info parse_arg (new_window window)
   in
   let edit_set_goal goal =
      raise_edit_error "no goal"
   in
   let edit_set_redex redex =
      raise_edit_error "no redex"
   in
   let edit_set_contractum contractum =
      raise_edit_error "no contractum"
   in
   let edit_set_assumptions assum =
      raise_edit_error "no assumptions"
   in
   let edit_set_params params =
      raise_edit_error "no params"
   in
   let edit_save () =
      Package.save pack_info
   in
   let edit_check _ =
      raise_edit_error "check the entire package?"
   in
   let edit_expand _ =
      raise_edit_error "expand all the proofs in the package? Use expand_all."
   in
   let edit_root () =
      ()
   in
   let edit_up i =
      ()
   in
   let edit_down i =
      ()
   in
   let edit_undo () =
      ()
   in
   let edit_redo () =
      ()
   in
   let edit_addr addr =
      ()
   in
   let edit_info () =
      raise_edit_error "no info for the package"
   in
   let edit_refine _ _ _ =
      raise_edit_error "can't refine the package"
   in
   let edit_interpret command =
      raise_edit_error "this is not a proof"
   in
   let edit_get_contents () =
      raise_edit_error "can only retrieve contents of an individual item, not of a package"
   in
      { edit_display = edit_display;
        edit_get_contents = edit_get_contents;
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

let create pack parse_arg window =
   let window = create_window window in
      edit pack parse_arg window

let view pack parse_arg window =
   edit pack parse_arg (create_window window)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
