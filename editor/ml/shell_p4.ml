(*
 * Define the additional grammar for the shell.
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

open Printf
open Lexing

open Longident
open Parsetree

open Lm_debug
open Lm_pervasives

open Pcaml

open Toploop

open Refiner.Refiner.Term
open Refiner.Refiner.RefineError

open Filter_ast
open Term_grammar
open Filter_grammar

open Tactic_type
open Tactic_type.Tacticals
open Shell_p4_sig
open Mp_version

(*
 * Ref cell for returning the tactic value.
 *)
let inline_tactic = ref None

let install_tactic tac =
   inline_tactic := Some tac

module ShellP4 =
struct
   (************************************************************************
    * STATE FUNCTIONS                                                      *
    ************************************************************************)

   (*
    * No notion of current state, since the toploop evaluates all
    * shell call relative to the Mp module.
    *)
   let current_state = ref (Shell_state.create ())

   let get_current_state () =
      !current_state

   let set_current_state state =
      current_state := state

   (************************************************************************
    * TOPLEVEL                                                             *
    ************************************************************************)

   let wrap_once state f lb =
      let x = Shell_state.wrap state f lb in
         Toploop.parse_toplevel_phrase := Shell_state.wrap state !Toploop.parse_toplevel_phrase;
         x

   (*
    * Wrap a file.
    * We don't need to modify the lexbuf.
    * Instead, we'll get the chars from the file.
    * Unfortunately, we don't close the file once we're done, because the
    * input hasn't been evaluated yet.
    *)
   let wrap_file state f lb =
      Shell_state.set_file state !Toploop.input_name;
      f lb

   (*
    * Wrap the toplevel.
    *)
   let _ =
      let wrapped = !Toploop.parse_toplevel_phrase in
      let motd lb =
         eprintf "\t%s\n%t" version eflush;
         Toploop.parse_toplevel_phrase := wrap_once !current_state wrapped;
         !Toploop.parse_toplevel_phrase lb
      in
         Toploop.parse_toplevel_phrase := motd

   (*
    * Wrap file usage.
    *)
   let _ =
      let wrapped = !Toploop.parse_use_file in
         Toploop.parse_use_file := wrap_file !current_state wrapped

   (************************************************************************
    * COMPILING TACTICS                                                    *
    ************************************************************************)

   (*
    * We create tactics through the toploop,
    * but it is delayed until the tactic is first evaluated.
    *)
   type delayed_tactic =
      Delay of MLast.expr
    | Tactic of tactic

   (*
    * Evaluate expressions with the toploop.
    *)
   let eval_str_item loc item =
      let pt_item = Ast2pt.str_item item [] in
          try
             if not (Toploop.execute_phrase false Format.std_formatter (Parsetree.Ptop_def pt_item)) then
                raise (RefineError ("eval_expr", StringError "evaluation failed"))
          with
             Typecore.Error (_, err) ->
                Typecore.report_error Format.std_formatter err;
                eflush stdout;
                raise (RefineError ("eval_expr", StringError "evaluation failed"))

   type ('a, 'b) once =
      OnceInitial of 'a
    | OnceFinal of 'b

   let eval_tactic_once tacv =
      match !tacv with
         OnceFinal tac ->
            tac
       | OnceInitial pt_item ->
            inline_tactic := None;
            try
               if Toploop.execute_phrase false Format.std_formatter (Parsetree.Ptop_def pt_item) then
                  match !inline_tactic with
                     Some tac ->
                        tacv := OnceFinal tac;
                        tac
                   | None ->
                        raise (RefineError ("eval_tactic", StringError "evaluation failed"))
               else
                  raise (RefineError ("eval_tactic", StringError "evaluation failed"))
            with
               Typecore.Error (_, err) ->
                  Typecore.report_error Format.std_formatter err;
                  eflush stdout;
                  raise (RefineError ("eval_tactic", StringError "evaluation failed"))

   let eval_tactic state =
      Shell_state.synchronize state (function expr ->
            let loc = 0, 0 in
            let expr = (<:expr< Shell_p4.install_tactic $expr$ >>) in
            let item = (<:str_item< $exp: expr$ >>) in
            let pt_item = Ast2pt.str_item item [] in
               eval_tactic_once (ref (OnceInitial pt_item)))

   let parse_string state =
      Shell_state.synchronize state (function str ->
          let instream = Stream.of_string str in
             Grammar.Entry.parse Pcaml.expr instream)

   let eval_expr state =
      Shell_state.synchronize state (function str ->
            let instream = Stream.of_string str in
            let expr = Grammar.Entry.parse Pcaml.expr instream in
            let loc = 0, 0 in
               eval_str_item loc <:str_item< $exp: expr$ >>)

   let eval_opens state =
      Shell_state.synchronize state (function opens ->
            let eval_open path =
               let loc = 0, 0 in
                  eval_str_item loc (<:str_item< open $path$ >>)
            in
               List.iter eval_open opens)

   (*
    * Build a delayed-evaluation tactic.
    *)
   let create_tactic state expr =
      let cell = ref (Delay expr) in
      funT (fun _ ->
         match !cell with
            Tactic tac ->
               tac
          | Delay expr ->
               let tac = eval_tactic state expr in
                  cell := Tactic tac;
                  tac)

   (************************************************************************
    * SHELL GRAMMAR                                                        *
    ************************************************************************)

   module Unit =
   struct
   end

   module Infix = MakeFilterGrammar (Unit)

   EXTEND
      GLOBAL: str_item;

      str_item:
         [[ "refine"; e = refine_item ->
             let e = <:expr< Mp.refine $e$ >> in
                <:str_item< $exp: e$ >>
          ]];

      refine_item:
         [[ e = expr ->
             Shell_state.set_tactic (Shell_state.get_text loc) e;
             e
          ]];
   END

   (*
    * Main function installs printers and include directories.
    * Then exits to pass control to the toploop.
    *)
   let main _ =
      let init () =
         let mplib =
            try Sys.getenv "MPLIB" with
               Not_found ->
                  raise (Invalid_argument "MPLIB environment variable in undefined")
         in
         let eval_include inc =
            let _ = Toploop.execute_phrase false Format.std_formatter (Ptop_dir ("directory", Pdir_string inc)) in
               ()
         in
            eval_include mplib;
            List.iter eval_include (Shell_state.get_includes ());
            let _ = Toploop.execute_phrase false Format.std_formatter
               (Ptop_dir ("install_printer", Pdir_ident (Ldot (Lident "Shell_state", "term_printer")))) in
            let _ = Toploop.execute_phrase false Format.std_formatter
               (Ptop_def [{ pstr_desc = Pstr_open (Lident "Mp"); pstr_loc = Location.none }]) in
            let _ = Tactic.main_loop () in
               ()
      in
         install_debug_printer Shell_state.print_short_term_fp;
         Printexc.catch init ()
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
