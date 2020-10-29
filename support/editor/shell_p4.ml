(*
 * Define the additional grammar for the shell.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
open Longident
open Parsetree

open Lm_printf
open Lm_thread

open Pcaml
open Filter_util

open Basic_tactics

(*
 * Ref cell for returning the tactic value.
 *)
let inline_tactic =
   State.private_val "Shell_p4.inline_tactic" (ref None) (fun x -> ref !x)

let install_tactic tac =
   State.write inline_tactic (fun x -> x := Some tac)

module ShellP4 =
struct
   (************************************************************************
    * TOPLEVEL                                                             *
    ************************************************************************)

   (*
    * Wrap a file.
    * We don't need to modify the lexbuf.
    * Instead, we'll get the chars from the file.
    * Unfortunately, we don't close the file once we're done, because the
    * input hasn't been evaluated yet.
    *)
   let wrap_file f lb =
      Shell_state.set_file !Toploop.input_name;
      f lb

   (*
    * Wrap the toplevel and file usage.
    *
    * Note: the reason we open modules here is that it needs to be done
    * _after_ the toploop module initializes its environment, and _before_
    * the user gets a chance to input anything.
    *)
   (*
   let _ =
      let wrapped = !Toploop.parse_toplevel_phrase in
      let open_module m =
         if
            try
               not (Toploop.execute_phrase true Format.std_formatter
                     (Ptop_def [{ pstr_desc = Pstr_open (Lident m); pstr_loc = Location.none }]))
            with _ ->
               true
         then
            eprintf "Shell_p4.main: opening module %s failed\n%t" m eflush
      in
      let motd lb =
         eprintf "\t%s\n%t" Mp_version.version eflush;
         List.iter open_module [ "Lm_printf"; "Basic_tactics"; "Mp"; "Shell_command" ];
         Toploop.parse_toplevel_phrase := Shell_state.wrap wrapped;
         !Toploop.parse_toplevel_phrase lb
      in
         Toploop.parse_toplevel_phrase := motd;
         Toploop.parse_use_file := wrap_file !Toploop.parse_use_file
    *)

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
                flush stdout;
                raise (RefineError ("eval_expr", StringError "evaluation failed"))

   type ('a, 'b) once =
      OnceInitial of 'a
    | OnceFinal of 'b

   let eval_tactic_once tacv =
      match !tacv with
         OnceFinal tac ->
            tac
       | OnceInitial pt_item ->
            State.write inline_tactic (fun x -> x := None);
            try
               if Toploop.execute_phrase false Format.std_formatter (Parsetree.Ptop_def pt_item) then
                  match State.read inline_tactic (fun x -> !x) with
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
                  flush stdout;
                  raise (RefineError ("eval_tactic", StringError "evaluation failed"))

   let eval_tactic e =
      Shell_state.synchronize (fun expr ->
            let _loc = dummy_loc in
            let expr = (<:expr< Shell_p4.install_tactic $expr$ >>) in
            let item = (<:str_item< $exp: expr$ >>) in
            let pt_item = Ast2pt.str_item item [] in
               eval_tactic_once (ref (OnceInitial pt_item))) e

   let parse_string s =
      Shell_state.synchronize (fun str ->
          let instream = Stream.of_string str in
             grammar_parse Pcaml.expr instream) s

   let eval_expr s =
      Shell_state.synchronize (fun str ->
            let instream = Stream.of_string str in
            let expr = grammar_parse Pcaml.expr instream in
            let _loc = dummy_loc in
               eval_str_item _loc <:str_item< $exp: expr$ >>) s

   let eval_top s =
      Shell_state.synchronize (fun str ->
            let instream = Stream.of_string str in
            let expr = grammar_parse Pcaml.expr instream in
            let _loc = dummy_loc in
               eval_str_item _loc <:str_item< $exp: expr$ >>) s

   let eval_opens s =
      Shell_state.synchronize (fun opens ->
            let eval_open path =
               let _loc = dummy_loc in
                  eval_str_item _loc (<:str_item< open $path$ >>)
            in
               List.iter eval_open opens) s

   (*
    * Build a delayed-evaluation tactic.
    *)
   let create_tactic expr =
      let cell = ref (Delay expr) in
         funT (fun _ ->
               match !cell with
                  Tactic tac ->
                     tac
                | Delay expr ->
                     let tac = eval_tactic expr in
                        cell := Tactic tac;
                        tac)

   (************************************************************************
    * SHELL GRAMMAR                                                        *
    ************************************************************************)

   EXTEND
      GLOBAL: str_item;

      str_item:
         [[ "refine"; e = refine_item ->
             let e = <:expr< Mp.refine $e$ >> in
                <:str_item< $exp: e$ >>
          ]];

      refine_item:
         [[ e = expr ->
             Shell_state.set_tactic (Shell_state.get_text _loc) e;
             e
          ]];
   END

   (*
    * Main function installs printers and include directories.
    * Then exits to pass control to the toploop.
    *)
   let main _ =
      install_debug_printer Shell_state.print_term_fp;
      let eval_include inc =
         if not (Toploop.execute_phrase false Format.std_formatter (Ptop_dir ("directory", Pdir_string inc))) then
            invalid_arg ("Shell_p4.main: adding include directory \"" ^ inc ^ "\" failed")
      in
         eval_include (Setup.lib());
         List.iter eval_include (Shell_state.get_includes ());
         if not
            (Toploop.execute_phrase false Format.std_formatter
               (Ptop_dir ("install_printer", Pdir_ident (Ldot (Lident "Shell_state", "term_printer")))))
         then
            invalid_arg "Shell_p4.main: installing term printer failed";
         Tactic_type.Tactic.main_loop ();

         (* Ignore initialization errors *)
         try Shell_command.init () with
            _ ->
               ()

   let version = Mp_version.version

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
