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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

extends Mptop

open Printf
open Lexing

open Mp_debug
open Mp_pervasives

open Pcaml
open MLast

open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermAddr
open Refiner.Refiner.RefineError
open Mp_resource
open Rformat

open Filter_ast
open Term_grammar
open Filter_grammar

open Mptop
open Mp_version

open Shell_p4_sig

module ShellP4 =
struct
   let _ =
      show_loading "Loading Shell_mp%t"

   (************************************************************************
    * STATE FUNCTIONS                                                      *
    ************************************************************************)

   (*
    * State for evaluating toploop expressions.
    *)
   let current_state = ref (Shell_state.create ())

   let get_current_state () =
      !current_state

   let set_current_state state =
      current_state := state

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
             let e = <:expr< $lid:"refine"$ $e$ >> in
                <:str_item< $exp: e$ >>
          ]];

      refine_item:
         [[ e = expr ->
             Shell_state.set_tactic (Shell_state.get_text loc) e;
             e
          ]];
   END

   (************************************************************************
    * COMPILING TACTICS                                                    *
    ************************************************************************)

   (*
    * Evaluate a tactic through the toploop resource.
    *)
   let eval_tactic state =
      Shell_state.synchronize state (function expr ->
          match expr_of_ocaml_expr (Shell_state.get_toploop state) expr with
             TacticExpr tac ->
                tac
           | _ ->
               raise (RefineError ("eval_tactic", StringError "expression is not a tactic")))

   let parse_string state =
      Shell_state.synchronize state (function str ->
          let instream = Stream.of_string str in
             Grammar.Entry.parse Pcaml.expr instream)

   let eval_expr state =
      Shell_state.synchronize state (function str ->
         let instream = Stream.of_string str in
         let expr = Grammar.Entry.parse Pcaml.expr instream in
         let _ = expr_of_ocaml_expr (Shell_state.get_toploop state) expr in
            ())

   (************************************************************************
    * TOPLOOP                                                              *
    ************************************************************************)

  let string_type = function
      UnitExpr _ ->        ": unit"
    | BoolExpr _ ->        ": bool"
    | IntExpr _ ->         ": int"
    | AddressExpr _ ->     ": address"
    | StringExpr _ ->      ": string"
    | TermExpr _ ->        ": term"
    | TacticExpr _ ->      ": tactic"
    | ConvExpr _ ->        ": conv"
    | ListExpr _ ->        ": list"
    | TupleExpr _ ->       ": tuple"
    | FunExpr _ ->         ": expr -> expr"
    | UnitFunExpr _ ->     ": unit -> expr"
    | BoolFunExpr _ ->     ": bool -> expr"
    | IntFunExpr _ ->      ": int -> expr"
    | StringFunExpr _ ->   ": string -> expr"
    | TermFunExpr _ ->     ": term -> expr"
    | TacticFunExpr _ ->   ": tactic -> expr"
    | IntTacticFunExpr _ -> ": (int -> tactic) -> expr"
    | AddressFunExpr _
    | AddrFunExpr _ ->     ": address -> expr"
    | ConvFunExpr _ ->     ": conv -> expr"
    | StringListFunExpr _ -> ": string list -> expr"
    | TermListFunExpr _ -> ": term list -> expr"
    | TacticListFunExpr _ -> ": tactic list -> expr"
    | ConvListFunExpr _ -> ": conv list -> expr"

   let rec format_expr buf df = function
      UnitExpr () ->
         format_string buf "()"
    | BoolExpr b ->
         format_string buf (sprintf "%b" b)
    | IntExpr i ->
         format_int buf i;
    | AddressExpr a ->
         format_string buf (string_of_address a);
    | StringExpr s ->
         format_string buf s;
    | TermExpr t ->
         Dform.format_term df buf t;
    | ListExpr l ->
         format_char buf '[';
         format_pushm buf 1;
         format_expr_list buf df l;
         format_popm buf;
         format_char buf ']';
    | TupleExpr l ->
         format_char buf '(';
         format_pushm buf 1;
         format_expr_list buf df l;
         format_popm buf;
         format_char buf ')';
    | _ ->
         format_string buf "-"

   and format_expr_type buf df expr =
      format_szone buf;
      format_expr buf df expr;
      format_space buf;
      format_ezone buf;
      format_string buf (string_type expr)

   and format_expr_list buf df = function
      [expr] ->
         format_expr_type buf df expr
    | expr :: exprs ->
         format_expr_type buf df expr;
         format_char buf ';';
         format_space buf;
         format_expr_list buf df exprs
    | [] ->
         ()

   let print_expr state out expr =
      let df = Shell_state.get_dfbase state in
      let buf = new_buffer () in
         format_expr_type buf df expr;
         print_to_channel default_width buf out;
         eflush out

   (*
    * Evaluate a struct item.
    *)
   let eval_str_item state item =
      let expr = expr_of_ocaml_str_item (Shell_state.get_toploop state) item in
         if Shell_state.is_interactive state then
            begin
               print_expr state stdout expr;
               flush stdout
            end

   let eval_opens _ _ =
      ()

   (*
    * Evaluate a directive.
    *)
   external exit : int -> unit = "caml_exit"

   let rec use state name =
      let inx = open_in name in
      let int_flag = Shell_state.is_interactive state in
      let stream = Shell_state.stream_of_channel state inx in
      let flush () = Stream.iter (fun _ -> ()) stream in
         Shell_state.set_interactive state false;
         toploop state false stream flush;
         Shell_state.set_interactive state int_flag;
         close_in inx

   (*
    * Toploop reads phrases, then prints errors.
    *)
   and toploop state prompt instream inflush =
      let loop = ref true in
         while !loop do
            let state =
               if prompt then
                  !current_state
               else
                  state
            in
               Shell_state.set_prompt state "# ";
               Shell_state.reset_terms state;
               try
                  match Shell_state.synchronize state (Grammar.Entry.parse Pcaml.top_phrase) instream with
                     Some phrase ->
                        eval_str_item state phrase
                   | None ->
                        loop := false
               with
                  End_of_file ->
                     loop := false

                | RefineError (_, ToploopIgnoreError) ->
                     ()

                | exn ->
                     let df = Shell_state.get_dfbase state in
                     let buf = new_buffer () in
                        begin match exn with
                           Stdpp.Exc_located _ | Pcaml.Qerror _ -> inflush ()
                         | _ -> format_string buf "Uncaught exception: ";
                        end;
                        Filter_exn.format_exn df buf exn;
                        print_to_channel default_width buf stderr;
                        eflush stderr
         done

   (*
    * Process some input files.
    *)
   let use_files state files =
      List.iter (use state) files

   (*
    * We just loop on the input.  Evaluation is performed by
    * the toploop resource.
    *)
   let main_loop_aux state =
      match Shell_state.get_input_files () with
         [] ->
            let instream, flush = Shell_state.stdin_stream state in
               printf "%s\n%t" version eflush;
               toploop state true instream flush
        | files ->
            use_files state files

   let main state =
      install_debug_printer Shell_state.print_term_fp;
      Sys.catch_break true;
      Tactic_type.Tactic.main_loop ();
      main_loop_aux state
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
