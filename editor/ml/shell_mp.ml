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

include Mptop

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

module ShellP4 (State : ShellStateSig) =
struct
   let _ =
      show_loading "Loading Shell_mp%t"

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Our state is just the global state.
    *)
   type t = State.t

   (************************************************************************
    * STATE FUNCTIONS                                                      *
    ************************************************************************)

   (*
    * State for evaluating toploop expressions.
    *)
   let current_state = ref (State.create ())

   let get_current_state () =
      !current_state

   let set_current_state state =
      current_state := state

   (*
    * We take these directly from the state.
    *)
   let create = State.create
   let fork = State.fork
   let get_includes = State.get_includes
   let get_tactic = State.get_tactic
   let print_term = State.print_term
   let set_module = State.set_module
   let set_mk_opname = State.set_mk_opname
   let set_df = State.set_dfbase
   let is_interactive = State.is_interactive

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
             State.set_tactic (State.get_text loc) e;
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
      State.synchronize state (function expr ->
          match expr_of_ocaml_expr (State.get_toploop state) expr with
             TacticExpr tac ->
                tac
           | _ ->
               raise (RefineError ("eval_tactic", StringError "expression is not a tactic")))

   let parse_string state =
      State.synchronize state (function str ->
          let instream = Stream.of_string str in
             Grammar.Entry.parse Pcaml.expr instream)

   let eval_expr state =
      State.synchronize state (function str ->
         let instream = Stream.of_string str in
         let expr = Grammar.Entry.parse Pcaml.expr instream in
         let _ = expr_of_ocaml_expr (State.get_toploop state) expr in
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
    | AddressFunExpr _ ->  ": address -> expr"
    | ConvFunExpr _ ->     ": conv -> expr"
    | AddrFunExpr _ ->     ": address -> expr"
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
      let df = State.get_dfbase state in
      let buf = new_buffer () in
         format_expr_type buf df expr;
         print_to_channel default_width buf out;
         eflush out

   (*
    * Evaluate a struct item.
    *)
   let eval_str_item state loc item =
      let expr = expr_of_ocaml_str_item (State.get_toploop state) item in
         if State.is_interactive state then
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
      let int_flag = State.is_interactive state in
         State.set_interactive state false;
         toploop state false (State.stream_of_channel state inx);
         State.set_interactive state true;
         close_in inx

   and eval_directive state loc str = function
      DpNon ->
         begin
            match str with
              "quit" ->
                 exit 0
             | _ ->
                 raise (Failure (sprintf "Unknown command %s" str))
         end
    | DpStr str' ->
         begin
            match str with
               "use" ->
                  use state str'
             | _ ->
                  raise (Failure (sprintf "Unknown %s %s" str str'))
         end
    | DpInt str' ->
         eprintf "Directive DpInt: %s/%s%t" str str' eflush
    | DpIde strs ->
         eprintf "Directive DpIde: %s" str;
         List.iter (fun s -> eprintf "/%s" s) strs;
         eflush stderr

   (*
    * Evaluate a toplevel phrase.
    *)
   and eval_phrase state = function
      PhStr (loc, item) ->
         eval_str_item state loc item
    | PhDir (loc, str, param) ->
         eval_directive state loc str param

   (*
    * Toploop reads phrases, then prints errors.
    *)
   and toploop state prompt instream =
      let loop = ref true in
         while !loop do
            let state =
               if prompt then
                  !current_state
               else
                  state
            in
               State.set_prompt state "# ";
               State.reset_terms state;
               try
                  match State.synchronize state (Grammar.Entry.parse Pcaml.top_phrase) instream with
                     Some phrase ->
                        eval_phrase state phrase
                   | None ->
                        loop := false
               with
                  Stdpp.Exc_located ((start, finish), exn) ->
                     let df = State.get_dfbase state in
                     let buf = new_buffer () in
                        format_string buf "chars ";
                        format_int buf start;
                        format_string buf "-";
                        format_int buf finish;
                        format_string buf ": ";
                        begin
                           match exn with
                              Pcaml.Qerror (_, _, exn) ->
                                 Filter_exn.format_exn df buf exn
                            | exn ->
                                 Filter_exn.format_exn df buf exn
                        end;
                        print_to_channel default_width buf stderr;
                        eflush stderr
                | End_of_file ->
                     loop := false

                | (Pcaml.Qerror _) as exn ->
                     Pcaml.report_error exn;
                     eflush stderr

                | RefineError (_, ToploopIgnoreError) ->
                     ()

                | exn ->
                     let df = State.get_dfbase state in
                     let buf = new_buffer () in
                        format_string buf "uncaught exception: ";
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
      match State.get_input_files () with
         [] ->
            let instream = State.stdin_stream state in
               printf "%s\n%t" version eflush;
               toploop state true instream
        | files ->
            use_files state files

   let main state =
      install_debug_printer State.print_term_fp;
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
