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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_debug
open Lm_rprintf
open Lm_rformat

open Pcaml

open Basic_tactics
open Mptop

open Refine_exn
open Exn_boot
open Shell_sig

module ShellP4 =
struct
   let _ =
      show_loading "Loading Shell_mp%t"

   (************************************************************************
    * SHELL GRAMMAR                                                        *
    ************************************************************************)

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
   let eval_tactic e =
      Shell_state.synchronize (tactic_of_ocaml_expr (Shell_state.get_toploop ())) e

   let parse_string s =
      Shell_state.synchronize (fun str ->
          let instream = Stream.of_string str in
             grammar_parse Pcaml.expr instream) s

   let eval_expr s =
      Shell_state.synchronize (fun str ->
         let instream = Stream.of_string str in
         let expr = grammar_parse Pcaml.expr instream in
            ignore (evaluate_ocaml_expr (Shell_state.get_toploop ()) expr)) s

   (************************************************************************
    * TOPLOOP                                                              *
    ************************************************************************)

   let rec format_expr buf df = function
      UnitExpr () ->
         format_string buf "()"
    | BoolExpr b ->
         format_string buf (string_of_bool b)
    | IntExpr i ->
         format_int buf i;
    | AddressExpr a ->
         format_string buf (string_of_address a);
    | Addr_itemExpr (Subterm i) ->
         format_int buf i;
    | Addr_itemExpr (ArgAddr) ->
         format_string buf "Arg"
    | Addr_itemExpr (ClauseAddr 0) ->
         format_string buf "Concl"
    | Addr_itemExpr (ClauseAddr i) ->
         format_string buf "Hyp ";
         format_int buf i
    | StringExpr s ->
         format_string buf s;
    | TermExpr t ->
         Dform.format_term df buf t;
    | ListExpr l ->
         format_char buf '[';
         format_pushm buf 1;
         format_expr_list buf df l;
         format_popm buf;
         format_char buf ']'
    | _ ->
         format_string buf "-"

   and format_expr_list buf df = function
      [expr] ->
         format_expr buf df expr
    | expr :: exprs ->
         format_expr buf df expr;
         format_char buf ';';
         format_space buf;
         format_expr_list buf df exprs
    | [] ->
         ()

   let print_expr out (expr, typ) =
      let buf = new_buffer () in
         format_szone buf;
         format_expr buf (Shell_state.get_dfbase ()) expr;
         format_space buf;
         format_ezone buf;
         format_string buf ": ";
         format_string buf (str_typ typ);
         format_newline buf;
         output_rbuffer out buf

   (*
    * Evaluate a struct item.
    *)
   let eval_str_item item =
      let expr = evaluate_ocaml_str_item (Shell_state.get_toploop ()) item in
         if Shell_state.is_interactive () then
            begin
               print_expr stdout expr;
               flush stdout
            end

   (*
    * For now, this assumes we are in browser mode.
    *)
   let eval_top s =
      Shell_state.synchronize (function str ->
         let instream = Shell_state.stream_of_string str in
            match grammar_parse Pcaml.top_phrase instream with
               Some item ->
                  let expr = evaluate_ocaml_str_item (Shell_state.get_toploop ()) item in
                     print_expr stdout expr;
                     flush stdout
             | None ->
                  ()) s

   let eval_opens _ =
      ()

   (*
    * Evaluate a directive.
    *)
   let rec use name =
      let inx = open_in name in
      let int_flag = Shell_state.is_interactive () in
      let stream = Shell_state.stream_of_channel inx in
      let flush () = Stream.iter (fun _ -> ()) stream in
         Shell_state.set_interactive false;
         toploop stream flush;
         Shell_state.set_interactive int_flag;
         close_in inx

   (*
    * Toploop reads phrases, then prints errors.
    *)
   and toploop instream inflush =
      let loop = ref true in
      let print_exn exn =
         let df = Shell_state.get_dfbase () in
         let buf = new_buffer () in
            let need_popm =
               match exn with
                  Stdpp.Exc_located _
                | Pcaml.Qerror _ ->
                     inflush ();
                     false
                | _ ->
                     format_pushm buf 2;
                     format_string buf "Uncaught exception: ";
                     format_newline buf;
                     true
            in
            Filter_exn.format_exn df buf exn;
            if need_popm then format_popm buf;
            output_rbuffer stderr buf;
            eflush stderr
      in
      let catch =
         if backtrace then
            begin
               eprintf "*** Note: OCAMLRUNPARAM environment variable contains \"b\".\n*** Uncaught exceptions will cause MetaPRL to exit@.";
               (fun f ->
                     try f () with
                        End_of_file ->
                           loop := false

                      | ToploopIgnoreExn _
                      | RefineError (_, ToploopIgnoreError) ->
                           ())
            end
         else
            (fun f ->
                  try f () with
                     End_of_file ->
                        loop := false

                   | ToploopIgnoreExn _
                   | RefineError (_, ToploopIgnoreError) ->
                        ()

                   | exn ->
                        (try print_exn exn with
                            _ ->
                               eprintf "ERROR: Exception printer raised an exception%t" eflush))
      in
      let () =
         (* Ignore initialization errors *)
         try Shell_command.init () with
            _ ->
               ()
      in
         while !loop do
            Shell_state.set_prompt "# ";
            Shell_state.reset_terms ();
            catch (fun () ->
                  match Shell_state.synchronize (grammar_parse Pcaml.top_phrase) instream with
                     Some phrase ->
                        eval_str_item phrase
                   | None ->
                        loop := false)
         done

   (*
    * Process some input files.
    *)
   let use_files files =
      List.iter use files

   (*
    * We just loop on the input.  Evaluation is performed by
    * the toploop resource.
    *)
   let main_loop_aux () =
      match Shell_state.get_input_files () with
         [] ->
            let instream, flush = Shell_state.stdin_stream () in
               if not !Shell_state.batch_flag then printf "%s\n@." Mp_version.version;
               toploop instream flush
        | files ->
            use_files files

   let main () =
      install_debug_printer Shell_state.print_term_fp;
      Sys.catch_break true;
      Tactic_type.Tactic.main_loop ();
      main_loop_aux ()

   let version = Mp_version.version

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
