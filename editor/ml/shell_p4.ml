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

open Mp_debug
open Mp_pervasives

open Pcaml

open Toploop

open Refiner.Refiner.Term
open Refiner.Refiner.RefineError

open Filter_ast
open Term_grammar
open Filter_grammar

open Tacticals
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
    * PRINTERS                                                             *
    ************************************************************************)

   (*
    * This is the current display form database.
    *)
   let df = ref None

   let set_df df' =
      df := df'

   let get_df found notfound =
      match !df with
         Some df ->
            found df
       | None ->
            notfound

   (*
    * Printers.
    *)
   let string_of_term t =
      (get_df Dform.string_of_term Simple_print.SimplePrint.string_of_term) t

   (*
    * Use format library for term printing.
    *)
   let print_term t =
      Format.print_string (string_of_term t)

   let print_term_fp out t =
      let db = get_df (fun x -> x) Dform.null_base in
      let buf = Rformat.new_buffer () in
         Dform.format_term db buf t;
         Rformat.print_to_channel 80 buf out

   (************************************************************************
    * QUOTATIONS                                                           *
    ************************************************************************)

   (*
    * Term parsing.
    *)
   let mk_opname_null _ =
      raise (Failure "Shell_p4.mk_opname: no current package")

   let mk_opname_ref = ref mk_opname_null

   let set_mk_opname = function
      Some f ->
         mk_opname_ref := f
    | None ->
         mk_opname_ref := mk_opname_null

   (*
    * Base term grammar.
    *)
   module TermGrammarBefore : TermGrammarSig =
   struct
      let mk_opname loc l =
         try !mk_opname_ref l with
            exn ->
               Stdpp.raise_with_loc loc exn

      (*
       * Term grammar.
       *)
      let gram = Pcaml.gram
      let term_eoi = Grammar.Entry.create gram "term"
      let term = Grammar.Entry.create gram "term"
      let quote_term = Grammar.Entry.create gram "quote_term"
      let mterm = Grammar.Entry.create gram "mterm"
      let singleterm = Grammar.Entry.create gram "singleterm"
      let bound_term = Grammar.Entry.create gram "bound_term"
      let xdform = Grammar.Entry.create gram "xdform"
   end

   (*
    * Extended term grammar.
    *)
   module TermGrammar = MakeTermGrammar (TermGrammarBefore)

   (*
    * String -> string translator.
    *)
   let term_exp s =
      let cs = Stream.of_string s in
      let t = Grammar.Entry.parse TermGrammar.term_eoi cs in
         build_ml_term (0, 0) t

   let term_patt s =
      raise (Failure "Shell_p4.term_patt: not implemented yet")

   let _ = Quotation.add "term" (Quotation.ExAst (term_exp, term_patt))
   let _ = Quotation.default := "term"

   (************************************************************************
    * ARGUMENT COLLECTION                                                  *
    ************************************************************************)

   (*
    * -I <dir>
    *)
   let includes = ref []

   let get_includes () =
      !includes

   let add_include dir =
      includes := !includes @ [dir]

   (*
    * Interactive flag.
    *)
   let interactive_flag = ref true

   let is_interactive () =
      !interactive_flag

   (*
    * Anonymous arguments are rejected.
    *)
   let handle_anon_arg arg =
      raise (Failure ("illegal argument: " ^ arg))

   (*
    * Argument specifications.
    *)
   let spec =
      ["-I", Arg.String add_include, "add an directory to the path for include files"]

   let _ =
      Arg.current := 0;
      Env_arg.parse spec handle_anon_arg "MetaPRL toploop"

   (************************************************************************
    * TOPLEVEL                                                             *
    ************************************************************************)

   (*
    * Don't need to do anything when module is set.
    *)
   let set_module _ _ =
      ()

   (*
    * Save the text in the input_buffers during each toplevel read.
    *)
   type info =
      Buffered of (int * int * string) list
    | Filename of string
    | File of in_channel

   let input_info = ref (Buffered [])

   (*
    * Push a new value into the buffer.
    *)
   let push_buffer abs len buf =
      match !input_info with
         Buffered l ->
            input_info := Buffered ((abs, len, buf) :: l)
       | _ ->
            raise (Failure "Shell_p4.push_buffer")

   (*
    * Reset the input to the buffered state.
    *)
   let reset_input () =
      let _ =
         match !input_info with
            File input ->
               close_in input
          | _ ->
               ()
      in
         input_info := Buffered []

   (*
    * Set the file to read from.
    *)
   let set_file name =
      reset_input ();
      input_info := Filename name

   (*
    * Get the text associated with a location.
    *)
   let get_buffered_text (start, finish) bufs =
      let count = finish - start in
      let s = String_util.create "Shell_p4.get_buffered_text" count in
      let rec collect count = function
         (pos, len, buf) :: t ->
            if start > pos then
               if start + count - pos > len then
                  raise (Failure "collect")
               else
                  String_util.blit "Shell_p4.get_buffered_text" buf (start - pos) s 0 count
            else if start + count > pos then
               let amount = start + count - pos in
                  if amount > len then
                     raise (Failure "collect")
                  else
                     begin
                        String_util.blit "Shell_p4.get_buffered_text" buf 0 s (pos - start) amount;
                        collect (count - amount) t
                     end
            else
               collect count t
       | [] ->
            if count <> 0 then
               raise (Failure "collect")
      in
         try
            collect count bufs;
            s
         with
            Failure "collect" ->
               eprintf "Can't recover input, characters (%d, %d)%t" start finish eflush;
               raise (Failure "get_text")

   (*
    * Get the text from the file.
    *)
   let get_file_text (start, finish) input =
      let buf = String_util.create "Shell_p4.get_file_text" (finish - start) in
         try
            seek_in input start;
            really_input input buf 0 (finish - start);
            buf
         with
            End_of_file ->
               eprintf "Can't recover input, characters (%d, %d)%t" start finish eflush;
               raise (Failure "get_file_text")

   (*
    * Get the text from the input.
    *)
   let get_text loc =
      match !input_info with
         Buffered bufs ->
            get_buffered_text loc bufs
       | Filename name ->
            begin
               try
                  let input = open_in name in
                     input_info := File input;
                     get_file_text loc input
               with
                  Sys_error _ ->
                     let start, finish = loc in
                        eprintf "Can't recover input, file %s, characters (%d, %d)%t" name start finish eflush;
                        raise (Failure "get_text")
            end
       | File input ->
            get_file_text loc input

   (*
    * Wrap the toplevel input function.
    * Replace the buffer filler so that we record all the input.
    *)
   let rec wrap f lb =
      let { refill_buff = refill;
            lex_buffer = buffer;
            lex_buffer_len = len;
            lex_abs_pos = abs_pos;
            lex_start_pos = start_pos;
            lex_curr_pos = curr_pos;
            lex_last_pos = last_pos;
            lex_last_action = last_action;
            lex_eof_reached = eof_reached
          } = lb
      in
      let refill' lb =
         lb.lex_buffer <- String.copy lb.lex_buffer;
         refill lb;
         let { lex_buffer = buffer;
               lex_buffer_len = len;
               lex_abs_pos = abs_pos;
               lex_start_pos = start_pos;
               lex_curr_pos = curr_pos
             } = lb
         in
            push_buffer abs_pos len buffer;
      in
      let lb =
         { refill_buff = refill';
           lex_buffer = buffer;
           lex_buffer_len = len;
           lex_abs_pos = abs_pos;
           lex_start_pos = start_pos;
           lex_curr_pos = curr_pos;
           lex_last_pos = last_pos;
           lex_last_action = last_action;
           lex_eof_reached = eof_reached
         }
      in
         reset_input ();
         push_buffer abs_pos len buffer;
         let x = f lb in
            reset_input ();
            x

   let wrap_once f lb =
      let x = wrap f lb in
         Toploop.parse_toplevel_phrase := wrap !Toploop.parse_toplevel_phrase;
         x

   (*
    * Wrap a file.
    * We don't need to modify the lexbuf.
    * Instead, we'll get the chars from the file.
    * Unfortunately, we don't close the file once we're done, because the
    * input hasn't been evaluated yet.
    *)
   let wrap_file f lb =
      set_file !Toploop.input_name;
      f lb

   (*
    * Wrap the toplevel.
    *)
   let _ =
      let wrapped = !Toploop.parse_toplevel_phrase in
      let motd lb =
         eprintf "\t%s\n%t" version eflush;
         Toploop.parse_toplevel_phrase := wrap_once wrapped;
         !Toploop.parse_toplevel_phrase lb
      in
         Toploop.parse_toplevel_phrase := motd

   (*
    * Wrap file usage.
    *)
   let _ =
      let wrapped = !Toploop.parse_use_file in
         Toploop.parse_use_file := wrap_file wrapped

   (************************************************************************
    * TACTIC SAVING                                                        *
    ************************************************************************)

   (*
    * We save the tactic and its text to be passed to the refiner.
    *)
   let saved_tactic = ref ("\"no saved tactic\"",
                           let loc = (0, 0) in
                              <:expr< $str: "no saved tactic"$ >>)

   let set_tactic s e =
      saved_tactic := (s, e)

   let get_tactic () =
      !saved_tactic

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
             if not (Toploop.execute_phrase false (Parsetree.Ptop_def pt_item)) then
                raise (RefineError ("eval_expr", StringError "evaluation failed"))
          with
             Typecore.Error (_, err) ->
                Typecore.report_error err;
                eflush stdout;
                raise (RefineError ("eval_expr", StringError "evaluation failed"))

   let eval_expr loc expr =
      eval_str_item loc (<:str_item< $exp: expr$ >>)

   let eval_tactic expr =
      let loc = 0, 0 in
      let expr = (<:expr< $uid: "Shell_p4"$ . $lid: "install_tactic"$ $expr$ >>) in
      let item = (<:str_item< $exp: expr$ >>) in
      let pt_item = Ast2pt.str_item item [] in
          inline_tactic := None;
          try
             if Toploop.execute_phrase false (Parsetree.Ptop_def pt_item) then
                match !inline_tactic with
                   Some tac ->
                      tac
                 | None ->
                      raise (RefineError ("eval_tactic", StringError "evaluation failed"))
             else
                raise (RefineError ("eval_tactic", StringError "evaluation failed"))
          with
             Typecore.Error (_, err) ->
                Typecore.report_error err;
                eflush stdout;
                raise (RefineError ("eval_tactic", StringError "evaluation failed"))

   let parse_string str =
      let instream = Stream.of_string str in
         Grammar.Entry.parse Pcaml.expr instream

   let eval_opens opens =
      let eval_open path =
         let loc = 0, 0 in
            eprintf "Open %a%t" print_string_list path eflush;
            eval_str_item loc (<:str_item< open $path$ >>)
      in
         List.iter eval_open opens

   (*
    * Build a delayed-evaluation tactic.
    *)
   let create_tactic expr =
      let cell = ref (Delay expr) in
      let tac p =
         match !cell with
            Tactic tac ->
               tac p
          | Delay expr ->
               let tac = eval_tactic expr in
                  cell := Tactic tac;
                  tac p
      in
         tac

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
             let e = <:expr< $uid:"Mp"$ . $lid:"refine"$ $e$ >> in
                <:str_item< $exp: e$ >>
          ]];

      refine_item:
         [[ e = expr ->
             set_tactic (get_text loc) e;
             e
          ]];
   END

   (*
    * Main function installs printers and include directories.
    * Then exits to pass control to the toploop.
    *)
   let main () =
      let init () =
         let mplib =
            try Sys.getenv "MPLIB" with
               Not_found ->
                  raise (Invalid_argument "MPLIB environment variable in undefined")
         in
         let eval_include inc =
            Toploop.execute_phrase false (Ptop_dir ("directory", Pdir_string inc));
            ()
         in
            Debug_set.init ();
            eval_include mplib;
            List.iter eval_include !includes;
            Toploop.execute_phrase false (Ptop_dir ("install_printer", Pdir_ident (Ldot (Lident "Shell_p4", "print_term"))));
            Toploop.execute_phrase false (Ptop_def [{ pstr_desc = Pstr_open (Lident "Mp");
                                                      pstr_loc = Location.none
                                                    }]);
            Tactic_type.main_loop ();
            ()
      in
         install_debug_printer print_term_fp;
         Printexc.catch init ()
end

let print_term = ShellP4.print_term

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
