(*
 * Define the additional grammar for the shell.
 *)

open Printf
open Lexing

open Debug

open Pcaml

open Toploop

open Refiner.Refiner.Term

open Filter_ast
open Term_grammar

(************************************************************************
 * CONSTANTS                                                            *
 ************************************************************************)

(*
 * Version number.
 *)
let version = "1.07.0.1"

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
   (get_df Dform.string_of_term Simple_print.string_of_term) t

(*
 * Use format library for term printing.
 *)
let print_term t =
   Format.print_string (string_of_term t)

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
 * TOPLEVEL                                                             *
 ************************************************************************)

(*
 * We wrap the toplevel so that we can get the text associated with a
 * location.  We save the text in the input_buffers during each toplevel read.
 *)
let input_buffers = ref []

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
         Ref_util.push (abs_pos, len, buffer) input_buffers
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
      input_buffers := [(abs_pos, len, buffer)];
      let x = f lb in
         input_buffers := [];
         x

let wrap_once f lb =
   let x = wrap f lb in
      Toploop.parse_toplevel_phrase := wrap !Toploop.parse_toplevel_phrase;
      x

(*
 * Get the text associated with a location.
 *)
let get_text (start, finish) =
   let count = finish - start in
   let s = String.create count in
   let rec collect count = function
      (pos, len, buf) :: t ->
         if start > pos then
            if start + count - pos > len then
               raise (Failure "collect")
            else
               String.blit buf (start - pos) s 0 count
         else if start + count > pos then
            let amount = start + count - pos in
               if amount > len then
                  raise (Failure "collect")
               else
                  begin
                     String.blit buf 0 s (pos - start) amount;
                     collect (count - amount) t
                  end
         else
            collect count t
    | [] ->
         if count <> 0 then
            raise (Failure "collect")
   in
      try
         collect count !input_buffers;
         s
      with
         Failure "collect" ->
            eprintf "Can't recover input, characters (%d, %d)%t" start finish eflush;
            raise (Failure "get_text")

(*
 * Wrap the toplevel.
 *)
let _ =
   let wrapped = !Toploop.parse_toplevel_phrase in
   let motd lb =
      eprintf "\tNuprl-Light version %s\n%t" version eflush;
      Toploop.parse_toplevel_phrase := wrap_once wrapped;
      !Toploop.parse_toplevel_phrase lb
   in
      Toploop.parse_toplevel_phrase := motd

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
 * SHELL GRAMMAR                                                        *
 ************************************************************************)

EXTEND
   GLOBAL: str_item;

   str_item:
      [[ "refine"; e = refine_item ->
          let e = <:expr< $uid:"Shell"$ . $lid:"refine"$ $e$ >> in
             <:str_item< $exp: e$ >>
       ]];

   refine_item:
      [[ e = expr ->
          set_tactic (get_text loc) e;
          e
       ]];
END

(*
 * $Log$
 * Revision 1.4  1998/06/01 13:52:30  jyh
 * Proving twice one is two.
 *
 * Revision 1.3  1998/05/28 13:46:00  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.2  1998/04/28 18:29:56  jyh
 * ls() works, adding display.
 *
 * Revision 1.1  1998/04/23 20:04:14  jyh
 * Initial rebuilt editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
