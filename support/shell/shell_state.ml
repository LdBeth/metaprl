(*
 * Implement the global functions required by the shell.
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
 * Copyright (C) 1999-2004 MetaPRL Group
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
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *              Nathaniel Gray <n8gray@caltech.edu>
 *)
open Printf
open Lexing

open Lm_debug
open Lm_threads

open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta

open Dform
open Filter_type
open Term_grammar

open Shell_sig

let debug_full_terms =
   create_debug {
      debug_name = "full_terms";
      debug_description = "Print terms fully in debug messages";
      debug_value = false;
   }

let debug_lock =
   create_debug (**)
      { debug_name = "lock";
        debug_description = "show locking operations";
        debug_value = false
      }

(*
 * Intialize readline package.
 *)
let rl_history_file =
   try
      Some (Sys.getenv "MP_HISTORY_FILE")
   with
      Not_found ->
         try
            let home = Sys.getenv "HOME" in
               Some (Filename.concat home ".metaprl_history")
         with
            Not_found -> None

let rl_history_length =
   try
      int_of_string (Sys.getenv "MP_HISTORY_LENGTH")
   with
      _ -> 100

let _ = Lm_readline.initialize_readline ()
let _ = match rl_history_file with
      None -> ()
    | Some name ->
         try
            Lm_readline.read_history name
         with
            Not_found ->
               ()
          | Sys_error err ->
               eprintf "Couldn't load readline history file: \"%s\"\n%s\n" name err;
               flush_all ()

(*
 * Save the text in the input_buffers during each toplevel read.
 *)
type info =
   Buffered of (int * int * string) list
 | Filename of string
 | File of in_channel

(*
 * A buffer for input strings.
 *)
type input_buf =
   { mutable buf_index : int;
     mutable buf_buffer : string
   }

(*
 * This is the type of global state.
 *)
type t =
   { mutable state_mk_opname : opname_fun;
     mutable state_mk_var_contexts : context_fun;
     mutable state_df_base : dform_base;
     mutable state_inline_terms : (int * term) list;
     mutable state_inline_var : int;
     mutable state_tactic : string * MLast.expr;
     mutable state_toploop : Mptop.top_table;
     mutable state_input_info : info;
     mutable state_interactive : bool;
     mutable state_infixes : Infix.Set.t;
   }

(*
 * Global state is a reference to one of the states.
 * Invariant: this variable is (Some _) only if the lock
 * is locked (we are within some toploop).
 *)
let state = ref None

(*
 * Lock modifications to the state.
 *)
let lock = Recursive_lock.create ()

(*
 * The infix/suffix mods that we currently have in the grammar.
 * This is only updated inside synchronize_shell calls
 *)
let infixes = ref Infix.Set.empty

(*
 * Default values.
 *)
let mk_opname_null _ =
   raise (Failure "Shell_mp.mk_opname: no current package")

let mk_var_contexts_null v i =
   if i=0 then None else raise(Failure "No context known for SO variables (need to specify contexts explicitly when not inside a rule")

let default_saved_tactic =
   let loc = 0, 0 in
      ("\"no saved tactic\"", <:expr< $str: "no saved tactic"$ >>)

(*
 * Default state.
 *)
let create () =
   Mp_resource.recompute_top ();
   { state_mk_opname = mk_opname_null;
     state_mk_var_contexts =  mk_var_contexts_null;
     state_df_base = Dform.null_base;
     state_inline_terms = [];
     state_inline_var = 0;
     state_tactic = default_saved_tactic;
     state_toploop = Mptop.get_toploop_resource (Mp_resource.find Mp_resource.top_bookmark) [];
     state_input_info = Buffered [];
     state_interactive = true;
     state_infixes = Infix.Set.empty;
   }

let fork state =
   { state with state_mk_opname = state.state_mk_opname }

(************************************************************************
 * CLIENT FUNCTIONS                                                     *
 ************************************************************************)

let update_infixes state =
   if !infixes != state.state_infixes then begin
      Infix.Set.iter Infix.add (Infix.Set.diff state.state_infixes !infixes);
      Infix.Set.iter Infix.remove (Infix.Set.diff !infixes state.state_infixes);
      infixes := state.state_infixes
   end

(*
 * Synchronize the client.
 * For some client functions, it is allowable that we are not within the
 * toploop, so don't do that check here.
 *)
let synchronize_client f =
   if !debug_lock then
      eprintf "Shell_state.lock: %d client waiting%t" (Thread.id (Thread.self ())) eflush;
   Recursive_lock.lock lock;
   if !debug_lock then
      eprintf "Shell_state.lock: %d client done%t" (Thread.id (Thread.self ())) eflush;
   try
      let result = f !state in
         ignore (Recursive_lock.unlock lock);
         result
   with
      exn ->
         ignore (Recursive_lock.unlock lock);
         raise exn

(*
 * This is the case where the client really needs to be within the toploop.
 * Check the state, and fetch it for the client.
 *)
let synchronize_state f =
   if !debug_lock then
      eprintf "Shell_state.lock: %d state waiting%t" (Thread.id (Thread.self ())) eflush;
   Recursive_lock.lock lock;
   if !debug_lock then
      eprintf "Shell_state.lock: %d state done%t" (Thread.id (Thread.self ())) eflush;
   match !state with
      None ->
         ignore (Recursive_lock.unlock lock);
         raise (Invalid_argument "Shell_mp.synchronize_state: client call is not within toploop")
    | Some state ->
         try
            update_infixes state;
            let result = f state in
               ignore (Recursive_lock.unlock lock);
               result
         with
            exn ->
               ignore (Recursive_lock.unlock lock);
               raise exn

(*
 * Extend the grammar with terms.
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   (*
    * Use global mk_opname function.
    *)
   let mk_opname loc l =
      synchronize_state (function state ->
            try state.state_mk_opname l with
               exn ->
                  Stdpp.raise_with_loc loc exn)

   let mk_var_contexts loc v i =
      synchronize_state (function state ->
            try state.state_mk_var_contexts v i with
               exn ->
                  Stdpp.raise_with_loc loc exn)

   (*
    * Term grammar.
    *)
   let gram = Pcaml.gram
   let term_eoi = Grammar.Entry.create gram "term"
   let term = Grammar.Entry.create gram "term"
   let parsed_term = Grammar.Entry.create gram "term"
   let quote_term = Grammar.Entry.create gram "quote_term"
   let mterm = Grammar.Entry.create gram "mterm"
   let bmterm = Grammar.Entry.create gram "bmterm"
   let singleterm = Grammar.Entry.create gram "singleterm"
   let applytermlist = Grammar.Entry.create gram "applytermlist"
   let parsed_bound_term = Grammar.Entry.create gram "parsed_bound_term"
   let xdform = Grammar.Entry.create gram "xdform"
   let term_con_eoi = Grammar.Entry.create gram "term_con_eoi"
end

module TermGrammar = MakeTermGrammar (TermGrammarBefore);;

(*
 * Extend the grammar.
 *)
let save_term state t =
   let loc = 0, 0 in
   let v = state.state_inline_var in
      state.state_inline_var <- succ v;
      state.state_inline_terms <- (v, t) :: state.state_inline_terms;
      (<:expr< ($lid: "shell_get_term"$) $int: string_of_int v$ >>)

let get_term_state state i =
   try List.assoc i state.state_inline_terms with
      Not_found ->
         eprintf "Term %d not found%t" i eflush;
         xnil_term

let get_term i =
   synchronize_state (fun state -> get_term_state state i)

let term_exp s =
   synchronize_state (fun state ->
         let cs = Stream.of_string s in
         let t = Grammar.Entry.parse TermGrammarBefore.term_eoi cs in
            save_term state (term_of_parsed_term_with_vars t))

let term_patt s =
   raise (Failure "Shell_mp.term_patt: not implemented yet")

let _ = Quotation.add "term" (Quotation.ExAst (term_exp, term_patt))
let _ = Quotation.default := "term"

(*
 * The client also saves the most recent tactic.
 *)
let set_tactic s e =
   synchronize_state (fun state ->
         state.state_tactic <- (s, e))

(*
 * Use format library for term printing.
 * This will not always print the term correctly,
 * since a print can occur in the refiner somewhere
 * outside the current invocation of the toploop.
 *)
let print_term state t =
   let db = state.state_df_base in
   let buf = Rformat.new_buffer () in
      Dform.format_term db buf t;
      Rformat.print_to_channel Rformat.default_width buf stdout;
      flush stdout

let output_short db out t =
    let str = try
       Rformat.line_format Rformat.default_width (fun bf -> Dform.format_term db bf t)
    with exn ->
       "unprintable term (printer raised " ^ (Printexc.to_string exn) ^ ")"
    in
       output_string out str

let output_long db out t =
   let buf = Rformat.new_buffer () in
      Dform.format_term db buf t;
      Rformat.print_to_channel Rformat.default_width buf out

let get_dbase = function
   Some state -> state.state_df_base
 | None -> Dform.null_base

let print_term_fp out t =
   let printer = if !debug_full_terms then output_long else output_short in
      synchronize_client (fun state -> printer (get_dbase state) out t; flush out)

let term_printer t =
   synchronize_client (fun state ->
      Format.open_box 0;
      Format.print_string (Dform.string_of_term (get_dbase state) t);
      Format.close_box())

(************************************************************************
 * TOPLOOP FUNCTIONS                                                    *
 ************************************************************************)

(*
 * Get the tactic for the last refinement.
 *)
let get_tactic state =
   state.state_tactic

(*
 * Set the opname function.
 *)
let set_mk_opname state = function
   Some f ->
      state.state_mk_opname <- f
 | None ->
      state.state_mk_opname <- mk_opname_null

let set_infixes state = function
   Some infs ->
      state.state_infixes <- infs
 | None ->
      state.state_infixes <- Infix.Set.empty

let set_so_var_context state = function
   Some ts ->
      let delayed_fun v i =
         let f = context_subst_of_terms ts in
         let f v i = match f v i with
            Some _ as conts -> conts
          | None ->
               if i = 0 then None else
                  raise(Failure "Unknown SO variable, please specify contexts explicitly")
         in
            state.state_mk_var_contexts <- f;
            f v i
      in
         state.state_mk_var_contexts <- delayed_fun
 | None ->
      state.state_mk_var_contexts <- mk_var_contexts_null

(*
 * Set the display base.
 *)
let set_dfbase state df =
   let df =
      match df with
         Some df ->
            df
       | None ->
            Dform.null_base
   in
      Dform.debug_base := df;
      state.state_df_base <- df

let get_dfbase state =
   state.state_df_base

(*
 * Fetch terms after parsing.
 *)
let reset_terms state =
   state.state_inline_terms <- []

(*
 * Activate the toploop.
 * Be careful about recursive locks.
 *)
let synchronize state' f x =
   if !debug_lock then
      eprintf "Shell_state.synchronize: %d begin%t" (Thread.id (Thread.self ())) eflush;
   Recursive_lock.lock lock;
   if !debug_lock then
      eprintf "Shell_state.synchronize: %d got lock%t" (Thread.id (Thread.self ())) eflush;
   state := Some state';
   try
      update_infixes state';
      let result = f x in
         state := None;
         if not (Recursive_lock.unlock lock) then
            state := Some state';
         if !debug_lock then
            eprintf "Shell_state.synchronize: %d released lock%t" (Thread.id (Thread.self ())) eflush;
         result
      with
         exn ->
            if not (Recursive_lock.unlock lock) then
               state := Some state';
            if !debug_lock then
               eprintf "Shell_state.synchronize: %d released lock%t" (Thread.id (Thread.self ())) eflush;
            raise exn

let unsynchronize state' f x =
   state := None;
   if !debug_lock then
      eprintf "Shell_state.unsynchronize: %d releasing lock%t" (Thread.id (Thread.self ())) eflush;
   ignore (Recursive_lock.unlock lock);
   try
      let result = f x in
         if !debug_lock then
            eprintf "Shell_state.unsynchronize: %d reacquiring lock%t" (Thread.id (Thread.self ())) eflush;
         Recursive_lock.lock lock;
         if !debug_lock then
            eprintf "Shell_state.unsynchronize: %d lock reacquired%t" (Thread.id (Thread.self ())) eflush;
         state := Some state';
         update_infixes state';
         result
   with
      exn ->
         if !debug_lock then
            eprintf "Shell_state.unsynchronize: %d reacquiring lock%t" (Thread.id (Thread.self ())) eflush;
         Recursive_lock.lock lock;
         if !debug_lock then
            eprintf "Shell_state.unsynchronize: %d lock reacquired%t" (Thread.id (Thread.self ())) eflush;
         state := Some state';
         raise exn

(*
 * Set the module.
 * Collect the toplevel commands to use.
 * Shell commands are always added in.
 *)
let set_module state name =
   let rsrc =
      try Mp_resource.find (Mp_resource.theory_bookmark name) with
         Not_found ->
            eprintf "Module %s: resources not found%t" (String.capitalize name) eflush;
            Mp_resource.recompute_top ();
            Mp_resource.find Mp_resource.top_bookmark
   in
   let shell_expr = IntFunExpr (fun i -> TermExpr (get_term_state state i)) in
   let top = Mptop.get_toploop_resource rsrc ["", "shell_get_term", shell_expr, FunType(IntType, TermType)] in
      state.state_toploop <- top

let get_toploop state =
   state.state_toploop

(*
 * Return interactive flag.
 *)
let is_interactive state =
   state.state_interactive

let set_interactive state flag =
   state.state_interactive <- flag

(************************************************************************
 * TOPLEVEL PARSING                                                     *
 ************************************************************************)

(*
 * Push a new value into the buffer.
 *)
let push_buffer state abs len buf =
   match state.state_input_info with
      Buffered l ->
         state.state_input_info <- Buffered ((abs, len, buf) :: l)
    | _ ->
         raise (Failure "Shell_p4.push_buffer")

(*
 * Reset the input to the buffered state
 * with an empty buffer.
 *)
let reset_input state =
   let _ =
      match state.state_input_info with
         File input ->
            close_in input
       | _ ->
            ()
   in
      state.state_input_info <- Buffered []

(*
 * Set the file to read from.
 *)
let set_file state name =
   reset_input state;
   state.state_input_info <- Filename name

(*
 * Get the text associated with a location.
 *)
let get_buffered_text (start, finish) bufs =
   let count = finish - start in
   let s = Lm_string_util.create "Shell_p4.get_buffered_text" count in
   let rec collect count = function
      (pos, len, buf) :: t ->
         if start > pos then
            if start + count - pos > len then
               raise (Failure "collect")
            else
               Lm_string_util.blit "Shell_p4.get_buffered_text" buf (start - pos) s 0 count
         else if start + count > pos then
            let amount = start + count - pos in
               if amount > len then
                  raise (Failure "collect")
               else
                  begin
                     Lm_string_util.blit "Shell_p4.get_buffered_text" buf 0 s (pos - start) amount;
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
   let buf = Lm_string_util.create "Shell_p4.get_file_text" (finish - start) in
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
let get_text_aux state loc =
   match state.state_input_info with
      Buffered bufs ->
         get_buffered_text loc bufs
    | Filename name ->
         begin
            try
               let input = open_in name in
                  state.state_input_info <- File input;
                  get_file_text loc input
            with
               Sys_error _ ->
                  let start, finish = loc in
                     eprintf "Can't recover input, file %s, characters (%d, %d)%t" name start finish eflush;
                     raise (Failure "get_text")
         end
    | File input ->
         get_file_text loc input

let get_text loc =
   synchronize_state (function
      state ->
         get_text_aux state loc)

(*
 * Create an empty buffer.
 *)
let create_buffer () =
   { buf_index = 0; buf_buffer = "" }

(*
 * Wrap the input channel so that we can recover input.
 * Unblock the state while we are reading so other shells can make progress.
 *)
let stream_of_channel state inx =
   let buf = create_buffer () in
   let refill loc =
      let str = unsynchronize state input_line inx ^ "\n" in
         buf.buf_index <- 0;
         buf.buf_buffer <- str;
         push_buffer state loc (String.length str) str
   in
   let rec read loc =
      let { buf_index = index; buf_buffer = buffer } = buf in
         if index = String.length buffer then
            try
               refill loc;
               read loc
            with
               End_of_file ->
                  None
         else
            let c = buffer.[index] in
               buf.buf_index <- index + 1;
               Some c
   in
      reset_input state;
      Stream.from read

(*
 * Wrap the input channel so that we can recover input.
 * Use the readline package.  Input is always from stdin.
 *)
let stdin_prompt = ref "# "
let stdin_prompt2 = ref "  "

let set_prompt _ prompt =
   stdin_prompt := prompt

let set_prompt2 _ prompt =
   stdin_prompt2 := prompt

let save_readline_history () =
   match rl_history_file with
      None -> ()
    | Some name ->
         try
            Lm_readline.write_history name;
            Lm_readline.history_truncate_file name rl_history_length
         with
            Sys_error err ->
               eprintf "Couldn't save readline history file \"%s\"\n%s\n" name err


let stdin_stream state =
   let buf = create_buffer () in
   let refill loc =
      let str = unsynchronize state Lm_readline.readline !stdin_prompt ^ "\n" in
         stdin_prompt := !stdin_prompt2;
         buf.buf_index <- 0;
         buf.buf_buffer <- str;
         push_buffer state loc (String.length str) str
   in
   let rec read loc =
      let { buf_index = index; buf_buffer = buffer } = buf in
         if index = String.length buffer then
            try
               refill loc;
               read loc
            with
               End_of_file ->
                  save_readline_history ();
                  None
         else
            let c = buffer.[index] in
               buf.buf_index <- index + 1;
               Some c
   in
   let flush () =
      buf.buf_index <- 0;
      buf.buf_buffer <- ""
   in
      reset_input state;
      Stream.from read, flush

(*
 * Wrap the toplevel input function.
 * Replace the buffer filler so that we record all the input.
 *)
let rec wrap state f lb =
   let refill = lb.refill_buff in
   let refill' lb =
      lb.lex_buffer <- String.copy lb.lex_buffer;
      unsynchronize state refill lb;
         push_buffer state lb.lex_abs_pos lb.lex_buffer_len lb.lex_buffer;
   in
   let lb = { lb with refill_buff = refill' } in
      reset_input state;
      push_buffer state lb.lex_abs_pos lb.lex_buffer_len lb.lex_buffer;
      let x = f lb in
         reset_input state;
         x

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
 * File arguments.
 *)
let input_files = ref []

let get_input_files () =
   !input_files

(*
 * Anonymous arguments are rejected.
 *)
let handle_anon_arg arg =
   input_files := !input_files @ [arg]

(*
 * Argument specifications.
 *)
let spec =
   ["-I", Arg.String add_include, ": add a directory to the path for include files"]

let _ =
   (* Debug_symbols.debug_symbols Sys.argv.(0); *)
   Arg.current := 0;
   Env_arg.parse spec handle_anon_arg "MetaPRL toploop"

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
