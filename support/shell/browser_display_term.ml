(*
 * This is the standard interface to the window system.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
open Lm_string_set
open Lm_rformat
open Lm_format

(************************************************************************
 * A simple bounded buffer.
 *)
module LineBuffer =
struct
   type 'a t = 'a Queue.t

   let max_queue_length = 100

   let create = Queue.create

   let add queue buf =
      if Queue.length queue = max_queue_length then
         ignore (Queue.take queue);
      Queue.add buf queue

   let iter = Queue.iter
   let fold = Queue.fold
end

(*
 * For converting to buffers.
 *)
let add_to_buffer queue width buf =
   Queue.iter (fun buffer ->
         Lm_rformat_html.print_html_buffer width buffer buf) queue

(*
 * The buffer.
 *)
type t =
   { info_history : string LineBuffer.t;
     info_message : buffer LineBuffer.t;
     mutable info_content : buffer
   }

(*
 * Term output is directed to the "current" buffer.
 *)
let current = ref None

(*
 * Empty buffer.
 *)
let create () =
   { info_history = LineBuffer.create ();
     info_message = LineBuffer.create ();
     info_content = new_buffer ()
   }

(*
 * Simplify invis strings.
 *)
let format_invis buf s =
   format_izone buf;
   format_string buf s;
   format_ezone buf

(*
 * Add the prompt to the output box.
 *)
let add_prompt info str =
   let buffer = new_buffer () in
      format_invis buffer "<b># ";
      format_string buffer str;
      format_invis buffer "</b><br>\n";
      LineBuffer.add info.info_message buffer;
      LineBuffer.add info.info_history str

(*
 * Capture output channels.
 *)
let add_channel message color =
   let font = Printf.sprintf "<span class=\"%s\">" color in
      (fun buf ->
            if not (Lm_rformat.buffer_is_empty buf) then
               let buffer = new_buffer () in
                  format_invis buffer font;
                  format_buffer buffer buf;
                  format_invis buffer "</span>";
                  LineBuffer.add message buffer)

(*
 * Format it.
 *)
let format_message info width buf =
   LineBuffer.iter (fun buffer ->
         Lm_rformat_html.print_html_buffer width buffer buf) info.info_message

(*
 * Create history.
 *)
let get_history info macros =
   let history_buf = Buffer.create 1024 in
   let () = Buffer.add_string history_buf "<option>--History--</option>\n" in
   let macros, _ =
      LineBuffer.fold (fun (macros, last) s ->
            if s = last then
               macros, last
            else
               let id = Printf.sprintf "id%d" (StringTable.cardinal macros) in
               let () =
                  (* BUG JYH: probably need our own escape function *)
                  Printf.bprintf history_buf "<option value=\"%s\">%s</option>\n" id s
               in
               let macros = StringTable.add macros id s in
                  macros, s) (macros, "") info.info_history
   in
      history_buf, macros

(*
 * Display a term in the window.
 *)
let set_main buf =
   match !current with
      Some info ->
         info.info_content <- buf
    | None ->
         eprintf "Browser_display_term.set_main: no current buffer@."

let format_main info width buf =
   Lm_rformat_html.print_html_buffer width info.info_content buf

(*
 * Divert output during this call.
 *)
let synchronize info f x =
   current := Some info;
   Lm_format.divert std_formatter (Some (add_channel info.info_message "stdout"));
   Lm_format.divert err_formatter (Some (add_channel info.info_message "stderr"));
   let result =
      try f x with
         exn ->
            current := None;
            Lm_format.divert std_formatter None;
            Lm_format.divert err_formatter None;
            raise exn
   in
      current := None;
      Lm_format.divert std_formatter None;
      Lm_format.divert err_formatter None;
      result

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)