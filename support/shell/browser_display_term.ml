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
   let max_queue_length = 100

   let create = Queue.create

   let add queue buf =
      if Queue.length queue = max_queue_length then
         ignore (Queue.take queue);
      Queue.add buf queue

   let iter = Queue.iter
   let fold = Queue.fold
end

let history = LineBuffer.create ()
let message = LineBuffer.create ()

let add_to_buffer queue width buf =
   Queue.iter (fun buffer ->
         Lm_rformat_html.print_html_buffer width buffer buf) queue

let message = LineBuffer.create ()

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
let add_prompt str =
   let buffer = new_buffer () in
      format_invis buffer "<b># ";
      format_string buffer str;
      format_invis buffer "</b><br>\n";
      LineBuffer.add message buffer;
      LineBuffer.add history str

(*
 * Capture output channels.
 *)
let add_channel color =
   let font = Printf.sprintf "<span class=\"%s\">" color in
      (fun buf ->
            if not (Lm_rformat.buffer_is_empty buf) then
               let buffer = new_buffer () in
                  format_invis buffer font;
                  format_buffer buffer buf;
                  format_invis buffer "</span>";
                  LineBuffer.add message buffer)

let divert () =
   Lm_format.divert std_formatter (Some (add_channel "stdout"));
   Lm_format.divert err_formatter (Some (add_channel "stderr"))

(*
 * Format it.
 *)
let format_message width buf =
   LineBuffer.iter (fun buffer ->
         Lm_rformat_html.print_html_buffer width buffer buf) message

(*
 * Create history.
 *)
let get_history macros =
   let macros_buf = Buffer.create 1024 in
   let history_buf = Buffer.create 1024 in
   let () =
      Buffer.add_string history_buf "<option><b>History</b></option>\n"
   in
   let macros =
      LineBuffer.fold (fun macros s ->
            let id = Printf.sprintf "id%d" (StringTable.cardinal macros) in
               (* BUG JYH: probably need our own escape function *)
               Printf.bprintf history_buf "<option value=\"%s\">%s</option>\n" id (String.escaped s);
               StringTable.add macros id s) macros history
   in
      StringTable.iter (fun s v ->
            (* BUG JYH: probably need our own escape function *)
            Printf.bprintf macros_buf "\tmacros[\"%s\"] = \"%s\";\n" s (String.escaped v)) macros;
      Buffer.contents macros_buf, Buffer.contents history_buf

(*
 * Display a term in the window.
 *)
let buffer = ref (new_buffer ())

let set_main buf =
   buffer := buf

let format_main width buf =
   Lm_rformat_html.print_html_buffer width !buffer buf

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
