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
open Lm_rformat
open Lm_format

(*
 * Line-based buffer.
 *)
module type LineBufferSig =
sig
   type t

   val create : unit -> t
   val add_buffer : t -> buffer -> unit
   val add_to_buffer : t -> int -> Buffer.t -> unit
end

module LineBuffer : LineBufferSig =
struct
   type t = Lm_rformat.buffer Queue.t

   let max_queue_length = 100

   let create = Queue.create

   let add_buffer queue buf =
      if Queue.length queue = max_queue_length then
         ignore (Queue.take queue);
      Queue.add buf queue

   let add_to_buffer queue width buf =
      Queue.iter (fun buffer ->
            Lm_rformat_html.print_html_buffer width buffer buf) queue
end

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
      format_invis buffer "<b>";
      format_string buffer str;
      format_invis buffer "</b><br>\n";
      LineBuffer.add_buffer message buffer

(*
 * Capture output channels.
 *)
let add_channel color =
   let font = Printf.sprintf "<font class=\"%s\">" color in
      (fun buf ->
            if not (Lm_rformat.buffer_is_empty buf) then
               let buffer = new_buffer () in
                  format_invis buffer font;
                  format_buffer buffer buf;
                  format_invis buffer "</font>";
                  LineBuffer.add_buffer message buffer)

let divert () =
   Lm_format.divert std_formatter (Some (add_channel "stdout"));
   Lm_format.divert err_formatter (Some (add_channel "stderr"))

(*
 * Format it.
 *)
let format_message width buf =
   LineBuffer.add_to_buffer message width buf

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
