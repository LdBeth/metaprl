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

(*
 * Line-based buffer.
 *)
module type LineBufferSig =
sig
   type t

   val create : unit -> t
   val add_buffer : t -> Buffer.t -> unit
   val add_to_buffer : t -> Buffer.t -> unit
end

module LineBuffer : LineBufferSig =
struct
   type t = string Queue.t

   let max_queue_length = 100

   let create = Queue.create

   let add_buffer queue buf =
      if Queue.length queue = max_queue_length then
         ignore (Queue.take queue);
      Queue.add (Buffer.contents buf) queue

   let add_to_buffer queue buf =
      Queue.iter (fun s ->
            Buffer.add_string buf s;
            Buffer.add_string buf "<br>\n") queue
end

let message = LineBuffer.create ()

(*
 * The display buffer is global.
 *)
let buffer = Buffer.create 1024

(*
 * Set the rule text.
 *)
let set_message width buf =
   let buffer = Buffer.create 100 in
      Lm_rformat_html.print_html_buffer width buf buffer;
      LineBuffer.add_buffer message buffer

let set_message_string str =
   let buffer = Buffer.create 100 in
      Buffer.add_string buffer "<b>";
      Buffer.add_string buffer str;
      Buffer.add_string buffer "</b>";
      LineBuffer.add_buffer message buffer

let format_message buf =
   LineBuffer.add_to_buffer message buf

(*
 * Display a term in the window.
 *)
let set_main width buf =
   Buffer.clear buffer;
   Lm_rformat_html.print_html_buffer width buf buffer

let format_main buf =
   Buffer.add_buffer buf buffer

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
