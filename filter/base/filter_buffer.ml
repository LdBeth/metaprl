(*
 * An "infinite" buffer.  The buffer grows as the space requirements
 * increase.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Lm_debug

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Buffer%t"

type t =
   { mutable buf_str : bytes;
     mutable buf_index : int
   }

(*
 * Create a new empty buffer.
 *)
let create () =
   { buf_str = Bytes.create 32;
     buf_index = 0
   }

(*
 * Clear the buffer.
 *)
let clear buf =
   buf.buf_index <- 0

(*
 * Place something in the buffer,
 * and increase the buffer size if necessary.
 *)
let putc buf c =
   let { buf_str = str; buf_index = i } = buf in
      if i = Bytes.length str then
         (* Grow the buffer *)
         buf.buf_str <- Bytes.cat str (Lm_string_util.create "Buffer.putc" i);

      (* Insert the char *)
      str.[i] <- c;
      buf.buf_index <- i + 1

(*
 * Place a string in the buffer.
 *)
let puts buf s =
   let { buf_str = str; buf_index = i } = buf in
   let len = String.length s in
      if i + len > Bytes.length str then
         buf.buf_str <- Bytes.cat str (Lm_string_util.create "Buffer.puts" (i + len));

      (* Add the string *)
      Bytes.blit_string s 0 str i len;
      buf.buf_index <- i + len

(*
 * Get the contents of the buffer.
 *)
let gets { buf_str = str; buf_index = i } =
   Bytes.sub_string str 0 i

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
