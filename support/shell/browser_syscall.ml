(*
 * Handle system calls in browser mode.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_thread

module Buffer =
struct
   (*
    * A string buffer that allows reading *and* writing.
    *)
   type t =
      { mutable buf_data : String.t;
        mutable buf_length : int
      }

   (*
    * Create a new buffer.
    *)
   let create n =
      { buf_data = String.create n;
        buf_length = 0
      }

   (*
    * Length of the buffer.
    *)
   let length buf =
      buf.buf_length

   (*
    * Convert contents to a string.
    *)
   let contents buf =
      let { buf_data = data;
            buf_length = length
          } = buf
      in
         String.sub data 0 length

   (*
    * Expand to buffer.  Size grows by powers of 2.
    *)
   let expand buf =
      let { buf_data = data;
            buf_length = length
          } = buf
      in
      let len = String.length data in
      let data' = String.create (2 * len) in
         String.blit data 0 data' 0 length;
         buf.buf_data <- data'

   (*
    * Add a new character.
    *)
   let rec add_char buf c =
      let { buf_data = data;
            buf_length = length
          } = buf
      in
      let len = String.length data in
         if length = len then
            begin
               expand buf;
               add_char buf c
            end
         else
            begin
               data.[length] <- c;
               buf.buf_length <- succ length
            end

   (*
    * Get a character from the buffer.
    *)
   let get_char buf i =
      let { buf_data = data;
            buf_length = length
          } = buf
      in
         if i < 0 || i >= length then
            raise (Invalid_argument "get_char");
         data.[i]
end

(*
 * Buffer for storing output from commands.
 *)
type t =
   { io_in : Unix.file_descr;
     io_pid : int;
     io_input : in_channel;
     io_buffer : Buffer.t;
     mutable io_finished : bool
   }

(*
 * Buffer for reading from the buffer incrementally.
 *)
type buffer =
   { in_io : t;
     mutable in_index  : int
   }

let cmd_exe, cmd_argv =
   match Sys.os_type with
      "Win32" ->
         let cmd_exe = "cmd.exe" in
            cmd_exe, (fun line -> [|cmd_exe; "/C"; line|])
    | "Cygwin"
    | "Unix"
    | "MacOS" ->
         let cmd_exe = "/bin/sh" in
            cmd_exe, (fun line -> [|cmd_exe; "-c"; line|])
    | s ->
         raise (Invalid_argument ("Unknown operating system: " ^ s))

(*
 * Start a new process dumping data into the buffer.
 *)
let create command =
   let fd_in, fd_out = Unix.pipe () in
   let pid = Unix.create_process cmd_exe (cmd_argv command) Unix.stdin fd_out fd_out in
   let inx = Unix.in_channel_of_descr fd_in in
      Unix.close fd_out;
      { io_in = fd_in;
        io_pid = pid;
        io_input = inx;
        io_buffer = Buffer.create 256;
        io_finished = false
      }

let close io =
   let { io_in = fd_in;
         io_pid = pid;
         io_input = inx;
         io_finished = finished
       } = io
   in
      if not finished then
         begin
            io.io_finished <- true;
            Unix.close fd_in;
            close_in inx;
            ignore (Unix.waitpid [] pid)
         end

(*
 * Flush the buffer by reading all possible data.
 *)
let flush io =
   let { io_input = inx;
         io_buffer = buf
       } = io
   in
   let rec copy () =
      let c = input_char inx in
         Buffer.add_char buf c;
         copy ()
   in
   let () =
      try copy () with
         End_of_file ->
            ()
   in
      close io

(*
 * Get the contents as a string.
 *)
let contents io =
   Buffer.contents io.io_buffer

(*
 * Open an incremental channel.
 *)
let open_in io =
   { in_io = io;
     in_index = 0
   }

(*
 * Get the next char.
 *)
let get_char inx =
   let { in_io = io;
         in_index = index
       } = inx
   in
   let { io_input = iny;
         io_buffer = buf;
         io_finished = finished
       } = io
   in
   let len = Buffer.length buf in
      if index = len then
         if finished then
            raise End_of_file
         else
            try
               let c = input_char iny in
                  Buffer.add_char buf c;
                  inx.in_index <- succ index;
                  c
            with
               End_of_file ->
                  close io;
                  raise End_of_file
      else
         let c = Buffer.get_char buf index in
            inx.in_index <- succ index;
            c

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
