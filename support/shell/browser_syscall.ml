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
      { mutable buf_data : Bytes.t;
        mutable buf_length : int
      }

   (*
    * Create a new buffer.
    *)
   let create n =
      { buf_data = Bytes.create n;
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
         Bytes.sub_string data 0 length

   (*
    * Expand to buffer.  Size grows by powers of 2.
    *)
   let expand buf =
      let { buf_data = data;
            buf_length = length
          } = buf
      in
      let len = Bytes.length data in
      let data' = Bytes.create (2 * len) in
         Bytes.blit data 0 data' 0 length;
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

   (*
    * Clear the buffer.
    *)
   let clear buf =
      buf.buf_length <- 0
end

(*
 * Buffer for storing output from commands.
 *)
type command =
   { command_in : Unix.file_descr;
     command_pid : int;
     command_input : in_channel
   }

type process =
   ProcessRunning of command
 | ProcessIdle

type io =
   { mutable io_process : process;
     mutable io_command : string;
     mutable io_version : int;
     io_buffer : Buffer.t
   }

type t = io State.entry

(*
 * Buffer for reading from the buffer incrementally.
 *)
type buffer =
   { in_io_entry : t;
     mutable in_index : int
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
let create () =
   let default =
      { io_process = ProcessIdle;
        io_command = "No Command";
        io_version = 0;
        io_buffer = Buffer.create 256
      }
   in
      State.shared_val "Browser_syscall.create" default

let add_char io_entry c =
   State.write io_entry (fun io ->
         Buffer.add_char io.io_buffer c)

let close io_entry =
    State.write io_entry (fun io ->
         eprintf "Closing: %s@." io.io_command;
         match io.io_process with
            ProcessRunning command ->
               let { command_in = fd_in;
                     command_pid = pid;
                     command_input = inx
                   } = command
               in
                  Unix.close fd_in;
                  close_in inx;

                  (*
                   * BUG JYH: Linux *says* that any thread can call waitpid,
                   * but it isn't true.
                   *)
                  (try ignore (Unix.waitpid [] pid) with
                      Unix.Unix_error _ ->
                         ());

                  io.io_process <- ProcessIdle
          | ProcessIdle ->
               ())

let start io_entry command_string =
   close io_entry;
   State.write io_entry (fun io ->
         let fd_in, fd_out = Unix.pipe () in
         let () = Unix.set_close_on_exec fd_in in
         let pid = Unix.create_process cmd_exe (cmd_argv command_string) Unix.stdin fd_out fd_out in
         let inx = Unix.in_channel_of_descr fd_in in
         let command =
            { command_in = fd_in;
              command_pid = pid;
              command_input = inx
            }
         in
            eprintf "Command: %s@." command_string;
            Unix.close fd_out;
            Buffer.clear io.io_buffer;
            io.io_command <- command_string;
            io.io_version <- succ io.io_version;
            io.io_process <- ProcessRunning command)

(*
 * Flush the buffer by reading all possible data.
 *)
let flush io_entry =
   State.write io_entry (fun io ->
         let { io_process = process;
               io_buffer = buf;
               _
             } = io
         in
            match process with
               ProcessRunning { command_input = inx; _ } ->
                  let rec copy () =
                     let c = input_char inx in
                        Buffer.add_char buf c;
                        copy ()
                  in
                     (try copy () with
                         End_of_file ->
                            ());
                     close io_entry
             | ProcessIdle ->
                  ())

(*
 * Get the contents as a string.
 *)
let contents io_entry =
   State.read io_entry (fun io ->
         Buffer.contents io.io_buffer)

(*
 * Command name.
 *)
let command io_entry =
   State.read io_entry (fun io ->
         io.io_command)

let set_command io_entry command =
   State.write io_entry (fun io ->
         io.io_command <- command)

(*
 * Version number.
 *)
let version io_entry =
   State.read io_entry (fun io ->
         io.io_version)

(*
 * Open an incremental channel.
 *)
let open_in io_entry =
   { in_io_entry = io_entry;
     in_index = 0
   }

(*
 * Get the next char.
 *)
let get_char inx =
   let { in_io_entry = io_entry;
         in_index = index
       } = inx
   in
      State.write io_entry (fun io ->
            let { io_process = process;
                  io_buffer = buf;
                  _
                } = io
            in
            let len = Buffer.length buf in
               if index >= len then
                  match process with
                     ProcessRunning { command_input = iny; _ } ->
                        (try
                            let c = input_char iny in
                               Buffer.add_char buf c;
                               inx.in_index <- succ index;
                               c
                         with
                            End_of_file ->
                               close io_entry;
                               raise End_of_file)
                   | ProcessIdle ->
                        raise End_of_file
               else
                  let c = Buffer.get_char buf index in
                     inx.in_index <- succ index;
                     c)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
