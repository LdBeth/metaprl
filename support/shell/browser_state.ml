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
open Lm_rformat_html
open Lm_thread

open Refiner.Refiner.Term

open Shell_util

(************************************************************************
 * A simple bounded buffer.
 *)
module type LineBufferSig =
sig
   type 'a t

   val create : unit -> 'a t
   val add : 'a t -> 'a -> unit
   val iter : ('a -> unit) -> 'a t -> unit
   val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
   val last : 'a t -> 'a option
   val remove_last : 'a t -> unit
   val length : 'a t -> int
end

module LineBuffer : LineBufferSig =
struct
   type 'a t =
      { mutable lines  : 'a option array;
        mutable first  : int;
        mutable length : int
      }

   let max_queue_length = 100

   let create () =
      { lines = Array.create max_queue_length None;
        first = 0;
        length = 0
      }

   let length queue =
      queue.length

   let add queue buf =
      let { lines = lines;
            first = first;
            length = length
          } = queue
      in
      let alength = Array.length lines in
      let last = (first + length) mod alength in
         lines.(last) <- Some buf;
         if length = alength then
            queue.first <- (succ first) mod alength
         else
            queue.length <- succ length

   let last queue =
      let { lines = lines;
            first = first;
            length = length
          } = queue
      in
         if length = 0 then
            None
         else
            let alength = Array.length lines in
            let last = (first + length - 1) mod alength in
               lines.(last)

   let remove_last queue =
      let { length = length } = queue in
         if length <> 0 then
            queue.length <- pred length

   let iter f queue =
      let { lines = lines;
            first = first;
            length = length
          } = queue
      in
      let alength = Array.length lines in
         for i = 0 to length do
            let j = (first + i) mod alength in
               match lines.(j) with
                  Some x -> f x
                | None -> ()
         done

   let fold f x queue =
      let { lines  = lines;
            first  = first;
            length = length
          } = queue
      in
      let alength = Array.length lines in
      let rec collect x i =
         if i = length then
            x
         else
            let j = (first + i) mod alength in
            let x =
               match lines.(j) with
                  Some y ->
                     f x y
                | None ->
                     x
            in
               collect x (succ i)
      in
         collect x 0
end

(*
 * Save the contents of a (string LineBuffer.t) to a file in the user's
 * home directory.
 *)
let save_stringbuffer_to_home queue name =
   try
      let filename = Filename.concat (Setup.home ()) name in
      let out = Pervasives.open_out filename in
         LineBuffer.iter (fun s ->
               Printf.fprintf out "%s\n" (String.escaped s)) queue;
         close_out out
   with
      Sys_error s ->
         eprintf "Can't write %s@." s

(*
 * Add the contents of a file to a (string LineBuffer.t)
 *)
let add_stringbuffer_from_home queue name =
   try
      let filename = Filename.concat (Setup.home ()) name in
      let inx = Pervasives.open_in filename in
      let rec read () =
         let line =
            try Some (input_line inx) with
               End_of_file ->
                  None
         in
            match line with
               Some line ->
                  LineBuffer.add queue (Lm_string_util.unescape line);
                  read ()
             | None ->
                  ()
      in
         read ();
         close_in inx
   with
      Sys_error s ->
         ()

(*
 * Read/write directories and history.
 *)
let read_stringbuffer_from_home name =
   let queue = LineBuffer.create () in
      add_stringbuffer_from_home queue name;
      queue

(************************************************************************
 * Directories are maintained as a StringTable.t
 *)
module type LineTableSig =
sig
   type 'a t

   val empty  : 'a t
   val mem    : 'a t -> string -> bool
   val add    : 'a t -> string -> 'a -> 'a t
   val iter   : (string -> 'a -> unit) -> 'a t -> unit
   val fold   : ('a -> string -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

module LineTable : LineTableSig =
struct
   (*
    * The float is the time the entry was added.
    *)
   type 'a t = (float * 'a) StringTable.t

   (*
    * Length is hardcoded for now.
    *)
   let max_queue_length = 10

   (*
    * Empty queue.
    *)
   let empty = StringTable.empty

   (*
    * Remove the oldest element.
    *)
   let remove_oldest table =
      let key =
         StringTable.fold (fun oldest key (time, _) ->
               match oldest with
                  Some (time', _) ->
                     if time < time' then
                        Some (time, key)
                     else
                        oldest
                | None ->
                     Some (time, key)) None table
      in
         match key with
            Some (_, key) ->
               StringTable.remove table key
          | None ->
               table

   (*
    * Test for membership.
    *)
   let mem = StringTable.mem

   (*
    * Add an element to the table.
    *)
   let add table key x =
      let time = Unix.gettimeofday () in
      let table = StringTable.add table key (time, x) in
         if StringTable.cardinal table > max_queue_length then
            remove_oldest table
         else
            table

   (*
    * Iterate over the queue.
    *)
   let iter f table =
      StringTable.iter (fun key (_, x) ->
            f key x) table

   (*
    * Fold over the queue.
    *)
   let fold f x table =
      StringTable.fold (fun x key (_, y) ->
            f x key y) x table
end

(*
 * Save the contents of a (string LineBuffer.t) to a file in the user's
 * home directory.
 *)
let save_stringtable_to_home queue name =
   try
      let filename = Filename.concat (Setup.home ()) name in
      let out = Pervasives.open_out filename in
         LineTable.iter (fun dir subdir ->
               Printf.fprintf out "%s#%s\n" dir subdir) queue;
         close_out out
   with
      Sys_error s ->
         eprintf "Can't write %s@." s

(*
 * Add the contents of a file to a (string LineBuffer.t)
 *)
let read_stringtable_from_home name =
   try
      let filename = Filename.concat (Setup.home ()) name in
      let inx = Pervasives.open_in filename in
      let rec read table =
         let line =
            try Some (input_line inx) with
               End_of_file ->
                  None
         in
            match line with
               Some line ->
                  let dir, subdir =
                     try
                        let index = String.index line '#' in
                        let dir = String.sub line 0 index in
                        let subdir = String.sub line (succ index) (String.length line - index - 1) in
                           dir, subdir
                     with
                        Not_found ->
                           line, ""
                  in
                     read (LineTable.add table dir subdir)
             | None ->
                  table
      in
      let table = read LineTable.empty in
         close_in inx;
         table
   with
      Sys_error s ->
         LineTable.empty

(*
 * Save the ls_options to a file in the user's
 * home directory.
 *)
let save_options_to_home options name =
   try
      let filename = Filename.concat (Setup.home ()) name in
      let out = Pervasives.open_out filename in
         output_string out (string_of_ls_options options);
         close_out out
   with
      Sys_error s ->
         eprintf "Can't write %s@." s

(*
 * Save the ls_options to a file in the user's
 * home directory.
 *)
let read_options_from_home name =
   try
      let filename = Filename.concat (Setup.home ()) name in
      let inx = Pervasives.open_in filename in
      let options =
         try ls_options_of_string (input_line inx) with
            End_of_file ->
               ls_options_default
      in
         close_in inx;
         options
   with
      Sys_error s ->
         ls_options_default

(************************************************************************
 * The browser state.
 *)

(*
 * The buffer.
 *)
type info =
   { info_history                : string LineBuffer.t;
     mutable info_directories    : string LineTable.t;
     mutable info_files          : string LineTable.t;
     info_message                : buffer LineBuffer.t;
     mutable info_content_buffer : buffer;
     mutable info_content_table  : term StringTable.t;
     mutable info_options        : LsOptionSet.t
   }

type t = info State.entry

(*
 * Names of the history files.
 *)
let history_filename     = "history"
let files_filename       = "files"
let directories_filename = "directories"
let options_filename     = "options"

(*
 * Term output is directed to the "current" buffer.
 *)
let current_entry =
   State.shared_val "Browser_state.current" (ref None)

(*
 * Empty buffer.
 *)
let create () =
   let info =
      { info_history        = read_stringbuffer_from_home history_filename;
        info_directories    = read_stringtable_from_home directories_filename;
        info_files          = read_stringtable_from_home files_filename;
        info_message        = LineBuffer.create ();
        info_content_buffer = new_buffer ();
        info_content_table  = StringTable.empty;
        info_options        = read_options_from_home options_filename
      }
   in
      State.shared_val "Browser_state.create" info

(*
 * Set the options.
 *)
let set_options info_entry options =
   State.write info_entry (fun info ->
         info.info_options <- options;
         save_options_to_home options options_filename)

let get_options info_entry =
   State.read info_entry (fun info -> info.info_options)

(*
 * Get the HTML tagger from the current state.
 *)
let get_tagger info =
   let tagger =
      if LsOptionSet.mem info.info_options LsHandles then
         (fun s -> Printf.sprintf "<span class=\"slot\" id=\"%s\">&#8227;" s)
      else
         (fun s -> Printf.sprintf "<span class=\"slot\" id=\"%s\">" s)
   in
      Some { html_tag_begin = FunTagger tagger;
             html_tag_end = StringTagger "</span>"
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
let add_prompt info_entry str =
   State.write info_entry (fun info ->
         let buffer = new_buffer () in
            format_invis buffer "<b># ";
            format_string buffer str;
            format_invis buffer "</b><br>\n";
            LineBuffer.add info.info_message buffer;

            (* Add to the history only if the command is different *)
            (match LineBuffer.last info.info_history with
                Some line ->
                   if str <> line then
                      LineBuffer.add info.info_history str
              | None ->
                   LineBuffer.add info.info_history str);
            save_stringbuffer_to_home info.info_history history_filename)

(*
 * Add a file.
 *)
let rec resolve_symlink filename =
   let stat = Unix.lstat filename in
      if stat.Unix.st_kind = Unix.S_LNK then
         resolve_symlink (Unix.readlink filename)
      else if stat.Unix.st_kind = Unix.S_REG then
         filename
      else
         raise Not_found

let resolve_symlink filename =
   try Some (resolve_symlink filename) with
      Unix.Unix_error _
    | Not_found ->
         None

let strip_root filename =
   match filename with
      Some filename ->
          let root = (Setup.root()) ^ "/" in
          let root_len = String.length root in
          let file_len = String.length filename in
          let convert c =
             if c = '\\' then
                '/'
             else
                c
          in
          let rec matches i =
             if i = root_len then
                true
             else
                convert root.[i] = convert filename.[i] && matches (succ i)
          in
          let file =
             if root_len < file_len && matches 0 then
                String.sub filename root_len (file_len - root_len)
             else
                filename
          in
             (* eprintf "Root: %s Filename: %s Stripped: %s\n%t" root filename file flush; *)
             Some file
    | None ->
         None

let add_edit_internal info name =
   info.info_files <- LineTable.add info.info_files name ""

let add_filename_internal info name =
   match name with
      Some name ->
         add_edit_internal info name
    | None ->
         ()

let add_file_internal info file =
   match file with
      Some name ->
         let basename =
            try String.sub name 0 (String.rindex name '.') with
               Not_found ->
                  name
         in
         let ml_name = strip_root (resolve_symlink (basename ^ ".ml")) in
         let mli_name = strip_root (resolve_symlink (basename ^ ".mli")) in
            add_filename_internal info ml_name;
            add_filename_internal info mli_name;
            save_stringtable_to_home info.info_files files_filename
    | None ->
         ()

let add_edit info_entry name =
   State.write info_entry (fun info ->
         add_edit_internal info name)

let add_filename info_entry name =
   State.write info_entry (fun info ->
         add_filename_internal info name)

let add_file info_entry file =
   State.write info_entry (fun info ->
         add_file_internal info file)

(*
 * Add a directory.  For the buffer, remember the main part of the directory
 * as well as the current location.
 *)
let add_directory info_entry str =
   State.write info_entry (fun info ->
         let dirs = info.info_directories in

         (* Parse the filename *)
         let path =
            match Lm_string_util.split "/" str with
               [""; ""] ->
                  []
             | "" :: path ->
                  path
             | path ->
                  path
         in
            match path with
               [] ->
                  ()
             | [dir] ->
                  if not (LineTable.mem info.info_directories dir) then
                     info.info_directories <- LineTable.add info.info_directories dir "";
                  save_stringtable_to_home info.info_directories directories_filename
             | dir :: subdir ->
                  info.info_directories <- LineTable.add info.info_directories dir (String.concat "/" subdir);
                  save_stringtable_to_home info.info_directories directories_filename)

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
let format_message info_entry width buf =
   State.read info_entry (fun info ->
         let tagger = get_tagger info in
            LineBuffer.iter (fun buffer ->
                  Lm_rformat_html.print_html_buffer width tagger buffer buf) info.info_message)

(*
 * Get the history.
 *)
let get_history info_entry =
   State.read info_entry (fun info ->
         let history =
            LineBuffer.fold (fun lines line ->
                  line :: lines) [] info.info_history
         in
            List.rev history)

(*
 * Get the files.
 *)
let get_files info_entry =
   State.read info_entry (fun info ->
         let files =
            LineTable.fold (fun files file _ ->
                  file :: files) [] info.info_files
         in
            List.rev files)

(*
 * Get the directories.
 *)
let get_directories info_entry =
   State.read info_entry (fun info ->
         let dirs =
            LineTable.fold (fun lines dir subdir ->
                  if subdir = "" then
                     ("/" ^ dir) :: lines
                  else
                     (sprintf "/%s/%s" dir subdir) :: ("/" ^ dir) :: lines) [".."; "~"; "/"] info.info_directories
         in
            List.rev dirs)

(*
 * Get the term matched by the id.
 *)
let get_term info_entry id =
   State.read info_entry (fun info ->
         StringTable.find info.info_content_table id)

(*
 * Display a term in the window.
 *)
let set_main buf terms =
   let current = State.get current_entry in
      match !current with
         Some info ->
            info.info_content_buffer <- buf;
            info.info_content_table <- terms
       | None ->
            eprintf "Browser_state.set_main: no current buffer@."

let format_main info_entry width buf =
   State.read info_entry (fun info ->
         let tagger = get_tagger info in
            Lm_rformat_html.print_html_buffer width tagger info.info_content_buffer buf)

(*
 * Divert output during this call.
 *)
let synchronize info_entry f x =
   State.write current_entry (fun current ->
   State.write info_entry (fun info ->
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
            result))

(*
 * Currently, we save the history on every command,
 * so there is no need to flush.
 *)
let flush info =
   ()

(*
 * Perform the command, and add the output to the message window.
 *)
let add_html_string buf s =
   let len = String.length s in
   let rec copy col i =
      if i < len then
         let col =
            match s.[i] with
               '<' -> Buffer.add_string buf "&lt;"; succ col
             | '>' -> Buffer.add_string buf "&gt;"; succ col
             | '&' -> Buffer.add_string buf "&amp;"; succ col
             | ' ' -> Buffer.add_string buf "&nbsp;"; succ col
             | '\r'
             | '\n' -> Buffer.add_string buf "<br>\n"; 0
             | '\t' ->
                  let col' = (col + 8) land (lnot 7) in
                     for i = col to pred col' do
                        Buffer.add_string buf "&nbsp;"
                     done;
                     col'
             | c -> Buffer.add_char buf c; succ col
         in
            copy col (succ i)
   in
      copy 0 0

let add_command info_entry io =
   State.write info_entry (fun info ->
         let s =
            Browser_syscall.flush io;
            Browser_syscall.contents io
         in
         let buf = Buffer.create (String.length s * 2) in
         let buffer = new_buffer () in
            Buffer.add_string buf "<span class=\"system\">\n";
            add_html_string buf s;
            Buffer.add_string buf "\n</span>\n";
            format_invis buffer (Buffer.contents buf);
            LineBuffer.add info.info_message buffer)

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
