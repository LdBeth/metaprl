(*
 * Print the function associated with a value of the PC.
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
 *
 *)

open Lm_printf

open Instruct

(*
 * I/O helper.
 *)
let eflush out =
   output_char out '\n';
   flush out

(*
 * Taken from symbols.ml
 *)
let read_symbols' bytecode_file =
  let ic = open_in_bin bytecode_file in
  let pos_trailer =
    in_channel_length ic - 24 - String.length Config.exec_magic_number in
  seek_in ic pos_trailer;
  let path_size = input_binary_int ic in
  let code_size = input_binary_int ic in
  let prim_size = input_binary_int ic in
  let data_size = input_binary_int ic in
  let symbol_size = input_binary_int ic in
  let debug_size = input_binary_int ic in
  let magic = String.create (String.length Config.exec_magic_number) in
  really_input ic magic 0 (String.length Config.exec_magic_number);
  if magic <> Config.exec_magic_number then begin
    prerr_string bytecode_file; prerr_endline " is not a bytecode file.";
    exit 2
  end;
  if debug_size = 0 then begin
    prerr_string bytecode_file; prerr_endline " has no debugging info.";
    exit 2
  end;
  seek_in ic (pos_trailer - debug_size - symbol_size);
  Symtable.restore_state (input_value ic);
  let all_events = (input_value ic : debug_event list list) in
  close_in ic;
  all_events

(*
 * Load the events.
 *)
let load_symbols file =
   let rec convert1 events = function
      hd1 :: tl1 ->
         let rec convert2 events = function
            { ev_pos = pos;
              ev_info = Event_function;
              ev_module = modname;
              ev_char = cpos
            } :: tl2 ->
               convert2 ((pos, modname, cpos) :: events) tl2
          | _ :: tl2 ->
               convert2 events tl2
          | [] ->
               convert1 events tl1
         in
            convert2 events hd1
    | [] ->
         events
   in
      List.rev (convert1 [] (read_symbols' file))

(*
 * Print the symbols.
 *)
let print_symbol (pos, modname, cpos) =
   eprintf "0x%08x: %s/%d\n" pos modname cpos

(*
 * Load the file, and dump the symbols to an output file.
 *)
let spec = []

let add_anon_arg file =
   let _ = eprintf "Loading symbols from %s%t" file eflush in
   let symbols = load_symbols file in
   let compare (pos1, _, _) (pos2, _, _) =
      pos1 < pos2
   in
   let symbols = Array.of_list (Sort.list compare symbols) in
   let _ =
      Array.iter print_symbol symbols;
      flush stderr
   in
   let outfile = file ^ ".symbols" in
   let out = open_out_bin outfile in
      output_value out symbols;
      close_out out

let _ =
   Arg.parse spec add_anon_arg "MetaPRL symbol loader"

(*
 * -*-
 * Local Variables:
 * Caml-master: "mp.run"
 * End:
 * -*-
 *)
