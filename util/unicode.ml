(*
 * Generate a macro file for unicode.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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

(*
 * Output a unicode code in utf8.
 * This is the general form, that takes an output
 * function.
 *)
let output_utf8_gen output_char outx i =
   if i < 0 then
      raise (Invalid_argument "output_utf8")
   else if i < 0x80 then
      output_char outx (Char.chr i)
   else if i < 0x800 then
      begin
         output_char outx (Char.chr (0xc0 lor (i lsr 6)));
         output_char outx (Char.chr (0x80 lor (i land 0x3f)))
      end
   else if i < 0x10000 then
      begin
         output_char outx (Char.chr (0xe0 lor (i lsr 12)));
         output_char outx (Char.chr (0x80 lor ((i lsr 6) land 0x3f)));
         output_char outx (Char.chr (0x80 lor (i land 0x3f)))
      end
   else if i < 0x200000 then
      begin
         output_char outx (Char.chr (0xf0 lor (i lsr 18)));
         output_char outx (Char.chr (0x80 lor ((i lsr 12) land 0x3f)));
         output_char outx (Char.chr (0x80 lor ((i lsr 6) land 0x3f)));
         output_char outx (Char.chr (0x80 lor (i land 0x3f)))
      end
   else
      raise (Invalid_argument "output_utf8")

(*
 * Channel output.
 *)
let output_utf8 = output_utf8_gen output_char

(*
 * Output as escapes.
 *)
let escape_char outx c =
   if c > ' ' && c <= '\127' then
      output_char outx c
   else
      fprintf outx "\\%03d" (Char.code c)

let escape_utf8 = output_utf8_gen escape_char

(*
 * Input utf8 from a channel.
 *)
let input_utf8 index inx =
   let i = Char.code (input_char inx) in
      match i lsr 4 with
         12
       | 13 ->
            let byte1 = i land 0x1f in
            let byte2 = (Char.code (input_char inx)) land 0x3f in
               index + 2, (byte1 lsl 6) lor byte2
       | 14 ->
            let byte1 = i land 0xf in
            let byte2 = (Char.code (input_char inx)) land 0x3f in
            let byte3 = (Char.code (input_char inx)) land 0x3f in
               index + 3, (byte1 lsl 12) lor (byte2 lsl 6) lor byte3
       | 15 ->
            let byte1 = i land 0x7 in
            let byte2 = (Char.code (input_char inx)) land 0x3f in
            let byte3 = (Char.code (input_char inx)) land 0x3f in
            let byte4 = (Char.code (input_char inx)) land 0x3f in
               index + 4, (byte1 lsl 18) lor (byte2 lsl 12) lor (byte3 lsl 6) lor byte3
       | 8
       | 9
       | 10
       | 11 ->
            raise (Failure (sprintf "input_utf8: lost synchronization at char %d, code = %03d" index i))
       | _ ->
            index + 1, i

(*
 * This function copies input to output, replacing non-ASCII
 * Unicode chartacters with escaped sequences.
 *)
let nl = Char.code '\n'

let rec copy index inx codes outx =
   let index, i = input_utf8 index inx in
      if i = nl then
         begin
            flush_codes outx codes;
            copy index inx [] outx
         end
      else if i < 0x80 then
         begin
            output_char outx (Char.chr i);
            copy index inx codes outx
         end
      else
         begin
            escape_utf8 outx i;
            copy index inx (i :: codes) outx
         end

and flush_codes outx codes =
   if codes <> [] then
      let codes = List.rev codes in
         output_string outx "\n\t\t(* ";
         List.iter (fun code -> fprintf outx "\\u%04d" code) codes;
         output_string outx " = ";
         List.iter (output_utf8 outx) codes;
         output_string outx " *)\n"
   else
      output_char outx '\n'

let copy inx outx =
   try copy 1 inx [] outx with
      End_of_file ->
         ()

(*
 * Main function does a copy.
 *)
let _ = copy stdin stdout

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
