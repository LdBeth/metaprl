(*
 * Operations on strings.
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

open Printf
open Mp_debug
(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading String_util%t"

let debug_string =
   create_debug (**)
      { debug_name = "string";
        debug_description = "check string bounds";
        debug_value = false
      }

(*
 * Catch errors.
 *)
let create name i =
   if !debug_string then
      if i < 0  then
         begin
            eprintf "String_util.create: %s: %d < 0%t" name i eflush;
            raise (Failure "String_util.create")
         end;
   String.create i

let make name i c =
   if !debug_string then
      if i < 0 then
         begin
            eprintf "String_util.make: %s: %d < 0%t" name i eflush;
            raise (Failure "String_util.make")
         end;
   String.make i c

let sub name s i len =
   if !debug_string then
      let len' = String.length s in
         if i >= 0 & len >= 0 & i + len < len' then
            String.sub s i len
         else
            begin
               eprintf "String_util.sub error: %s: %s.[%d]%t" name s i eflush;
               raise (Failure "String_util.sub")
            end
   else
      String.sub s i len

let blit name froms i tos j len =
   if !debug_string then
      let from_len = String.length froms in
      let to_len = String.length tos in
         if i >= 0 & j >= 0 & len >= 0 & i + len < from_len & j + len < to_len then
            String.blit froms i tos j len
         else
            begin
               eprintf "String_util.blit_error: %s: %s %d %s %d %d%t" name froms i tos j len eflush;
               raise (Failure "String_util.blit")
            end
   else
      String.blit froms i tos j len

let set name s i c =
   if !debug_string then
      let len = String.length s in
         if i >= 0 & i < len then
            String.set s i c
         else
            begin
               eprintf "String_util.set error: %s: %s.[%d] <- %c%t" name s i c eflush;
               raise (Failure "String_util.set")
            end
   else
      String.set s i c

let get name s i =
   let len = String.length s in
      if i >= 0 & i < len then
         String.get s i
      else
         begin
            eprintf "String_util.get error: %s: %s[%d]%t" name s i eflush;
            raise (Failure "String_util.get")
         end

(*
 * Check all chars in the string.
 *)
let for_all f s =
   let len = String.length s in
   let rec check i =
      (i = len) or (f s.[i] & check (succ i))
   in
      check 0

(*
 * Index of first char in a set.
 *)
let index_set s set =
   let len = String.length s in
   let rec loop i =
      if i = len then
         raise Not_found
      else
         let c = s.[i] in
            if String.contains set c then
               i
            else
               loop (succ i)
   in
      loop 0

let rindex_set s set =
   let rec loop i =
      if i < 0 then
         raise Not_found
      else
         let c = s.[i] in
            if String.contains set c then
               i
            else
               loop (i - 1)
   in
      loop (String.length s - 1)

(*
 * Split a string at a particular char.
 *)
let split c s =
   let len = String.length s in
   let rec loop i j =
      if j = len then
         if i = j then
            []
         else
            [String.sub s i (j - i)]
      else if s.[j] = c then
         if i = j then
            loop (succ j) (succ j)
         else
            (String.sub s i (j - i)) :: (loop (succ j) (succ j))
      else
         loop i (succ j)
   in
      loop 0 0

(*
 * Split a string at a particular char.
 *)
let split_set c s =
   let len = String.length s in
   let rec loop i j =
      if j = len then
         if i = j then
            []
         else
            [String.sub s i (j - i)]
      else if String.contains c s.[j] then
         if i = j then
            loop (succ j) (succ j)
         else
            (String.sub s i (j - i)) :: (loop (succ j) (succ j))
      else
         loop i (succ j)
   in
      loop 0 0

let code0 = Char.code '0'
let codea = Char.code 'a'
let codeA = Char.code 'A'

(*
 * Turn a string into an argument list.
 *)
let parse_args line =
   let len = String.length line in
   let buf = String.create len in
   let rec skip i =
      if i = len then
         []
      else
         match line.[i] with
            ' ' | '\t' | '\n' | '\r' ->
               skip (succ i)
          | '"' ->
               string 0 (succ i)
          | _ ->
               collect i (succ i)
   and collect i j =
      if j = len then
         [String.sub line i (j - i)]
      else
         match line.[j] with
            ' ' | '\t' | '\n' | '\r' ->
               let s = String.sub line i (j - i) in
                  s :: (skip (succ j))
          | _ ->
               collect i (succ j)
   and string j k =
      if k = len then
         raise (Invalid_argument ("String_util.parse_args: " ^ line))
         (* [String.sub buf 0 j] *)
      else
         let c = line.[k] in
            if c = '"' then
               let s = String.sub buf 0 j in
                  s :: (skip (succ k))
            else if c = '\\' then
               escape j (succ k)
            else
               begin
                  buf.[j] <- c;
                  string (succ j) (succ k)
               end
   and escape j k =
      if k = len then
         raise (Invalid_argument ("String_util.parse_args: " ^ line))
         (* [String.sub buf 0 j] *)
      else
         let c,k =
            match line.[k] with
               't' -> '\t', succ k
             | 'n' -> '\n', succ k
             | 'r' -> '\r', succ k
             | '\\' -> '\\', succ k
             | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') as c ->
                  Char.chr (100 * (Char.code c) +
                            10 * (Char.code line.[succ k]) +
                            (Char.code line.[k+2]) - 111 * code0),
                  k+3
             | c -> c, succ k
         in
            buf.[j] <- c;
            string (succ j) k
   in
   let _ =
      if !debug_string then
         eprintf "String_util.get_args: %s%t" (String.escaped line) eflush
   in
   let args = skip 0 in
      if !debug_string then
         eprintf "String_util.get_args: done%t" eflush;
      args

(*
 * Use hex notation.
 *)
let hexify s =
   let len = String.length s in
   let rec hexify i =
      if i < len then
         (sprintf "%02x" (Char.code s.[i])) ^ (hexify (succ i))
      else
         ""
   in
      hexify 0

let unhex i =
   match i with
      '0' .. '9' ->
         (Char.code i) - code0
    | 'a' .. 'f' ->
         (Char.code i) - codea + 10
    | 'A' .. 'F' ->
         (Char.code i) - codeA + 10
    | _ ->
         raise (Failure "unhexify")

let unhexify s =
   let len = String.length s in
      if len mod 2 = 0 then
         let buf = create "String_util.unhexify" (len / 2) in
         let rec unhexify i j =
            if j < len then
               begin
                  buf.[i] <- Char.chr ((unhex s.[j]) * 16 + (unhex s.[succ j]));
                  unhexify (i + 1) (j + 2)
               end
         in
            unhexify 0 0;
            buf
      else
         raise (Failure "unhexify")

let unhexify_int s =
   let len = String.length s in
   let rec unhexify index i =
      if i < len then
         unhexify (index * 16 + (unhex s.[i])) (succ i)
      else
         index
   in
      unhexify 0 0

(*
 * Locale functions
 *)
external set_locale: unit -> unit = "set_locale"
external is_print: char -> bool = "is_print"
external is_digit: char -> bool = "is_digit"
external is_alnum: char -> bool = "is_alnum"
external is_upper: char -> bool = "is_upper"
external is_graph: char -> bool = "is_graph"

let _ = set_locale ()

let is_capitalized s = is_upper s.[0]

(*
 * Functions to quote and unquote strings.
 *)
let rec is_simple l i s =
   if i = l then
      true
   else
      match String.unsafe_get s i with
         '"' | '\\' | '\r' | '\n' | '\t' | ' ' -> false
       | c ->
         is_print c && is_simple l (succ i) s

let quote s =
   if s <> "" && is_simple (String.length s) 0 s then
      s
   else
      "\"" ^ (String.escaped s) ^ "\""

let newname v i =
   v ^ "_" ^ (string_of_int i)

let rec new_var v avoid i =
   let v' = newname v i in
   if avoid v'
      then new_var v avoid (succ i)
      else v'

let vnewname v avoid =
   let v = if String.contains v '_' then String.sub v 0 (String.rindex v '_') else v in
      new_var v avoid 1

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
