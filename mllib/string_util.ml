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
   if !debug_load then
      eprintf "Loading String_util%t" eflush

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
 * Find a char in a string.
 *)
let strchr s c =
   let l = String.length s in
   let rec aux i =
      if i < l then
         if s.[i] = c then
            i
         else
            aux (i + 1)
      else
         raise Not_found
   in
      aux 0

(*
 * Check all chars in the string.
 *)
let for_all f s =
   let len = String.length s in
   let rec check i =
      (i = len) or (f s.[i] & check (i + 1))
   in
      check 0

(*
 * Check if a char is in a string.
 *)
let mem c s =
   let len = String.length s in
   let rec loop i =
      (i != len) & (c = s.[i] or loop (i + 1))
   in
      loop 0

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
            if mem c set then
               i
            else
               loop (i + 1)
   in
      loop 0

let rindex_set s set =
   let rec loop i =
      if i < 0 then
         raise Not_found
      else
         let c = s.[i] in
            if mem c set then
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
            loop (j + 1) (j + 1)
         else
            (String.sub s i (j - i)) :: (loop (j + 1) (j + 1))
      else
         loop i (j + 1)
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
      else if mem s.[j] c then
         if i = j then
            loop (j + 1) (j + 1)
         else
            (String.sub s i (j - i)) :: (loop (j + 1) (j + 1))
      else
         loop i (j + 1)
   in
      loop 0 0

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
               skip (i + 1)
          | '"' ->
               string 0 (i + 1)
          | _ ->
               collect i (i + 1)
   and collect i j =
      if j = len then
         [String.sub line i (j - i)]
      else
         match line.[j] with
            ' ' | '\t' | '\n' | '\r' ->
               let s = String.sub line i (j - i) in
                  s :: (skip (j + 1))
          | _ ->
               collect i (j + 1)
   and string j k =
      if k = len then
         [String.sub buf 0 j]
      else
         let c = line.[k] in
            if c = '"' then
               let s = String.sub buf 0 j in
                  s :: (skip (k + 1))
            else if c = '\\' then
               escape j (k + 1)
            else
               begin
                  buf.[j] <- c;
                  string (j + 1) (k + 1)
               end
   and escape j k =
       if k = len then
           [String.sub buf 0 j]
       else
           let c =
              match line.[k] with
                 't' -> '\t'
               | 'n' -> '\n'
               | 'r' -> '\r'
               | '\\' -> '\\'
               | c -> c
           in
              buf.[j] <- c;
              string (j + 1) (k + 1)
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
 * Concatenate strings.
 *)
let rec concat s = function
   [h] -> h
 | h::t ->
      h ^ s ^ (concat s t)
 | [] ->
      ""

(*
 * Use hex notation.
 *)
let hexify s =
   let len = String.length s in
   let rec hexify i =
      if i < len then
         (sprintf "%02x" (Char.code s.[i])) ^ (hexify (i + 1))
      else
         ""
   in
      hexify 0

let unhex i =
   match i with
      '0' .. '9' ->
         (Char.code i) - (Char.code '0')
    | 'a' .. 'f' ->
         (Char.code i) - (Char.code 'a') + 10
    | 'A' .. 'F' ->
         (Char.code i) - (Char.code 'A') + 10
    | _ ->
         raise (Failure "unhexify")

let unhexify s =
   let len = String.length s in
      if len mod 2 = 0 then
         let buf = create "String_util.unhexify" (len / 2) in
         let rec unhexify i j =
            if j < len then
               begin
                  buf.[i] <- Char.chr ((unhex s.[j]) * 16 + (unhex s.[j + 1]));
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
      unhexify 0 0;

(*
 * Functions to quote and unquote strings.
 *)
external is_printable: char -> bool = "is_printable"

let rec is_simple l i s =
   if i = l then
      true
   else
      match String.unsafe_get s i with
         '"' | '\\' | '\n' | '\t' | ' ' -> false
       | c ->
         is_printable c && is_simple l (succ i) s

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

let vnewname v avoid = new_var v avoid 1

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
