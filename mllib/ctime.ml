(*
 * Time functions.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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
 * jyh@cs.caltech.edu
 *)
open Lm_debug

(*
 * Blown out representation.
 *)
type localtime =
   { time_sec : int;
     time_min : int;
     time_hour : int;
     time_mday : int;
     time_mon : int;
     time_year : int;
     time_zone : int
   }

(*
 * Convert a string to a float.
 *)
external localtime : int -> localtime = "c_localtime"
external mktime : localtime -> int = "c_mktime"

let parse_int s =
   try int_of_string s with
      Failure _ ->
         0

let parse_date_aux mday mon year hour min sec zone =
   let mday = parse_int mday in
   let mon =
      match String.lowercase mon with
         "jan" -> 0
       | "feb" -> 1
       | "mar" -> 2
       | "apr" -> 3
       | "may" -> 4
       | "jun" -> 5
       | "jul" -> 6
       | "aug" -> 7
       | "sep" -> 8
       | "oct" -> 9
       | "nov" -> 10
       | "dec" -> 11
       | _ -> 0
   in
   let year =
      let year = parse_int year in
         if year = 0 then
            2000
         else if year < 100 then
            year + 1900
         else
            year
   in
   let hour = parse_int hour in
   let min = parse_int min in
   let sec = parse_int sec in
   let zone =
      match String.lowercase zone with
         "ut" | "gmt" -> 0
       | "est" -> -5 * 60 * 60
       | "edt" -> -4 * 60 * 60
       | "cst" -> -6 * 60 * 60
       | "cdt" -> -5 * 60 * 60
       | "mst" -> -7 * 60 * 60
       | "mdt" -> -6 * 60 * 60
       | "pst" -> -8 * 60 * 60
       | "pdt" -> -7 * 60 * 60
       | _ -> 0
   in
      mktime (**)
         { time_sec = sec;
           time_min = min;
           time_hour = hour;
           time_mday = mday;
           time_mon = mon;
           time_year = year;
           time_zone = zone
         }

let parse_time s =
   match Lm_string_util.parse_args s with
      [mday; month; year; hour; minute; sec; zone] ->
         parse_date_aux mday month year hour minute sec zone
    | [_; mday; month; year; hour; minute; sec; zone] ->
         parse_date_aux mday month year hour minute sec zone
    | _ ->
         0

(*
 * Month name.
 *)
let name_of_month = function
   1 -> "January"
 | 2 -> "February"
 | 3 -> "March"
 | 4 -> "April"
 | 5 -> "May"
 | 6 -> "June"
 | 7 -> "July"
 | 8 -> "August"
 | 9 -> "September"
 | 10 -> "October"
 | 11 -> "November"
 | 12 -> "December"
 | _ -> "Unknown"

(*
 * Convert to a string.
 *)
external ctime : int -> string = "c_ctime"
external simpletime : int -> string = "c_simpletime"

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
