(*
 * Generate the mp_version.ml file.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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
open Printf
open Arg

let version = ref "<unknown-version>"
let refiner = ref "<unknown-refiner>"
let terms   = ref "<unknown-terms>"

let spec =
   ["-version", String (fun s -> version := s), "MetaPRL version";
    "-refiner", String (fun s -> refiner := s), "Refiner name";
    "-terms",   String (fun s -> terms := s), "Terms name"]

let wday_names =
   [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]

let mon_names =
   [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]

let main () =
   Arg.parse spec (fun s -> raise (Failure ("bad option: " ^ s))) "make_mp_version";
   let svnversion = 
      try
         let ch = open_in "gitversion.txt" in
         let version = input_line ch in
         let () = close_in ch in
            version
      with _ ->
         "unknown"
   in
   let now = Unix.time () in
   let { Unix.tm_year = year;
         Unix.tm_mon = mon;
         Unix.tm_mday = mday;
         Unix.tm_wday = wday;
         Unix.tm_hour = hour;
         Unix.tm_min = min;
         Unix.tm_sec = sec
       } = Unix.localtime now
   in
      printf "let version = \"MetaPRL %s  (Git rev %s):\\n\\tbuild [%s %s %d %02d:%02d:%02d %d]\\n\\ton %s\\n\\tUses %s Refiner_%s\"\n"
         !version
         svnversion
         wday_names.(wday)
         mon_names.(mon)
         mday
         hour
         min
         sec
         (year + 1900)
         (Unix.gethostname ())
         !refiner
         !terms

let _ = main ()

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
