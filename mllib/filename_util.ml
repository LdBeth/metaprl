(*
 * Utilities on filenames.
 *)

open Printf
open Debug
(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filename_util%t" eflush

(*
 * Pathname separator chars.
 *)
let separators = "/\\"

(*
 * Split a pathname.
 *)
let split = String_util.split_set separators

(*
 * Get the Last part of the filename.
 *)
let tail s =
   try
      let index = String_util.rindex_set s separators in
         String.sub s index (String.length s - index)
   with
      Not_found ->
         s

let head s =
   try
      let index = String_util.rindex_set s separators in
         String.sub s 0 index
   with
      Not_found ->
         s

let root s =
   try
      let index = String.rindex s '.' in
         String.sub s 0 index
   with
      Not_found ->
         s

let suffix s =
   try
      let index = String.rindex s '.' in
         String.sub s index (String.length s - index)
   with
      Not_found ->
         ""

(*
 * $Log$
 * Revision 1.3  1998/04/24 19:38:55  jyh
 * Updated debugging.
 *
 * Revision 1.2  1998/04/15 12:40:06  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.1  1998/02/24 05:33:14  jyh
 * Added filename utilities.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
