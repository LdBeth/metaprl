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
         String_util.sub "Filename_util.tail" s index (String.length s - index)
   with
      Not_found ->
         s

let head s =
   try
      let index = String_util.rindex_set s separators in
         String_util.sub "Filename_util.head" s 0 index
   with
      Not_found ->
         s

let root s =
   try
      let index = String.rindex s '.' in
         String_util.sub "Failename_util.root" s 0 index
   with
      Not_found ->
         s

let suffix s =
   try
      let index = String.rindex s '.' in
         String_util.sub "Filename_util.suffix"  s index (String.length s - index)
   with
      Not_found ->
         ""

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
