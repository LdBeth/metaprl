(*
 * Utilities on filenames.
 *)

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

let root s =
   try
      let index = String_util.rindex_set s separators in
         String.sub s 0 index
   with
      Not_found ->
         s

(*
 * $Log$
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
