(*
 * Operations on arrays.
 *)

(*
 * Membership in an array.
 *)
let mem i v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if i = v.(j) then
            true
         else
            aux (j + 1)
      else
         false
   in
      aux 0

(*
 * Membership in an array.
 *)
let index i v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if i = v.(j) then
            j
         else
            aux (j + 1)
      else
         raise Not_found
   in
      aux 0

(*
 * Membership in an array.
 *)
let exists f v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if f v.(j) then
            true
         else
            aux (j + 1)
      else
         false
   in
      aux 0

let find_index v f =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if f v.(j) then
            j
         else
            aux (j + 1)
      else
         raise Not_found
   in
      aux 0

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:13  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
