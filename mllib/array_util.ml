(*
 * Operations on arrays.
 *)

open Printf
open Debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Array_util%t" eflush

(*
 * Membership in an array.
 *)
let mem i v =
   let l = Array.length v in
   let rec aux j =
      j < l & ( i = v.(j) or
                aux (j + 1) )
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
      j < l & ( f v.(j) or
                aux (j + 1) )
   in
      aux 0

let find_index f v =
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
 * Revision 1.5  1998/06/14 01:31:47  nogin
 * Fixed a typo
 *
 * Revision 1.4  1998/06/14 01:29:37  nogin
 * Make it faster
 *
 * Revision 1.3  1998/04/24 19:38:45  jyh
 * Updated debugging.
 *
 * Revision 1.2  1998/04/21 19:53:50  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.1  1997/08/06 16:17:48  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
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
