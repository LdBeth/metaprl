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
 * Iterate over two arrays.
 *)
let iter2 f a1 a2 =
   let len = Array.length a1 in
      if Array.length a2 <> len then
         raise (Failure "Array.iter2")
      else
         let rec apply f a1 a2 i len =
            if i < len then
               begin
                  f a1.(i) a2.(i);
                  apply f a1 a2 (i + 1) len
               end
         in
            apply f a1 a2 0 len

(*
 * $Log$
 * Revision 1.6  1998/06/22 19:45:24  jyh
 * Rewriting in contexts.  This required a change in addressing,
 * and the body of the context is the _last_ subterm, not the first.
 *
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
