(*
 * Operations on strings.
 *)

open Printf
open Debug
(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading String_util%t" eflush

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
      (i < len) & (c = s.[i] or loop (i + 1))
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

let split_set c s =
   let len = String.length s in
   let rec loop i j =
      if j = len then
         if i = 0 then
            [s]
         else if i = j then
            []
         else
            [String.sub s i (j - i)]
      else if mem s.[i] c then
         (String.sub s i (j - i)) :: (loop (j + 1) (j + 1))
      else
         loop i (j + 1)
   in
      loop 0 0

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
 * $Log$
 * Revision 1.6  1998/06/14 22:58:52  nogin
 * Make it faster
 *
 * Revision 1.5  1998/06/01 13:54:42  jyh
 * Proving twice one is two.
 *
 * Revision 1.4  1998/04/28 18:30:32  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/04/24 19:39:01  jyh
 * Updated debugging.
 *
 * Revision 1.2  1998/02/23 14:46:38  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1997/08/06 16:18:02  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:40  jyh
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
