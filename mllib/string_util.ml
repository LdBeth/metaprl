(*
 * Operations on strings.
 *)

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
      if i = len then
         true
      else if f s.[i] then
         check (i + 1)
      else
         false
   in
      check 0

(*
 * Split a string at a particular char.
 *)
let split c s =
   let len = String.length s in
   let rec loop i j =
      if j = len then
         if i = 0 then
            [s]
         else if i = j then
            []
         else
            [String.sub s i (j - i)]
      else if s.[j] = c then
         (String.sub s i (j - i)) :: (loop (j + 1) (j + 1))
      else
         loop i (j + 1)
   in
      loop 0 0

(*
 * Concatenate strings.
 *)
let concat s l =
   let rec cat = function
      [h] -> h
    | h::t ->
         h ^ s ^ (cat t)
    | [] ->
         ""
   in
      cat l

(*
 * $Log$
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
