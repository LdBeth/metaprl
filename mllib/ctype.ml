(*
 * Some character case operations.
 *)

(* Character lists *)
let lowerchars = "abcdefghijklmnopqrstuvwxyz"
let upperchars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(*
 * Map a character to lowercase.
 *)
let tolower c =
   match c with
      'A'..'Z' -> lowerchars.[String_util.strchr upperchars c]
    | c -> c

let toupper c =
   match c with
      'a'..'z' -> upperchars.[String_util.strchr lowerchars c]
    | c -> c

(*
 * Testing.
 *)
let is_upperchar = function
   'A'..'Z' -> true
 | _ -> false

let is_lowerchar = function
   'a'..'z' -> true
 | _ -> false

let is_digit = function
   '0'..'9' -> true
 | _ -> false

(*
 * String operations.
 *)
let is_capitalized s =
   if String.length s > 0 then
      is_upperchar s.[0]
   else
      raise (Invalid_argument "is_capitalized")

let is_uppercase s =
   String_util.for_all is_upperchar s

let is_lowercase s =
   String_util.for_all is_lowerchar s

(*
 * $Log$
 * Revision 1.1  1997/08/06 16:17:51  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:14  jyh
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
