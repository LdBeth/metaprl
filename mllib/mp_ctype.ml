(*
 * Some character case operations.
 *)

open Printf
open Mp_debug

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Ctype%t"

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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
