(*
 * Display all the elements in a particular theory.
 *)

include Base_theory

include Package_info

type t1 = { l1 : int; l2 : float }

type t2 =
   Hello of string * string
 | World of int

let f x = x

let g x y = x

let h { l1 = x; l2 = f } = x, f

let m = function
   Some x ->
      x
 | None ->
      0

let k x =
   match x with
      Some x ->
         x
    | None ->
         0

let z f x =
   try f x with
      Failure x ->
         0

(*
 * $Log$
 * Revision 1.4  1998/05/04 13:01:01  jyh
 * Ocaml display without let rec.
 *
 * Revision 1.4  1998/04/29 20:53:09  jyh
 * Initial working display forms.
 *
 * Revision 1.3  1998/04/24 02:41:24  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/04/23 20:03:37  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.1  1998/04/17 20:48:11  jyh
 * Updating refiner for extraction.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
