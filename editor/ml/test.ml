(*
 * Test the display mechanism.
 *)

include Summary

open Printf

declare hello{x. 'b['x]}
declare app{'f; 'a}

(* primrw beta : app{hello{x. 'b['x]}; 'a} <--> 'b['a] *)

(*
 * $Log$
 * Revision 1.3  1998/05/01 14:59:14  jyh
 * Updating display forms.
 *
 * Revision 1.2  1998/04/30 14:20:09  jyh
 * Updating term_table.
 *
 * Revision 1.1  1998/04/29 20:53:12  jyh
 * Initial working display forms.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
