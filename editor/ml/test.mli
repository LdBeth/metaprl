(*
 * Test the display mechanism.
 *)

include Itt_theory

open MLast

declare guard{'a}

rewrite fold : 'a <--> guard{'a}

(*
 * $Log$
 * Revision 1.4  1998/06/12 13:45:19  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.3  1998/06/01 13:52:37  jyh
 * Proving twice one is two.
 *
 * Revision 1.2  1998/05/04 13:01:03  jyh
 * Ocaml display without let rec.
 *
 * Revision 1.1  1998/04/29 20:53:14  jyh
 * Initial working display forms.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
