(*
 * Display all the elements in a particular theory.
 *)

include Itt_theory

declare guard{'a}

primrw fold : 'a <--> guard{'a}

primrw test : apply{.Itt_rfun!lambda{x. 'x +@ 'x}; 1} <--> 2

(*
 * $Log$
 * Revision 1.6  1998/06/01 13:52:36  jyh
 * Proving twice one is two.
 *
 * Revision 1.5  1998/05/04 23:46:08  jyh
 * Most display forms now work.
 *
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
