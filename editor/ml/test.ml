(*
 * Display all the elements in a particular theory.
 *)

include Itt_theory

open Conversionals
open Itt_rfun
open Itt_bool
open Itt_int
open Itt_int_bool

declare guard{'a}
declare fact{'i}

primrw fold : 'a <--> guard{'a}

primrw reduceFact : fact{'i} <--> fix{f. lambda{i. ifthenelse{eq_int{'i; 0}; 1; .'i *@ 'f ('i -@ 1)}}} 'i

primrw test : fact{40} <--> 0 (* 815915283247897734345611269596115894272000000000 *)

dform fact_df : parens :: "prec"[prec_apply] :: fact{'i} =
   `"fact" " " slot{'i}

let redexC =
   firstC [betaReduction;
           reduceEQInt;
           reduceFact;
           boolTrue;
           boolFalse;
           ifthenelseTrue;
           ifthenelseFalse;
           reduceAdd;
           reduceSub;
           reduceMul;
           reduceDiv;
           fix]

(*
 * $Log$
 * Revision 1.10  1998/06/12 20:46:01  jyh
 * Switched to term_ds.
 *
 * Revision 1.9  1998/06/12 18:36:15  jyh
 * Working factorial proof.
 *
 * Revision 1.8  1998/06/12 13:45:18  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.7  1998/06/01 19:53:12  jyh
 * Working addition proof.  Removing polymorphism from refiner(?)
 *
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
