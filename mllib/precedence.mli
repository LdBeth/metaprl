(*
 * A precedence is really just a partial order.
 *
 *)

(*
 * Actual precedence.
 *)
type precedence

(*
 * Relations.
 *)
type relation =
   NoRelation
 | LTRelation
 | EQRelation
 | GTRelation

(*
 * Smallest and largest elements.
 *)
val min_prec : precedence
val max_prec : precedence

(*
 * New precedence in the base.
 *)
val new_prec : unit -> precedence

(*
 * Install a relation.
 *)
val add_lt : precedence -> precedence -> unit
val add_eq : precedence -> precedence -> unit

(*
 * Get the relation.
 *)
val get_prec : precedence -> precedence -> relation

(*
 * $Log$
 * Revision 1.2  1998/04/08 15:08:27  jyh
 * Moved precedence to mllib.
 *
 * Revision 1.2  1998/04/08 14:57:31  jyh
 * ImpDag is in mllib.
 *
 * Revision 1.1  1997/04/28 15:51:29  jyh
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
 * Revision 1.3  1996/04/11 13:29:40  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.2  1996/04/07 18:24:49  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.1  1996/04/04 20:19:19  jyh
 * This is a functional version of the precedentor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
