(*
 * More functional implementation of splay sets.
 *)

module Make (Ord : Set.OrderedType)
: Nl_set.S
  with type elt = Ord.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
