(*
 * Combine two sets to check that they have the same
 * behavior.
 *)
module Make
   (Set1: Nl_set.S with type elt = string)
   (Set2: Nl_set.S with type elt = string)
      : Nl_set.S with type elt = string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
