(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
 *)
module type SplayTableSig =
sig
   type elt
   type set
   type t
   type data

   val create : set -> t
   val add : t -> elt -> data -> t
   val union : t -> t -> t
   val mem : t -> elt -> bool
   val find : t -> elt -> data
   val find_all : t -> elt -> data list
   val remove : t -> elt -> t
   val iter : (elt -> data -> unit) -> t -> unit
   val map : (elt -> data -> data) -> t -> t
end

(*
 * Ordering module takes a comparison set.
 *)
module type TableBaseSig =
sig
   type elt
   type set
   type data

   val union : set -> set -> set
   val compare : set -> elt -> elt -> int
   val append : data list -> data list -> data list
end

(*
 * Build the table over an ordered type.
 *)
module MakeSplayTable (Base : TableBaseSig)
: SplayTableSig
  with type elt = Base.elt
  with type set = Base.set
  with type data = Base.data

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
