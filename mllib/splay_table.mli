(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
 *)
module type SplayTableSig =
sig
   type elt
   type set
   type 'a t

   val create : set -> 'a t
   val add : 'a t -> elt -> 'a -> 'a t
   val union : ('a list -> 'a list -> 'a list) -> 'a t -> 'a t -> 'a t
   val mem : 'a t -> elt -> bool
   val find : 'a t -> elt -> 'a
   val find_all : 'a t -> elt -> 'a list
   val remove : 'a t -> elt -> 'a t
   val iter : (elt -> 'a -> unit) -> 'a t -> unit
   val map : (elt -> 'a -> 'b) -> 'a t -> 'b t
end

(*
 * Ordering module takes a comparison set.
 *)
module type OrdSig =
sig
   type t
   type set

   val union : set -> set -> set
   val compare : set -> t -> t -> int
end

(*
 * Build the table over an ordered type.
 *)
module MakeSplayTable (Ord : OrdSig)
: SplayTableSig
  with type elt = Ord.t
  with type set = Ord.set

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
