(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
 *)
module type SplayTableSig =
sig
   type elt
   type 'a t

   val create : unit -> 'a t
   val add : 'a t -> elt -> 'a -> 'a t
   val union : 'a t -> 'a t -> 'a t
   val mem : 'a t -> elt -> bool
   val find : 'a t -> elt -> 'a
   val find_all : 'a t -> elt -> 'a list
   val remove : 'a t -> elt -> 'a t
   val iter : (elt -> 'a -> unit) -> 'a t -> unit
   val map : (elt -> 'a -> 'b) -> 'a t -> 'b t
end

(*
 * Build the table over an ordered type.
 *)
module MakeSplayTable (Ord : Set.OrderedType) : SplayTableSig

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
