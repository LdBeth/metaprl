module type TENT =
 sig

 open Basic

 type oid
 type 'a tent

 exception TentLookupWoidNone
 exception TentLookupNone
 exception TentUndoBadSeq
 exception TentUndoMissing
 exception TentCommitBadSeq
 exception TentCommitMissing

 val new_tent	: unit -> 'a tent

 val tent_lookup 	: 'a tent -> stamp -> 'a

 (*
 (* not sure we want these in sig. *)
 val tent_contains_committed_oid_p	: tent -> stamp -> oid -> bool
 val tent_contains_oid_p		: tent -> stamp -> oid -> bool
 val tent_contains_pending_p		: tent -> stamp -> int -> bool
 val tent_contains_visible_p		: tent -> stamp -> bool
 *)

 val tent_insert 	: 'a tent -> stamp -> int -> oid -> 'a -> unit
 val tent_delete 	: 'a tent -> stamp -> int -> oid -> unit
 val tent_undo		: 'a tent -> stamp -> int -> (oid * 'a option)
 val tent_commit	: 'a tent -> stamp -> int -> unit

 val tent_collect 	: 'a tent -> stamp list -> unit

 end

module type OID_TYPE =
 sig
  type t
  val eq: t -> t -> bool
 end

module Tent : functor (Oid : OID_TYPE) -> (TENT with type oid = Oid.t)

