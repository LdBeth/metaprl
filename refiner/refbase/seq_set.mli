open Lm_linear_set_sig

type 'a linear_set

module Make (Type : TypeSig) : LinearSetSig with type elt = Type.t and type t = Type.t linear_set
