(*
 * Extend the current grammar with the filter extensions.
 *)

module type UnitSig =
sig
end

module MakeFilterGrammar (Unit : UnitSig) : UnitSig

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
