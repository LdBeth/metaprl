(*
 * Extend the current grammar with the filter extensions.
 *)

module type UnitSig =
sig
end

module MakeFilterGrammar (Unit : UnitSig) : UnitSig

(*
 * $Log$
 * Revision 1.1  1998/06/12 13:46:29  jyh
 * D tactic works, added itt_bool.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
