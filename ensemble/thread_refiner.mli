(*
 * The thread refiner implementation.
 *)

open Thread_refiner_sig

module MakeThreadRefiner (Arg : ThreadRefinerArgSig) : ThreadRefinerSig
with type extract = Arg.extract

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
