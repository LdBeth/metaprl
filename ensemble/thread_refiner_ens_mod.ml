(*
 * Ensemble version of the refiner.
 *)

open Thread_refiner_sig

module MakeThreadRefiner (Arg : ThreadRefinerArgSig) =
   Thread_refiner_ens.MakeThreadRefiner (Arg)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
