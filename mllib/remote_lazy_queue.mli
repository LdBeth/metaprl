(*
 * Convert a queue into a lazy queue.
 *)

open Remote_queue_sig
open Remote_lazy_queue_sig

module Make (Queue : RemoteQueueSig) : RemoteLazyQueueSig

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
