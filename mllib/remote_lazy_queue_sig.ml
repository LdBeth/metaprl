(*
 * A Lazy queue is just like a Queue (as defined
 * in Remote_queue_sig), except that shared memory values
 * are computed lazily.  The shared values are communicated
 * as functions, but the return values are fully evaluated.
 *)
module type RemoteLazyQueueSig =
sig
   (*
    * These parts correspond directly to the Queue.
    *)
   type ('a, 'b, 'c) t
   type ('a, 'b) handle
   type ('a, 'b) lock
   type 'c key
   type ('a, 'b) upcall =
      UpcallCancel of ('a, 'b) lock
    | UpcallResult of ('a, 'b) handle * 'b
    | UpcallLock of ('a, 'b) lock
    | UpcallPreLock of ('a, 'b) lock
    | UpcallView

   val create : bool -> ('a, 'b, 'c) t
   val event_of_queue : ('a, 'b, 'c) t -> ('a, 'b) upcall Thread_event.event
   val add : ('a, 'b, 'c) t -> 'a -> ('a, 'b) handle
   val delete : ('a, 'b, 'c) t -> ('a, 'b) handle -> unit
   val lock : ('a, 'b, 'c) t -> unit
   val arg_of_lock : ('a, 'b) lock -> 'a
   val cancel : ('a, 'b, 'c) t -> ('a, 'b) lock -> unit
   val unlock : ('a, 'b, 'c) t -> ('a, 'b) lock -> 'b -> unit
   val args : unit -> (string * Arg.spec * string) list
   val main_loop : ('a, 'b, 'c) t -> unit

   (*
    * Install a lazy value.
    *)
   val share : ('a, 'b, 'c) t -> string -> (unit -> 'c) -> 'c key

   (*
    * Get the value associated with a key.
    *)
   val arg_of_key : ('a, 'b, 'c) t -> 'c key -> 'c
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
