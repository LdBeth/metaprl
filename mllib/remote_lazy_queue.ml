(*
 * Build a lazy queue from a queue.
 *)

open Remote_queue_sig

module Make (Queue : RemoteQueueSig) =
struct
   (*
    * Shared values are computed lazily.
    *)
   type 'c lazy_value =
      LazyFunction of (unit -> 'c)
    | LazyValue of 'c

   (*
    * Shared values.
    *)
   type 'c key = 'c lazy_value Queue.key

   (*
    * These parts correspond directly to the Queue.
    *)
   type ('a, 'b, 'c) t = ('a, 'b, 'c lazy_value) Queue.t
   type ('a, 'b) handle = ('a, 'b) Queue.handle
   type ('a, 'b) lock = ('a, 'b) Queue.lock
   type ('a, 'b) upcall = ('a, 'b) Queue.upcall =
      UpcallCancel of ('a, 'b) lock
    | UpcallResult of ('a, 'b) handle * 'b
    | UpcallLock of ('a, 'b) lock
    | UpcallPreLock of ('a, 'b) lock
    | UpcallView

   let create = Queue.create
   let event_of_queue = Queue.event_of_queue
   let add = Queue.add
   let delete = Queue.delete
   let lock = Queue.lock
   let arg_of_lock = Queue.arg_of_lock
   let cancel = Queue.cancel
   let unlock = Queue.unlock
   let args = Queue.args
   let main_loop = Queue.main_loop

   (*
    * Wrap the shared value in a ref.
    * Evaluate the function early to catch errors.
    *)
   let share queue debug f =
      let v = f () in
      let key = Queue.share queue debug (LazyFunction f) in
         Queue.share_local queue key (LazyValue v);
         key

   (*
    * Dereference and evaluate the lazy value.
    *)
   let arg_of_key queue key =
      match Queue.arg_of_key queue key with
         LazyValue v ->
            v
       | LazyFunction f ->
            let v = f () in
               Queue.share_local queue key (LazyValue v);
               v
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
