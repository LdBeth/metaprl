(*
 * This is a null implementation of the
 * shared queue for testing a single process.
 *)

open Printf
open Nl_debug

open Thread_util

let debug_queue =
   create_debug (**)
      { debug_name = "queue";
        debug_description = "Show remote queue operations";
        debug_value = false
      }

module Queue =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * A handle is just a queue value.
    *)
   type ('a, 'b) handle = 'a

   (*
    * A lock contains the owning handle,
    *)
   type ('a, 'b) lock = 'a

   (*
    * Since this queue is only local, key sharing is trivial.
    *)
   type 'c key =
      { key_value : 'c;
        mutable key_local : 'c
      }

   (*
    * Upcalls.
    *)
   type ('a, 'b) upcall =
      UpcallCancel of ('a, 'b) lock
    | UpcallResult of ('a, 'b) handle * 'b
    | UpcallLock of ('a, 'b) lock
    | UpcallView

   (*
    * The queue contains:
    *    1. a list of unlocked entries
    *    2. a list of locked entries
    *)
   type ('a, 'b, 'c) t =
      { queue_chan : ('a, 'b) upcall Thread_event.channel;
        queue_lock : Mutex.t;
        queue_cond : Condition.t;
        mutable queue_unlocked : ('a, 'b) handle list;
        mutable queue_locked : ('a, 'b) lock list;
        mutable queue_upcalls : ('a, 'b) upcall list
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Place a message in the upcall queue.
    *)
   let send_upcall queue upcall =
      queue.queue_upcalls <- upcall :: queue.queue_upcalls;
      Condition.signal queue.queue_cond

   (*
    * Deliver all the upcall messages.
    * The queue_lock must be unlocked in this
    * function.
    *)
   let issue_upcalls queue upcalls =
      let issue upcall =
         if !debug_queue then
            begin
               lock_printer ();
               eprintf "Remote_queue_null.issue_upcall%t" eflush;
               unlock_printer ()
            end;
         Thread_event.sync 0 (Thread_event.send queue.queue_chan upcall)
      in
         List.iter issue upcalls

   (*
    * Get an event for receiving upcalls.
    *)
   let event_of_queue queue =
      Thread_event.receive queue.queue_chan

   (*
    * Submit a value to the queue.
    *)
   let add queue x =
      Mutex.lock queue.queue_lock;
      queue.queue_unlocked <- x :: queue.queue_unlocked;
      Mutex.unlock queue.queue_lock;
      x

   (*
    * Delete a value in the queue.
    * May issue an upcall.
    *)
   let delete queue lock =
      Mutex.lock queue.queue_lock;
      try
         queue.queue_unlocked <- List_util.removeq lock queue.queue_unlocked;
         Mutex.unlock queue.queue_lock;
      with
         Not_found ->
            queue.queue_locked <- List_util.removeq lock queue.queue_locked;
            send_upcall queue (UpcallCancel lock);
            Mutex.unlock queue.queue_lock

   (*
    * Lock the first unlocked value.
    * Don't worry if there are no unlocked values--
    * we'll never send an upcall.
    *)
   let lock queue =
      Mutex.lock queue.queue_lock;
      if !debug_queue then
         begin
            lock_printer ();
            eprintf "Remote_queue_null.lock: %d%t" (List.length queue.queue_unlocked) eflush;
            unlock_printer ()
         end;
      match queue.queue_unlocked with
         h :: t ->
            queue.queue_locked <- h :: queue.queue_locked;
            queue.queue_unlocked <- t;
            send_upcall queue (UpcallLock h);
            Mutex.unlock queue.queue_lock
       | [] ->
            Mutex.unlock queue.queue_lock

   let arg_of_lock lock =
      lock

   (*
    * Cancel a pending lock.
    *)
   let cancel queue lock =
      Mutex.lock queue.queue_lock;
      queue.queue_locked <- List_util.removeq lock queue.queue_locked;
      queue.queue_unlocked <- lock :: queue.queue_unlocked;
      Mutex.unlock queue.queue_lock

   (*
    * Unlock and return a value to the owner.
    *)
   let unlock queue lock x =
      Mutex.lock queue.queue_lock;
      queue.queue_locked <- List_util.removeq lock queue.queue_locked;
      send_upcall queue (UpcallResult (lock, x));
      Mutex.unlock queue.queue_lock

   (*
    * Add a key to the queue.
    *)
   let share queue _ x =
      { key_value = x;
        key_local = x
      }

   let share_local _ key x =
      key.key_local <- x

   let arg_of_key _ key =
      key.key_local

   (*
    * Main loop waits for upcall events to be triggered.
    *)
   let thread_main_loop queue =
      while true do
         Mutex.lock queue.queue_lock;
         Condition.wait queue.queue_cond queue.queue_lock;
         let upcalls = List.rev queue.queue_upcalls in
            queue.queue_upcalls <- [];
            Mutex.unlock queue.queue_lock;
            issue_upcalls queue upcalls
      done

   (*
    * Create an empty queue.
    *)
   let create upcalls =
      { queue_chan = Thread_event.new_channel ();
        queue_lock = Mutex.create ();
        queue_cond = Condition.create ();
        queue_unlocked = [];
        queue_locked = [];
        queue_upcalls = []
      }

   (*
    * Nothing needed in main loop.
    *)
   let main_loop queue =
      Thread.create thread_main_loop queue;
      ()
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
