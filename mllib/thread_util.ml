(*
 * Useful utilities for threads.
 *)

(*
 * Printer locking.
*)
let print_lock = Mutex.create ()

let lock_printer () =
   Mutex.lock print_lock

let unlock_printer () =
   flush stdout;
   flush stderr;
   Mutex.unlock print_lock

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
