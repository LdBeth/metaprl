(*
 * Interface to getrusage.
 *)

type rusage =
   { ru_utime : float;
     ru_stime : float;
     ru_maxrss : int;
     ru_ixrss : int;
     ru_idrss : int;
     ru_isrss : int;
     ru_minflt : int;
     ru_majflt : int;
     ru_nswap : int;
     ru_inblock : int;
     ru_oublock : int;
     ru_msgsnd : int;
     ru_msgrcv : int;
     ru_nsignals : int;
     ru_nvcsw : int;
     ru_nivcsw : int
   }

val getrusage : unit -> rusage

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
