(*
 * Print refine exceptions.
 *)

let print_exn f x =
   try f x with
      exn ->
         match exn with
            Precedence.Cycle
            

(*
 * $Log$
 * Revision 1.1  1998/04/08 14:57:33  jyh
 * ImpDag is in mllib.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
