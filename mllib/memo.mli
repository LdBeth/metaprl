(*
 * Generic memoize function.
 * It looks just like a function.
 *)

type ('a, 'b, 'c, 'd) t

(*
 * This is a memoizer function.
 * Types:
 *   'a: the type of arguments that are not remembered
 *   'b: the type of arguments that are remembered by pointer equality
 *   'c: arguments to be remembered, computed from 'a
 *   'd: values to be remembered
 *
 * create arguments:
 * arg0: compute the actual argument
 *   however, if we ever lookup a value that is pointer
 *   equal to 'a that we have remembered, we return the remembered
 *   value directly.
 * arg1: compute the value of the function
 *   'b is the argument, and if we have ever computed this value
 *   before according to arg2, then we return the previous value.
 * arg2: compare values in 'b
 *   this function may be somewhat finer that normal equality,
 *   but it cannot be coarser
 *
 * This creates a memoizer, which acts like a function.
 * The function can be applied using the apply function.
 *
 * The objective is that (apply (create f g eq)) looks just like
 * (fun arg x -> g arg (f arg x)), but it is more efficient because
 * it remembers the arguments x (by pointer equality), and values
 * of f arg x and g arg (f arg x) (by the eq function).
 *)
val create :
   ('a -> 'b -> 'c) ->          (* Create the actual argument *)
   ('a -> 'c -> 'd) ->          (* Compute the value of the function *)
   ('c -> 'c -> bool) ->        (* Compare arguments *)
   ('a, 'b, 'c, 'd) t
val apply : ('a, 'b, 'c, 'd) t -> 'a -> 'b -> 'd

(*
 * $Log$
 * Revision 1.2  1998/07/03 22:05:36  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.1  1998/07/02 22:24:54  jyh
 * Created term_copy module to copy and normalize terms.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
