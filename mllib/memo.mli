(*
 * Generic memoize function.
 * It looks just like a function.
 *)

type ('a, 'b) t

val create : ('a -> 'b) -> ('a -> 'a -> bool) -> ('a, 'b) t
val apply : ('a, 'b) t -> 'a -> 'b

(*
 * $Log$
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
