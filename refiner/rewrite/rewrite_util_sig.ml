(*
 * Utilities for the rewriter.
 *)

module type RewriteUtilSig =
sig
   type term
   type rstack

   (*
    * Precomputed exceptions.
    *)
   val redex_params_iter_exn : exn

   (*
    * List operations that throw refiner errors.
    *)
   val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
   val rev_iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

   (*
    * Membership in the stack.
    *)
   val rstack_mem : string -> rstack list -> bool
   val rstack_so_mem : string -> rstack list -> bool
   val rstack_fo_mem : string -> rstack list -> bool
   val rstack_p_mem : string -> rstack list -> bool
   val rstack_c_mem : string -> rstack list -> bool

   val array_rstack_mem : string -> rstack array -> bool
   val array_rstack_so_mem : string -> rstack array -> bool
   val array_rstack_fo_mem : string -> rstack array -> bool
   val array_rstack_p_mem : string -> rstack array -> bool
   val array_rstack_c_mem : string -> rstack array -> bool

   (*
    * Location in the stack.
    *)
   val rstack_index : string -> rstack list -> int
   val rstack_so_index : string -> rstack list -> int
   val rstack_fo_index : string -> rstack list -> int
   val rstack_p_index : string -> rstack list -> int
   val rstack_c_index : string -> rstack list -> int

   val array_rstack_index : string -> rstack array -> int
   val array_rstack_so_index : string -> rstack array -> int
   val array_rstack_fo_index : string -> rstack array -> int
   val array_rstack_p_index : string -> rstack array -> int
   val array_rstack_c_index : string -> rstack array -> int

   (*
    * Consistency in the stack.
    *)
   val rstack_check_arity : string -> int -> rstack list -> unit

   (*
    * Stack operations.
    *)
   val rstack_upgrade : string -> rstack list -> rstack list

   (*
    * Assoc.
    *)
   val var_index : (string * int) list -> term -> int
   val svar_index : (string * int) list -> string -> int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
