(*
 * Addressed operations on terms.
 *)

module type TermAddrSig =
sig
   type term
   type address

   (*
    * Constructors.
    *)
   val make_address : int list -> address
   val compose_address : address -> address -> address

   (*
    * These constructors are specifically for sequents.
    *   nth_hd_address n: address of the nth clause
    *   nth_tl_address n: address of all clauses n and larger
    *)
   val nth_hd_address : int -> address
   val nth_tl_address : int -> address

   (*
    * Destructors.
    *)
   val string_of_address : address -> string

   (*
    * Addressed operations.
    *)
   val term_subterm :  term -> address -> term
   val replace_subterm : term -> address -> term -> term
   val replace_bound_subterm : term -> address -> string list list -> (string list list -> term) -> term
   val apply_fun_at_addr : (term -> term) -> address -> term -> term
   val apply_fun_arg_at_addr : (term -> term * 'a) -> address -> term -> term * 'a
   val apply_var_fun_at_addr : (string list list -> term -> term) -> address -> string list list -> term -> term
   val apply_var_fun_arg_at_addr : (string list list -> term -> term * 'a) -> address -> string list list -> term -> term * 'a

   (*
    * higherC low-level implementation
    *)
   val apply_fun_higher : (term -> term * 'a) -> term -> term * 'a list
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
