(*
 *
 *)

module type TermAddrSig =
sig
   type term
   type address

   (* Subterm addressing *)
   exception IncorrectAddress of address * term
   exception BadAddressPrefix of address * address

   (*
    * Constructors.
    *)
   val string_of_address : address -> string
   val make_address : int list -> address
   val make_seq_address : int -> address
   val nth_cdr_addr : int -> address

   (*
    * Addressed operations.
    *)
   val term_subterm :  term -> address -> term
   val replace_subterm : term -> address -> term -> term
   val apply_fun_at_addr : (term -> term) -> address -> (term -> term)
   val remove_addr_prefix : address -> address -> address
   val subterm_arities : term -> int list
end
   
(*
 * $Log$
 * Revision 1.1  1998/05/28 15:01:42  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:18  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
