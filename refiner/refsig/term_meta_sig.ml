(*
 * Term utilities.
 *
 *)

(************************************************************************
 * META-TERMS                                                           *
 ************************************************************************)

module type TermMetaSig =
sig
   (*
    * Have to import the type of terms.
    *)
   type term
   type meta_term

   (*
    * Some operations on meta_term.
    *)
   val binding_vars : meta_term -> string list
   val context_vars : meta_term -> string list
   val meta_alpha_equal : meta_term -> meta_term -> bool
   val unzip_mimplies : meta_term -> term list
   val zip_mimplies : term list -> meta_term
   val unzip_mfunction : meta_term -> (term option * term) list * term
   val zip_mfunction : (term option * term) list -> term -> meta_term
   val strip_mfunction : meta_term -> meta_term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
