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

   (*
    * The terms in the meta-logical framework include
    * a meta-implication and met-iff.
    *)
   type meta_term =
      MetaTheorem of term
    | MetaImplies of meta_term * meta_term
    | MetaFunction of term * meta_term * meta_term
    | MetaIff of meta_term * meta_term

   exception MetaTermMatch of meta_term

   (*
    * Some operations on meta_term.
    *)
   val normalize_mterm : meta_term -> unit

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
 * $Log$
 * Revision 1.3  1998/06/15 21:58:04  jyh
 * Added a few new functions.
 *
 * Revision 1.2  1998/06/01 13:55:15  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:01:46  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:34  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
