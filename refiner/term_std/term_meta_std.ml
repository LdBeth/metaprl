(*
 * Meta terms include implications, etc.
 *)

open Term_sig
open Term_subst_sig

module TermMeta 
   (Term : TermSig) 
   (TermSubst : TermSubstSig
    with type term = Term.term) =
struct
   open Term

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type term = Term.term

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

   (************************************************************************
    * META-TERMS                                                           *
    ************************************************************************)

   (*
    * Normalize all the inner terms.
    *)
   let rec normalize_mterm = function
      MetaTheorem t ->
         MetaTheorem (normalize_term t)
    | MetaImplies (a, b) ->
         MetaImplies (normalize_mterm a, normalize_mterm b)
    | MetaFunction (v, a, b) ->
         MetaFunction (v, normalize_mterm a, normalize_mterm b)
    | MetaIff (a, b) ->
         MetaIff (normalize_mterm a, normalize_mterm b)

   (*
    * Unzip a metaimplication into a list of terms.
    *)
   let rec unzip_mimplies = function
      MetaTheorem t ->
         [t]
    | MetaImplies (MetaTheorem a, t) ->
         a :: unzip_mimplies t
    | t -> raise (MetaTermMatch t)

   let rec zip_mimplies = function
      [h] -> MetaTheorem h
    | h::t -> MetaImplies (MetaTheorem h, zip_mimplies t)
    | [] -> raise (Invalid_argument "zip_mimplies")

   (*
    * Implication with bindings.
    *)
   let rec strip_mfunction = function
      MetaTheorem t ->
         MetaTheorem t
    | MetaImplies (a, t) ->
         MetaImplies (a, strip_mfunction t)
    | MetaFunction (v, a, t) ->
         MetaImplies (a, strip_mfunction t)
    | MetaIff (t1, t2) ->
         MetaIff (strip_mfunction t1, strip_mfunction t2)

   let unzip_mfunction t =
      let rec collect l = function
         MetaTheorem t ->
            List.rev l, t
       | MetaImplies (MetaTheorem a, t) ->
            collect ((None, a) :: l) t
       | MetaFunction (v, MetaTheorem a, t) ->
            collect ((Some v, a) :: l) t
       | t ->
            raise (MetaTermMatch t)
      in
         collect [] t

   let zip_mfunction args goal =
      let rec collect = function
         (Some v, a) :: t ->
            MetaFunction (v, MetaTheorem a, collect t)
       | (None, a) :: t ->
            MetaImplies (MetaTheorem a, collect t)
       | [] ->
            MetaTheorem goal
      in
         collect args

   (*
    * Unzip a rewrite term.
    *)
   let rec unzip_mrewrite = function
      MetaIff (MetaTheorem redex, MetaTheorem contractum) ->
         [], redex, contractum
    | MetaImplies(MetaTheorem a, t) ->
         let l, redex, contractum = unzip_mrewrite t in
            a::l, redex, contractum
    | t -> raise (MetaTermMatch t)

   (*
    * Calculate context vars.
    *)
   let rec binding_vars = function
      MetaTheorem t ->
         TermSubst.binding_vars t
    | MetaImplies (a, b) ->
         List_util.union (binding_vars a) (binding_vars b)
    | MetaFunction (v, a, b) ->
         List_util.union (binding_vars a) (binding_vars b)
    | MetaIff (a, b) ->
         List_util.union (binding_vars a) (binding_vars b)

   let rec context_vars = function
      MetaTheorem t ->
         TermSubst.context_vars t
    | MetaImplies (a, b) ->
         List_util.union (context_vars a) (context_vars b)
    | MetaFunction (v, a, b) ->
         List_util.union (context_vars a) (context_vars b)
    | MetaIff (a, b) ->
         List_util.union (context_vars a) (context_vars b)

   (*
    * Induction forms.
    *)
   let meta_for_all f =
      let rec aux = function
         MetaTheorem t -> f t
       | MetaImplies (a, b) ->
            aux a & aux b
       | MetaFunction (v, a, b) ->
            aux a & aux b
       | MetaIff (a, b) ->
            aux a & aux b
      in
         aux

   let meta_for_all2 f t1 t2 =
      let rec aux = function
         MetaTheorem a1, MetaTheorem a2 ->
            f a1 a2
       | MetaImplies (a1, b1), MetaImplies (a2, b2) ->
            aux (a1, a2) & aux (b1, b2)
       | MetaFunction (_, a1, b1), MetaFunction (_, a2, b2) ->
            aux (a1, a2) & aux (b1, b2)
       | MetaIff (a1, b1), MetaIff (a2, b2) ->
            aux (a1, a2) & aux (b1, b2)
       | _ ->
            raise (Failure "meta_for_all2")
      in
         aux (t1, t2)

   (*
    * Alpha equality.
    *)
   let meta_alpha_equal t1 t2 =
      try meta_for_all2 TermSubst.alpha_equal t1 t2 with
         Failure "meta_for_all2" ->
            false
end

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:02:32  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.2  1998/05/28 02:44:29  nogin
 * Parameterized by the Term and TermSubst modules
 *
 * Revision 1.1  1998/05/27 15:14:37  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
