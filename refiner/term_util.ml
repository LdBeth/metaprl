(*
 * Utilities on terms.
 *
 * $Log$
 * Revision 1.4  1998/04/09 15:26:47  jyh
 * Added strip_mfunction.
 *
 * Revision 1.3  1998/02/21 20:58:25  jyh
 * Two phase parse/extract.
 *
 * Revision 1.2  1997/08/06 16:18:16  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:49  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.4  1996/09/02 19:43:30  jyh
 * Semi working package management.
 *
 * Revision 1.3  1996/04/07 18:24:58  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.2  1996/03/25 20:51:06  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.1  1996/03/11 18:34:47  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 *)

open Printf

open Debug
open Term
open Opname

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

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
   MetaTheorem t -> Term.binding_vars t
 | MetaImplies (a, b) ->
      List_util.union (binding_vars a) (binding_vars b)
 | MetaFunction (v, a, b) ->
      List_util.union (binding_vars a) (binding_vars b)
 | MetaIff (a, b) ->
      List_util.union (binding_vars a) (binding_vars b)

let rec context_vars = function
   MetaTheorem t -> Term.context_vars t
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
         raise (Invalid_argument "meta_for_all2")
   in
      aux (t1, t2)

(*
 * Alpha equality.
 *)
let meta_alpha_equal = meta_for_all2 alpha_equal

(************************************************************************
 * UTILITIES                                                            *
 ************************************************************************)

(*
 * Generalization computation.
 * See if the first term generalizes the second, and
 * compute the alpha conversion.  If the generalization
 * is _not_ true, then raise the exception:
 * Invalid_argument "generalization".
 *
 * The generalization is with respect to matching:
 *    1. A variable matches anything
 *    2. A second order variable matches
 *       anything bound according to the subterms
 *    3. A meta-parameter matches a parameter of
 *       the same parameter type.
 *)
let generalization =
   let rec generalizes_term vars t1 t2 =
      if is_so_var_term t1 then
         vars
      else if is_context_term t1 then
         if is_context_term t2 then
            let _, t1', _ = dest_context t1 in
            let _, t2', _ = dest_context t2 in
               generalizes_term vars t1' t2'
         else
            raise (Invalid_argument "generalization")
      else if is_so_var_term t2 or is_context_term t2 then
         raise (Invalid_argument "generalization")
      else
         (* Regular terms *)
         let { term_op = op1; term_terms = bterms1 } = dest_term t1 in
         let { term_op = op2; term_terms = bterms2 } = dest_term t2 in
         let { op_name = name1; op_params = params1 } = dest_op op1 in
         let { op_name = name2; op_params = params2 } = dest_op op2 in
            if name1 = name2 then
               try
                  List.iter2 generalizes_param params1 params2;
                  List.fold_left2 generalizes_bterm vars bterms1 bterms2
               with
                  Invalid_argument _ ->
                     raise (Invalid_argument "generalization")
            else
               raise (Invalid_argument "generalization")

   and generalizes_param param1 param2 =
      if param1 <> param2 then
         raise (Invalid_argument "generalization")

   and generalizes_bterm vars bterm1 bterm2 =
      (* Keep track of binding vars *)
      let { bvars = vars1; bterm = term1 } = dest_bterm bterm1 in
      let { bvars = vars2; bterm = term2 } = dest_bterm bterm2 in
      let aux vars v1 v2 =
         try
            if v2 = List.assoc v1 vars then
               vars
            else
               raise (Invalid_argument "generalization")
         with _ ->
               (v1, v2)::vars
      in
      let vars' = List.fold_left2 aux vars vars1 vars2 in
         generalizes_term vars' term1 term2
   in
      (* Start it *)
      generalizes_term

let generalizes t1 t2 =
   try generalization [] t1 t2; true with
      Invalid_argument "generalization" -> false

(*
 * Build a redex.
 *)
let construct_redex vars params terms =
   let t = mk_xlist_term (params @ terms) in
   let l = Array.length vars in
   let rec aux i =
      if i < l then
         mk_xlambda_term vars.(i) (aux (i + 1))
      else
         t
   in
      aux 0

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
