(************************************************************************
 * Subterm addressing                                                   *
 ************************************************************************)

#include "refine_error.h"

open Printf
open Mp_debug

open Refine_error_sig
open Term_sig
open Term_base_sig
open Term_op_sig

(*
 * Address of a subterm.
 *)
type addr =
   Path of int list
 | NthClause of int * bool
 | Compose of addr * addr

module MakeAddressType (TermType : TermSig) =
struct
   type t = addr
end

module TermAddr (**)
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term = TermType.term
    with type term' = TermType.term'
    with type bound_term = TermType.bound_term
    with type bound_term' = TermType.bound_term'
    with type operator = TermType.operator
    with type operator' = TermType.operator')
   (TermOp : TermOpSig
    with type term = TermType.term)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = addr) =
struct
   open TermType
   open Term
   open TermOp
   open RefineError

   type term = TermType.term
   type address = addr

   (*
    * Constructor.
    *)
   let make_address l =
      Path l

   let nth_hd_address i =
      NthClause (i, true)

   let nth_tl_address i =
      NthClause (i, false)

   let depth_of_address = function
      NthClause (i, _) ->
         i
    | _ ->
         ref_raise(RefineError ("depth_of_address", StringError "address is not a sequent address"))

   let compose_address path1 path2 =
      match path1 with
         Path [] ->
            path2
       | _ ->
            Compose (path1, path2)

#ifdef VERBOSE_EXN
#  define ATERM a term
#else
#  define ATERM
#endif

   (*
    * Get a subterm.
    *)
   let rec getnth ATERM terms i =
      match (terms, i) with
         (hd::_, 0) ->
            hd
       | (hd::tl, _) ->
            getnth ATERM tl (pred i)
       | ([], _) ->
            ref_raise(RefineError ("getnth", AddressError (a, term)))

   (*
    * Follow an explicit path.
    *)
   let rec term_subterm_path ATERM t = function
      [] ->
         t
    | i::tl ->
         term_subterm_path ATERM (dest_bterm (getnth ATERM (dest_term t).term_terms i)).bterm tl

   (*
    * Follow a sequent path to a clause.
    *)
   let rec term_subterm_nthpath ATERM flag t = function
      0 ->
         if flag then
            match dest_term t with
               { term_op = op; term_terms = bterm :: _ } ->
                  if Opname.eq (dest_op op).op_name context_opname then
                     t
                  else
                     (dest_bterm bterm).bterm
             | _ ->
                  ref_raise(RefineError ("term_subterm_nthpath", AddressError (a, term)))
         else
            t
    | i ->
         let { term_op = op; term_terms = bterms } = dest_term t in
            match (dest_term t).term_terms with
               [bterm] ->
                  term_subterm_nthpath ATERM flag (dest_bterm bterm).bterm (i - 1)
             | ((_ :: bterm2 :: _) as bterms) ->
                  if Opname.eq (dest_op op).op_name context_opname then
                     term_subterm_nthpath ATERM flag (dest_bterm (List_util.last bterms)).bterm (i - 1)
                  else
                     term_subterm_nthpath ATERM flag (dest_bterm bterm2).bterm (i - 1)
             | _ ->
                  ref_raise(RefineError ("term_subterm_nthpath", AddressError (a, term)))

   (*
    * Get the subterm for any type of path.
    *)
   let rec term_subterm term a =
      match a with
         Path addr ->
            term_subterm_path ATERM term addr
       | NthClause (addr, flag) ->
            term_subterm_nthpath ATERM flag term addr
       | Compose (addr1, addr2) ->
            term_subterm (term_subterm term addr1) addr2

   let term_subterm_count term a =
      subterm_count (term_subterm term a)

   (*
    * Replace a subterm at the specified address.
    * Capture is not taken into account.  This function
    * allows the replacement to compute an extra value.
    *)
#ifdef VERBOSE_EXN
#  define FAIL fail
#  define DO_FAIL fail_addr fail
#else
#  define FAIL
#  define DO_FAIL raise_generic_exn
#endif

   let fail_addr (addr, term) =
      ref_raise(RefineError ("apply_*_fun_*", AddressError (addr, term)))

#define APPLY_FUN \
   let rec path_replace_term FAIL f BVARS t = function \
      i::tl -> \
         let { term_op = op; term_terms = bterms } = dest_term t in \
         let bterms, arg = path_replace_bterm FAIL f tl i BVARS bterms in \
            mk_term op bterms, arg \
    | [] -> \
         f BVARS t \
\
   and path_replace_bterm FAIL f tl i BVARS bterms = \
      match i, bterms with \
         (0, bterm :: bterms) -> \
            let { bvars = vars; bterm = term } = dest_bterm bterm in \
            let term, arg = path_replace_term FAIL f VARS_BVARS term tl in \
               mk_bterm vars term :: bterms, arg \
       | (_, bterm :: bterms) -> \
            let bterms, arg = path_replace_bterm FAIL f tl (i - 1) BVARS bterms in \
               bterm :: bterms, arg \
       | _, [] -> \
            DO_FAIL \
\
   let rec nthpath_replace_term FAIL flag f BVARS t i = \
      if i = 0 then \
         if flag then \
            match dest_term t with \
               { term_op = op; term_terms = bterm :: bterms } -> \
                  if Opname.eq (dest_op op).op_name context_opname then \
                     f BVARS t \
                  else \
                     let { bvars = vars; bterm = term } = dest_bterm bterm in \
                     let term, arg = f VARS_BVARS term in \
                     let bterm = mk_bterm vars term in \
                        mk_term op (bterm :: bterms), arg \
             | _ -> \
                  DO_FAIL \
         else \
            f BVARS t \
      else \
         match dest_term t with \
            { term_op = op; term_terms = [bterm] } -> \
               (* Always take subterm if there is only one *) \
               let { bvars = vars; bterm = trm } = dest_bterm bterm in \
               let term, arg = nthpath_replace_term FAIL flag f VARS_BVARS trm (i - 1) in \
               let bterm = mk_bterm vars term in \
                  mk_term op [bterm], arg \
          | { term_op = op; term_terms = ((bterm1 :: bterm2 :: bterms) as bterms1) } -> \
               if Opname.eq (dest_op op).op_name context_opname then \
                  let args, bterm = List_util.split_last bterms1 in \
                  let { bvars = vars; bterm = trm } = dest_bterm bterm in \
                  let term, arg = nthpath_replace_term FAIL flag f VARS_BVARS trm (i - 1) in \
                  let bterm = mk_bterm vars term in \
                     mk_term op (args @ [bterm]), arg \
               else \
                  let { bvars = vars; bterm = trm } = dest_bterm bterm2 in \
                  let term, arg = nthpath_replace_term FAIL flag f VARS_BVARS trm (i - 1) in \
                  let bterm = mk_bterm vars term in \
                     mk_term op (bterm1 :: bterm :: bterms), arg \
          | _ -> \
               DO_FAIL

#define APPLY_FUN_AUX\
   let rec apply_fun_arg_at_addr_aux FAIL f addr BVARS term = \
      match addr with \
         Path addr -> \
            path_replace_term FAIL f BVARS term addr \
       | NthClause (addr, flag) -> \
            nthpath_replace_term FAIL flag f BVARS term addr \
       | Compose (addr1, addr2) -> \
            apply_fun_arg_at_addr_aux FAIL (**) \
               (apply_fun_arg_at_addr_aux FAIL f addr2) addr1 BVARS term

#ifdef VERBOSE_EXN
#  define APPLY_FUN_MAIN APPLY_FUN_AUX\
   let apply_fun_arg_at_addr f addr BVARS term = \
      apply_fun_arg_at_addr_aux (addr, term) f addr BVARS term
#else
#  define apply_fun_arg_at_addr_aux apply_fun_arg_at_addr
#  define APPLY_FUN_MAIN APPLY_FUN_AUX
#endif

#define APPLY_FUN_NOARG \
   let add_unit_arg f BVARS t = \
      f BVARS t, () \
\
   let apply_fun_at_addr f addr BVARS term = \
      fst (apply_fun_arg_at_addr (add_unit_arg f) addr BVARS term)

#define BVARS
#define VARS_BVARS
      APPLY_FUN
      APPLY_FUN_MAIN
      APPLY_FUN_NOARG
#undef BVARS
#undef VARS_BVARS
#define BVARS bvars
#define VARS_BVARS (vars::bvars)
#define path_replace_term path_var_replace_term
#define path_replace_bterm path_var_replace_bterm
#define nthpath_replace_term nthpath_var_replace_term
#ifdef VERBOSE_EXN
#  define apply_fun_arg_at_addr_aux apply_var_fun_at_addr_aux
#endif
#define apply_fun_arg_at_addr apply_var_fun_arg_at_addr
#define add_unit_arg add_var_unit_arg
#define apply_fun_at_addr apply_var_fun_at_addr
      APPLY_FUN
      APPLY_FUN_MAIN
      APPLY_FUN_NOARG
#undef apply_fun_arg_at_addr
#undef apply_fun_at_addr

   let replace_subterm_aux subterm term =
      subterm

   let replace_subterm term addr subterm =
      apply_fun_at_addr (replace_subterm_aux subterm) addr term

   let replace_bound_subterm_aux f bvars term =
      f bvars

   let replace_bound_subterm term addr bvars f =
      apply_var_fun_at_addr (replace_bound_subterm_aux f) addr bvars term

   (*
    * Print address as a string.
    *)
   let rec collect_string_of_path_address = function
      [] ->
         ""
    | [h] ->
         string_of_int h
    | h::t ->
         (string_of_int h) ^ "; " ^ (collect_string_of_path_address t)

   let rec collect_string_of_nthpath_address_true = function
      0 ->
         "0"
    | i ->
         "@; " ^ (collect_string_of_nthpath_address_true (i - 1))

   let rec collect_string_of_nthpath_address_false = function
      0 ->
         ""
    | 1 ->
         "@"
    | i ->
         "@; " ^ (collect_string_of_nthpath_address_false (i - 1))

   let rec collect_string_of_address = function
      Path addr ->
         collect_string_of_path_address addr
    | NthClause (addr, flag) ->
         (if flag then
             collect_string_of_nthpath_address_true
          else
             collect_string_of_nthpath_address_false) addr
    | Compose (addr1, addr2) ->
         let addr1 = collect_string_of_address addr1 in
         let addr2 = collect_string_of_address addr2 in
            if addr1 = "" then
               addr2
            else if addr2 = "" then
               addr2
            else
               addr1 ^ "; " ^ addr2

   let string_of_address addr =
      "[" ^ collect_string_of_address addr ^ "]"

   (*
    * Apply the function to the outermost terms where it does not fail.
    *)
   let rec apply_fun_higher_term f coll term =
      try let (t,arg) = f term in
             t, (arg::coll)
      with RefineError _ ->
            let dt = dest_term term in
            let (btrms, args) = apply_fun_higher_bterms f coll dt.term_terms in
               if args == coll
               then (term,coll)
               else (mk_term dt.term_op btrms, args)

   and apply_fun_higher_bterms f coll = function
      [] ->
         ([],coll)
    | (bt::btrms) as bterms ->
         let (btrms_new, args) = apply_fun_higher_bterms f coll btrms in
         let dbt = dest_bterm bt in
         let (bt_new, args2) = apply_fun_higher_term f args dbt.bterm in
            if args2 == coll then (bterms, coll)
            else
               let bt_new =
                  if args2 == args
                  then bt else
                  mk_bterm dbt.bvars bt_new
               in
               (bt_new::btrms_new, args2)

   let apply_fun_higher f term = apply_fun_higher_term f [] term

   (*
    * Apply the function at the outermost terms where it does not fail,
    * and also pass in binding variables.
    *)
   let rec apply_var_fun_higher_term f bvars coll term =
      try
         let t, arg = f bvars term in
            t, arg :: coll
      with
         RefineError _ ->
            let dt = dest_term term in
            let bterms, args = apply_var_fun_higher_bterms f bvars coll dt.term_terms in
               if args == coll then
                  term, coll
               else
                  mk_term dt.term_op bterms, args

   and apply_var_fun_higher_bterms f bvars coll = function
      [] ->
         [], coll
    | (bterm :: bterms) as bterms1 ->
         let bterms_new, args = apply_var_fun_higher_bterms f bvars coll bterms in
         let { bvars = bvars'; bterm = term } = dest_bterm bterm in
         let bterm_new, args = apply_var_fun_higher_term f (bvars' :: bvars) args term in
            if args == coll then
               bterms1, coll
            else
               (mk_bterm bvars' bterm_new) :: bterms_new, args

   let apply_var_fun_higher f bvars term =
      apply_var_fun_higher_term f bvars [] term
end
