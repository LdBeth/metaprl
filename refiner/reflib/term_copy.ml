(*
 * This function copies a term, producing as much
 * aliasing as possible.  It is parameterized by the
 * term type, so it can be used for conversion between
 * term types.
 *)

open Debug
open Printf

open Opname

open Term_sig
open Term_base_sig
open Memo

let debug_memo =
   create_debug (**)
      { debug_name = "memo";
        debug_description = "Display memo operations";
        debug_value = false
      }

module MakeTermCopy (**)
   (FromType : TermSig)
   (FromTerm : TermBaseSig
    with type level_exp_var = FromType.level_exp_var
    with type level_exp = FromType.level_exp
    with type param = FromType.param
    with type operator = FromType.operator
    with type term = FromType.term
    with type bound_term = FromType.bound_term

    with type level_exp_var' = FromType.level_exp_var'
    with type level_exp' = FromType.level_exp'
    with type object_id = FromType.object_id
    with type param' = FromType.param'
    with type operator' = FromType.operator'
    with type term' = FromType.term'
    with type bound_term' = FromType.bound_term')
   (ToType : TermSig)
   (ToTerm : TermBaseSig
    with type level_exp_var = ToType.level_exp_var
    with type level_exp = ToType.level_exp
    with type param = ToType.param
    with type operator = ToType.operator
    with type term = ToType.term
    with type bound_term = ToType.bound_term

    with type level_exp_var' = ToType.level_exp_var'
    with type level_exp' = ToType.level_exp'
    with type object_id = ToType.object_id
    with type param' = ToType.param'
    with type operator' = ToType.operator'
    with type term' = ToType.term'
    with type bound_term' = ToType.bound_term')
=
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * This memo table is used for copying.
    *
    * The copy parts are used to compute new values for the term.
    * The remember parts are used to remember values that we computed,
    * so that if there is already sharing in the term, we will recover it.
    *)
   type t =
      { copy_level_var : (t,
                          FromType.level_exp_var,
                          ToType.level_exp_var',
                          ToType.level_exp_var) Memo.t;
        copy_level     : (t,
                          FromType.level_exp,
                          ToType.level_exp',
                          ToType.level_exp) Memo.t;
        copy_param     : (t,
                          FromType.param,
                          ToType.param',
                          ToType.param) Memo.t;
        copy_operator  : (t,
                          FromType.operator,
                          ToType.operator',
                          ToType.operator) Memo.t;
        copy_term      : (t,
                          FromType.term,
                          ToType.term',
                          ToType.term) Memo.t;
        copy_bterm     : (t,
                          FromType.bound_term,
                          ToType.bound_term',
                          ToType.bound_term) Memo.t
      }

   (************************************************************************
    * IMPLEMENTATIOB                                                       *
    ************************************************************************)

   (*
    * Compare that the elements on the lists are equal.
    *)
   let rec list_mem_eq l1 l2 =
      match l1, l2 with
         h1::t1, h2::t2 ->
            h1 == h2 & list_mem_eq t1 t2
       | [], [] ->
            true
       | _ ->
            false

   (*
    * Comparison functions.
    *)
   let compare_level_var { ToType.le_var = v1; ToType.le_offset = offset1 }
       { ToType.le_var = v2; ToType.le_offset = offset2 } =
      v1 == v2 & offset1 == offset2

   let compare_level { ToType.le_const = const1; ToType.le_vars = vars1 }
       { ToType.le_const = const2; ToType.le_vars = vars2 } =
      const1 = const2 & list_mem_eq vars1 vars2

   let compare_param param1 param2 =
      match param1, param2 with
         ToType.Number n1, ToType.Number n2 ->
            Num.eq_num n1 n2
       | ToType.String s1, ToType.String s2 ->
            s1 = s2
       | ToType.Token s1, ToType.Token s2 ->
            s1 = s2
       | ToType.Level l1, ToType.Level l2 ->
            l1 == l2
       | ToType.Var v1, ToType.Var v2 ->
            v1 = v2
       | ToType.MNumber s1, ToType.MNumber s2 ->
            s1 = s2
       | ToType.MString s1, ToType.MString s2 ->
            s1 = s2
       | ToType.MToken s1, ToType.MToken s2 ->
            s1 = s2
       | ToType.MLevel s1, ToType.MLevel s2 ->
            s1 = s2
       | ToType.MVar s1, ToType.MVar s2 ->
            s1 = s2
       | ToType.ObId oid1, ToType.ObId oid2 ->
            list_mem_eq oid1 oid2
       | ToType.ParamList params1, ToType.ParamList params2 ->
            list_mem_eq params1 params2
       | ToType.MSum (p11, p12), ToType.MSum (p21, p22) ->
            p11 == p12 & p21 == p22
       | ToType.MDiff (p11, p12), ToType.MSum (p21, p22) ->
            p11 == p12 & p21 == p22
       | ToType.MProduct (p11, p12), ToType.MSum (p21, p22) ->
            p11 == p12 & p21 == p22
       | ToType.MQuotient (p11, p12), ToType.MSum (p21, p22) ->
            p11 == p12 & p21 == p22
       | ToType.MRem (p11, p12), ToType.MSum (p21, p22) ->
            p11 == p12 & p21 == p22
       | ToType.MLessThan (p11, p12), ToType.MSum (p21, p22) ->
            p11 == p12 & p21 == p22
       | ToType.MEqual (p11, p12), ToType.MSum (p21, p22) ->
            p11 == p12 & p21 == p22
       | ToType.MNotEqual (p11, p12), ToType.MSum (p21, p22) ->
            p11 == p12 & p21 == p22
       | _ ->
            false

   let compare_operator { ToType.op_name = opname1; ToType.op_params = params1 }
       { ToType.op_name = opname2; ToType.op_params = params2 } =
      opname1 == opname2 & list_mem_eq params1 params2

   let compare_term { ToType.term_op = op1; ToType.term_terms = bterms1 }
       { ToType.term_op = op2; ToType.term_terms = bterms2 } =
      op1 == op2 & list_mem_eq bterms1 bterms2

   let compare_bterm { ToType.bvars = bvars1; ToType.bterm = bterm1 }
       { ToType.bvars = bvars2; ToType.bterm = bterm2 } =
      bvars1 = bvars2 & bterm1 == bterm2

   (*
    * Copy functions.
    *)
   let make_term info t =
      let { FromType.term_op = op; FromType.term_terms = bterms } = FromTerm.dest_term t in
         { ToType.term_op = Memo.apply info.copy_operator info op;
           ToType.term_terms = List.map (Memo.apply info.copy_bterm info) bterms
         }

   let make_bterm info bterm =
      let { FromType.bvars = bvars; FromType.bterm = bterm } = FromTerm.dest_bterm bterm in
         { ToType.bvars = bvars;
           ToType.bterm = Memo.apply info.copy_term info bterm
         }

   let make_operator info op =
      let { FromType.op_name = opname; FromType.op_params = params } = FromTerm.dest_op op in
         { ToType.op_name = normalize_opname opname;
           ToType.op_params = List.map (Memo.apply info.copy_param info) params
         }

   let make_param info param =
      match FromTerm.dest_param param with
         FromType.Number n1 ->
            ToType.Number n1
       | FromType.String s1 ->
            ToType.String s1
       | FromType.Token s1 ->
            ToType.Token s1
       | FromType.Level l1 ->
            ToType.Level (Memo.apply info.copy_level info l1)
       | FromType.Var v1 ->
            ToType.Var v1
       | FromType.MNumber s1 ->
            ToType.MNumber s1
       | FromType.MString s1 ->
            ToType.MString s1
       | FromType.MToken s1 ->
            ToType.MToken s1
       | FromType.MLevel s1 ->
            ToType.MLevel s1
       | FromType.MVar s1 ->
            ToType.MVar s1
       | FromType.ObId oid1 ->
            ToType.ObId (List.map (Memo.apply info.copy_param info) oid1)
       | FromType.ParamList p1 ->
            ToType.ParamList (List.map (Memo.apply info.copy_param info) p1)
       | FromType.MSum (p11, p21) ->
            ToType.MSum (Memo.apply info.copy_param info p11,
                         Memo.apply info.copy_param info p21)
       | FromType.MDiff (p11, p21) ->
            ToType.MDiff (Memo.apply info.copy_param info p11,
                          Memo.apply info.copy_param info p21)
       | FromType.MProduct (p11, p21) ->
            ToType.MProduct (Memo.apply info.copy_param info p11,
                             Memo.apply info.copy_param info p21)
       | FromType.MQuotient (p11, p21) ->
            ToType.MQuotient (Memo.apply info.copy_param info p11,
                              Memo.apply info.copy_param info p21)
       | FromType.MRem (p11, p21) ->
            ToType.MRem (Memo.apply info.copy_param info p11,
                         Memo.apply info.copy_param info p21)
       | FromType.MLessThan (p11, p21) ->
            ToType.MLessThan (Memo.apply info.copy_param info p11,
                              Memo.apply info.copy_param info p21)
       | FromType.MEqual (p11, p21) ->
            ToType.MEqual (Memo.apply info.copy_param info p11,
                           Memo.apply info.copy_param info p21)
       | FromType.MNotEqual (p11, p21) ->
            ToType.MNotEqual (Memo.apply info.copy_param info p11,
                              Memo.apply info.copy_param info p21)

   let make_level info level =
      let { FromType.le_const = c; FromType.le_vars = vars } = FromTerm.dest_level level in
         { ToType.le_const = c;
           ToType.le_vars = List.map (Memo.apply info.copy_level_var info) vars
         }

   and make_level_var info lvar =
      let { FromType.le_var = var; FromType.le_offset = offset } = FromTerm.dest_level_var lvar in
         { ToType.le_var = var;
           ToType.le_offset = offset
         }

   (*
    * Create the memo tables.
    *)
   let create () =
      { copy_level_var  = Memo.create (**)
           make_level_var
           (fun _ t -> ToTerm.make_level_var t)
           compare_level_var;
        copy_level     = Memo.create (**)
           make_level
           (fun _ t -> ToTerm.make_level t)
           compare_level;
        copy_param     = Memo.create (**)
           make_param
           (fun _ t -> ToTerm.make_param t)
           compare_param;
        copy_operator  = Memo.create (**)
           make_operator
           (fun _ t -> ToTerm.make_op t)
           compare_operator;
        copy_term      = Memo.create (**)
           make_term
           (fun _ t -> ToTerm.make_term t)
           compare_term;
        copy_bterm     = Memo.create (**)
           make_bterm
           (fun _ t -> ToTerm.make_bterm t)
           compare_bterm
      }

   (*
    * Basic term application.
    *)
   let copy_term info t =
      Memo.apply info.copy_term info t

   (*
    * Meta terms.
    * We don't share at the meta-term level.
    *)
   let rec copy_meta_term info = function
      FromType.MetaTheorem t ->
         ToType.MetaTheorem (Memo.apply info.copy_term info t)
    | FromType.MetaImplies (t1, t2) ->
         ToType.MetaImplies (copy_meta_term info t1,
                             copy_meta_term info t2)
    | FromType.MetaFunction (t1, mt1, mt2) ->
         ToType.MetaFunction (Memo.apply info.copy_term info t1,
                              copy_meta_term info mt1,
                              copy_meta_term info mt2)
    | FromType.MetaIff (mt1, mt2) ->
         ToType.MetaIff (copy_meta_term info mt1,
                         copy_meta_term info mt2)


   (*
    * Single-use versions.
    *)
   let copy_term_single t =
      copy_term (create ()) t

   let copy_meta_term_single t =
      copy_meta_term (create ()) t
end

(*
 * Common cases.
 *)
module NormalizeTerm =
   MakeTermCopy (**)
      (Refiner_std.Refiner.TermType)
      (Refiner_std.Refiner.Term)
      (Refiner.Refiner.TermType)
      (Refiner.Refiner.Term)

module DenormalizeTerm =
   MakeTermCopy (**)
      (Refiner.Refiner.TermType)
      (Refiner.Refiner.Term)
      (Refiner_std.Refiner.TermType)
      (Refiner_std.Refiner.Term)

type normalize = NormalizeTerm.t
type denormalize = DenormalizeTerm.t

let create_norm = NormalizeTerm.create
let create_denorm = DenormalizeTerm.create

let normalize_term = NormalizeTerm.copy_term
let normalize_meta_term = NormalizeTerm.copy_meta_term
let denormalize_term = DenormalizeTerm.copy_term
let denormalize_meta_term = DenormalizeTerm.copy_meta_term

let normalize_term_single = NormalizeTerm.copy_term_single
let normalize_meta_term_single = NormalizeTerm.copy_meta_term_single
let denormalize_term_single = DenormalizeTerm.copy_term_single
let denormalize_meta_term_single = DenormalizeTerm.copy_meta_term_single

(*
let normalize_term info t =
   if !debug_memo then
      eprintf "Normalizing:%t" eflush;
   let t = NormalizeTerm.copy_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_meta_term info t =
   if !debug_memo then
      eprintf "Normalizing:%t" eflush;
   let t = NormalizeTerm.copy_meta_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_term info t =
   if !debug_memo then
      begin
         eprintf "Denormalizing: ";
         flush stderr;
         Simple_print.prerr_simple_term t;
         eflush stderr
      end;
   let t = DenormalizeTerm.copy_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_meta_term info t =
   if !debug_memo then
      eprintf "Deormalizing:%t" eflush;
   let t = DenormalizeTerm.copy_meta_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_term_single t =
   if !debug_memo then
      eprintf "Normalizing Single:%t" eflush;
   let t = NormalizeTerm.copy_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_meta_term_single t =
   if !debug_memo then
      eprintf "Normalizing Single:%t" eflush;
   let t = NormalizeTerm.copy_meta_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_term_single t =
   if !debug_memo then
      begin
         eprintf "Denormalizing: Single:";
         flush stderr;
         Simple_print.prerr_simple_term t;
         eflush stderr
      end;
   let t = DenormalizeTerm.copy_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_meta_term_single t =
   if !debug_memo then
      eprintf "Deormalizing: Single%t" eflush;
   let t = DenormalizeTerm.copy_meta_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t
*)

(*
 * $Log$
 * Revision 1.2  1998/07/03 22:05:46  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.1  1998/07/02 22:24:58  jyh
 * Created term_copy module to copy and normalize terms.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
