(*
 * This function copies a term, producing as much
 * aliasing as possible.  It is parameterized by the
 * term type, so it can be used for conversion between
 * term types.
 *)

open Opname

open Term_sig
open Term_base_sig
open Memo

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
    * Memo tables used during copying.
    *)
   type t =
      { copy_level_var : (ToType.level_exp_var', ToType.level_exp_var) Memo.t;
        copy_level : (ToType.level_exp', ToType.level_exp) Memo.t;
        copy_param : (ToType.param', ToType.param) Memo.t;
        copy_operator : (ToType.operator', ToType.operator) Memo.t;
        copy_term : (ToType.term', ToType.term) Memo.t;
        copy_bterm : (ToType.bound_term', ToType.bound_term) Memo.t
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
    * Create the memo tables.
    *)
   let create () =
      { copy_level_var = Memo.create ToTerm.make_level_var compare_level_var;
        copy_level = Memo.create ToTerm.make_level compare_level;
        copy_param = Memo.create ToTerm.make_param compare_param;
        copy_operator = Memo.create ToTerm.make_op compare_operator;
        copy_term = Memo.create ToTerm.make_term compare_term;
        copy_bterm = Memo.create ToTerm.make_bterm compare_bterm
      }

   (*
    * Copy functions.
    *)
   let rec copy_term_aux info t =
      let { FromType.term_op = op; FromType.term_terms = bterms } = FromTerm.dest_term t in
      let t =
         { ToType.term_op = copy_op info op;
           ToType.term_terms = List.map (copy_bterm info) bterms
         }
      in
         Memo.apply info.copy_term t

   and copy_bterm info bterm =
      let { FromType.bvars = bvars; FromType.bterm = bterm } = FromTerm.dest_bterm bterm in
      let bterm =
         { ToType.bvars = bvars;
           ToType.bterm = copy_term_aux info bterm
         }
      in
         Memo.apply info.copy_bterm bterm

   and copy_op info op =
      let { FromType.op_name = opname; FromType.op_params = params } = FromTerm.dest_op op in
      let op =
         { ToType.op_name = normalize_opname opname;
           ToType.op_params = List.map (copy_param info) params
         }
      in
         Memo.apply info.copy_operator op

   and copy_param info param =
      let param =
         match FromTerm.dest_param param with
            FromType.Number n1 ->
               ToType.Number n1
          | FromType.String s1 ->
               ToType.String s1
          | FromType.Token s1 ->
               ToType.Token s1
          | FromType.Level l1 ->
               ToType.Level (copy_level info l1)
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
               ToType.ObId (List.map (copy_param info) oid1)
          | FromType.ParamList p1 ->
               ToType.ParamList (List.map (copy_param info) p1)
          | FromType.MSum (p11, p21) ->
               ToType.MSum (copy_param info p11, copy_param info p21)
          | FromType.MDiff (p11, p21) ->
               ToType.MDiff (copy_param info p11, copy_param info p21)
          | FromType.MProduct (p11, p21) ->
               ToType.MProduct (copy_param info p11, copy_param info p21)
          | FromType.MQuotient (p11, p21) ->
               ToType.MQuotient (copy_param info p11, copy_param info p21)
          | FromType.MRem (p11, p21) ->
               ToType.MRem (copy_param info p11, copy_param info p21)
          | FromType.MLessThan (p11, p21) ->
               ToType.MLessThan (copy_param info p11, copy_param info p21)
          | FromType.MEqual (p11, p21) ->
               ToType.MEqual (copy_param info p11, copy_param info p21)
          | FromType.MNotEqual (p11, p21) ->
               ToType.MNotEqual (copy_param info p11, copy_param info p21)
      in
         Memo.apply info.copy_param param

   and copy_level info level =
      let { FromType.le_const = c; FromType.le_vars = vars } = FromTerm.dest_level level in
      let level =
         { ToType.le_const = c;
           ToType.le_vars = List.map (copy_level_var info) vars
         }
      in
         Memo.apply info.copy_level level

   and copy_level_var info lvar =
      let { FromType.le_var = var; FromType.le_offset = offset } = FromTerm.dest_level_var lvar in
      let lvar =
         { ToType.le_var = var;
           ToType.le_offset = offset
         }
      in
         Memo.apply info.copy_level_var lvar

   (*
    * Need several memo-pads for recording term copies.
    *)
   let copy_term t =
      let info = create () in
         copy_term_aux info t

   let copy_meta_term t =
      let rec copy info = function
         FromType.MetaTheorem t ->
            ToType.MetaTheorem (copy_term_aux info t)
       | FromType.MetaImplies (t1, t2) ->
            ToType.MetaImplies (copy info t1, copy info t2)
       | FromType.MetaFunction (t1, mt1, mt2) ->
           ToType.MetaFunction (copy_term_aux info t1, copy info mt1, copy info mt2)
       | FromType.MetaIff (mt1, mt2) ->
            ToType.MetaIff (copy info mt1, copy info mt2)
      in
         copy (create ()) t
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

let normalize_term = NormalizeTerm.copy_term
let normalize_meta_term = NormalizeTerm.copy_meta_term
let denormalize_term = DenormalizeTerm.copy_term
let denormalize_meta_term = DenormalizeTerm.copy_meta_term

(*
 * $Log$
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
