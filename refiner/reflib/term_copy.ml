(*
 * This function copies a term, producing as much
 * aliasing as possible.  It is parameterized by the
 * term type, so it can be used for conversion between
 * term types.
 *)

open Nl_debug
open Printf

open Opname

open Refiner_sig
open Memo

let debug_memo =
   create_debug (**)
      { debug_name = "memo";
        debug_description = "Display memo operations";
        debug_value = false
      }

module MakeTermCopy (FromRefiner : RefinerSig) (ToRefiner : RefinerSig)
=
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type to_term = TTerm of ToRefiner.TermType.term' | TSeq of ToRefiner.TermType.esequent

   type t =
      { copy_level_var : (t,
                          FromRefiner.TermType.level_exp_var,
                          ToRefiner.TermType.level_exp_var',
                          ToRefiner.TermType.level_exp_var) Memo.t;
        copy_level     : (t,
                          FromRefiner.TermType.level_exp,
                          ToRefiner.TermType.level_exp',
                          ToRefiner.TermType.level_exp) Memo.t;
        copy_param     : (t,
                          FromRefiner.TermType.param,
                          ToRefiner.TermType.param',
                          ToRefiner.TermType.param) Memo.t;
        copy_operator  : (t,
                          FromRefiner.TermType.operator,
                          ToRefiner.TermType.operator',
                          ToRefiner.TermType.operator) Memo.t;
        copy_term      : (t,
                          FromRefiner.TermType.term,
                          to_term,
                          ToRefiner.TermType.term) Memo.t;
        copy_bterm     : (t,
                          FromRefiner.TermType.bound_term,
                          ToRefiner.TermType.bound_term',
                          ToRefiner.TermType.bound_term) Memo.t
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Compare that the elements on the lists are equal.
    *)
   let list_mem_eq = List_util.compare_eq

   (*
    * Comparison functions.
    *)
   let compare_level_var { ToRefiner.TermType.le_var = v1; ToRefiner.TermType.le_offset = offset1 }
                         { ToRefiner.TermType.le_var = v2; ToRefiner.TermType.le_offset = offset2 } =
      v1 == v2 & offset1 == offset2

   let compare_level { ToRefiner.TermType.le_const = const1; ToRefiner.TermType.le_vars = vars1 }
                     { ToRefiner.TermType.le_const = const2; ToRefiner.TermType.le_vars = vars2 } =
      const1 = const2 & list_mem_eq vars1 vars2

   let compare_param param1 param2 =
      match param1, param2 with
         ToRefiner.TermType.Number    n1,         ToRefiner.TermType.Number    n2         -> Nl_num.eq_num n1 n2
       | ToRefiner.TermType.String    s1,         ToRefiner.TermType.String    s2         -> s1 = s2
       | ToRefiner.TermType.Token     s1,         ToRefiner.TermType.Token     s2         -> s1 = s2
       | ToRefiner.TermType.Level     l1,         ToRefiner.TermType.Level     l2         -> l1 == l2
       | ToRefiner.TermType.Var       v1,         ToRefiner.TermType.Var       v2         -> v1 = v2
       | ToRefiner.TermType.MNumber   s1,         ToRefiner.TermType.MNumber   s2         -> s1 = s2
       | ToRefiner.TermType.MString   s1,         ToRefiner.TermType.MString   s2         -> s1 = s2
       | ToRefiner.TermType.MToken    s1,         ToRefiner.TermType.MToken    s2         -> s1 = s2
       | ToRefiner.TermType.MLevel    s1,         ToRefiner.TermType.MLevel    s2         -> s1 = s2
       | ToRefiner.TermType.MVar      s1,         ToRefiner.TermType.MVar      s2         -> s1 = s2
       | ToRefiner.TermType.ObId      oid1,       ToRefiner.TermType.ObId      oid2       -> list_mem_eq oid1 oid2
       | ToRefiner.TermType.ParamList params1,    ToRefiner.TermType.ParamList params2    -> list_mem_eq params1 params2
       | ToRefiner.TermType.MSum      (p11, p12), ToRefiner.TermType.MSum      (p21, p22) -> p11 == p12 & p21 == p22
       | ToRefiner.TermType.MDiff     (p11, p12), ToRefiner.TermType.MDiff     (p21, p22) -> p11 == p12 & p21 == p22
       | ToRefiner.TermType.MProduct  (p11, p12), ToRefiner.TermType.MProduct  (p21, p22) -> p11 == p12 & p21 == p22
       | ToRefiner.TermType.MQuotient (p11, p12), ToRefiner.TermType.MQuotient (p21, p22) -> p11 == p12 & p21 == p22
       | ToRefiner.TermType.MRem      (p11, p12), ToRefiner.TermType.MRem      (p21, p22) -> p11 == p12 & p21 == p22
       | ToRefiner.TermType.MLessThan (p11, p12), ToRefiner.TermType.MLessThan (p21, p22) -> p11 == p12 & p21 == p22
       | ToRefiner.TermType.MEqual    (p11, p12), ToRefiner.TermType.MEqual    (p21, p22) -> p11 == p12 & p21 == p22
       | ToRefiner.TermType.MNotEqual (p11, p12), ToRefiner.TermType.MNotEqual (p21, p22) -> p11 == p12 & p21 == p22
       | _ -> false

   let compare_operator { ToRefiner.TermType.op_name = opname1; ToRefiner.TermType.op_params = params1 }
                        { ToRefiner.TermType.op_name = opname2; ToRefiner.TermType.op_params = params2 } =
      opname1 == opname2 & list_mem_eq params1 params2

   let compare_term { ToRefiner.TermType.term_op = op1; ToRefiner.TermType.term_terms = bterms1 }
                    { ToRefiner.TermType.term_op = op2; ToRefiner.TermType.term_terms = bterms2 } =
      op1 == op2 & list_mem_eq bterms1 bterms2

   let rec compare_hyps hyp1 hyp2 i =
      (i<0) ||
      ((match (ToRefiner.Term.SeqHyp.get hyp1 i), (ToRefiner.Term.SeqHyp.get hyp2 i) with
           ToRefiner.TermType.Hypothesis (v1,t1),  ToRefiner.TermType.Hypothesis (v2,t2)   -> v1 = v2 && t1 == t2
         | ToRefiner.TermType.Context    (v1,ts1), ToRefiner.TermType.Context    (v2, ts2) -> v1 = v2 && list_mem_eq ts1 ts2
         | _ -> false) &&
       (compare_hyps hyp1 hyp2 (pred i)))

   let rec compare_goals goal1 goal2 i =
      (i<0) ||
      (((ToRefiner.Term.SeqGoal.get goal1 i) == (ToRefiner.Term.SeqGoal.get goal2 i)) &&
       (compare_goals goal1 goal2 (pred i)))

   let compare_tterm t1 t2 =
      match (t1,t2) with
         TTerm t1, TTerm t2 ->
            compare_term t1 t2
       | TSeq { ToRefiner.TermType.sequent_args = arg1; ToRefiner.TermType.sequent_hyps = hyp1; ToRefiner.TermType.sequent_goals = goal1},
         TSeq { ToRefiner.TermType.sequent_args = arg2; ToRefiner.TermType.sequent_hyps = hyp2; ToRefiner.TermType.sequent_goals = goal2} ->
            (arg1 == arg2) &&
            (ToRefiner.Term.SeqHyp.length hyp1 = ToRefiner.Term.SeqHyp.length hyp2) &&
            (compare_hyps hyp1 hyp2 (ToRefiner.Term.SeqHyp.length hyp1 - 1)) &&
            (ToRefiner.Term.SeqGoal.length goal1 = ToRefiner.Term.SeqGoal.length goal2) &&
            (compare_goals goal1 goal2 (ToRefiner.Term.SeqGoal.length goal1 - 1))
       | _ -> false

   let compare_bterm { ToRefiner.TermType.bvars = bvars1; ToRefiner.TermType.bterm = bterm1 }
       { ToRefiner.TermType.bvars = bvars2; ToRefiner.TermType.bterm = bterm2 } =
      bvars1 = bvars2 & bterm1 == bterm2

   (*
    * Copy functions.
    *)
   let make_hyp info hyps i =
      match FromRefiner.Term.SeqHyp.get hyps i with
         FromRefiner.TermType.Hypothesis (v, t) -> ToRefiner.TermType.Hypothesis (v, Memo.apply info.copy_term info t)
       | FromRefiner.TermType.Context (v, trms) -> ToRefiner.TermType.Context (v, List.map (Memo.apply info.copy_term info) trms)

   let make_goal info goals i =
      Memo.apply info.copy_term info (FromRefiner.Term.SeqGoal.get goals i)

   let make_term info t =
      if FromRefiner.TermMan.is_sequent_term t then
         let { FromRefiner.TermType.sequent_args = args;
               FromRefiner.TermType.sequent_hyps = hyps;
               FromRefiner.TermType.sequent_goals = goals } = (FromRefiner.TermMan.explode_sequent t)
         in
            TSeq
               { ToRefiner.TermType.sequent_args =
                    Memo.apply info.copy_term info args;
                 ToRefiner.TermType.sequent_hyps =
                    ToRefiner.Term.SeqHyp.init (FromRefiner.Term.SeqHyp.length hyps) (make_hyp info hyps);
                 ToRefiner.TermType.sequent_goals =
                    ToRefiner.Term.SeqGoal.init (FromRefiner.Term.SeqGoal.length goals) (make_goal info goals)
            }
      else let { FromRefiner.TermType.term_op = op; FromRefiner.TermType.term_terms = bterms } = FromRefiner.Term.dest_term t in
         TTerm
            { ToRefiner.TermType.term_op = Memo.apply info.copy_operator info op;
              ToRefiner.TermType.term_terms = List.map (Memo.apply info.copy_bterm info) bterms }

   let do_make_term _ = function
      TTerm t -> ToRefiner.Term.make_term t
    | TSeq s -> ToRefiner.TermMan.mk_sequent_term s

   let make_bterm info bterm =
      let { FromRefiner.TermType.bvars = bvars; FromRefiner.TermType.bterm = bterm } = FromRefiner.Term.dest_bterm bterm in
         { ToRefiner.TermType.bvars = bvars;
           ToRefiner.TermType.bterm = Memo.apply info.copy_term info bterm
         }

   let make_operator info op =
      let { FromRefiner.TermType.op_name = opname; FromRefiner.TermType.op_params = params } = FromRefiner.Term.dest_op op in
         { ToRefiner.TermType.op_name = normalize_opname opname;
           ToRefiner.TermType.op_params = List.map (Memo.apply info.copy_param info) params
         }

   let make_param info param =
      match FromRefiner.Term.dest_param param with
         FromRefiner.TermType.Number n1 ->    ToRefiner.TermType.Number n1
       | FromRefiner.TermType.String s1 ->    ToRefiner.TermType.String s1
       | FromRefiner.TermType.Token s1 ->     ToRefiner.TermType.Token s1
       | FromRefiner.TermType.Level l1 ->     ToRefiner.TermType.Level (Memo.apply info.copy_level info l1)
       | FromRefiner.TermType.Var v1 ->       ToRefiner.TermType.Var v1
       | FromRefiner.TermType.MNumber s1 ->   ToRefiner.TermType.MNumber s1
       | FromRefiner.TermType.MString s1 ->   ToRefiner.TermType.MString s1
       | FromRefiner.TermType.MToken s1 ->    ToRefiner.TermType.MToken s1
       | FromRefiner.TermType.MLevel s1 ->    ToRefiner.TermType.MLevel s1
       | FromRefiner.TermType.MVar s1 ->      ToRefiner.TermType.MVar s1
       | FromRefiner.TermType.ObId oid1 ->    ToRefiner.TermType.ObId (List.map (Memo.apply info.copy_param info) oid1)
       | FromRefiner.TermType.ParamList p1 -> ToRefiner.TermType.ParamList (List.map (Memo.apply info.copy_param info) p1)
       | FromRefiner.TermType.MSum (p11, p21) ->
            ToRefiner.TermType.MSum (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromRefiner.TermType.MDiff (p11, p21) ->
            ToRefiner.TermType.MDiff (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromRefiner.TermType.MProduct (p11, p21) ->
            ToRefiner.TermType.MProduct (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromRefiner.TermType.MQuotient (p11, p21) ->
            ToRefiner.TermType.MQuotient (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromRefiner.TermType.MRem (p11, p21) ->
            ToRefiner.TermType.MRem (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromRefiner.TermType.MLessThan (p11, p21) ->
            ToRefiner.TermType.MLessThan (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromRefiner.TermType.MEqual (p11, p21) ->
            ToRefiner.TermType.MEqual (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromRefiner.TermType.MNotEqual (p11, p21) ->
            ToRefiner.TermType.MNotEqual (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)

   let make_level info level =
      let { FromRefiner.TermType.le_const = c; FromRefiner.TermType.le_vars = vars } = FromRefiner.Term.dest_level level in
         { ToRefiner.TermType.le_const = c;
           ToRefiner.TermType.le_vars = List.map (Memo.apply info.copy_level_var info) vars
         }

   let make_level_var info lvar =
      let { FromRefiner.TermType.le_var = var; FromRefiner.TermType.le_offset = offset } = FromRefiner.Term.dest_level_var lvar in
         { ToRefiner.TermType.le_var = var;
           ToRefiner.TermType.le_offset = offset
         }

   (*
    * Create the memo tables.
    *)
   let create () =
      { copy_level_var  = Memo.create (**)
           make_level_var
           (fun _ t -> ToRefiner.Term.make_level_var t)
           compare_level_var;
        copy_level     = Memo.create (**)
           make_level
           (fun _ t -> ToRefiner.Term.make_level t)
           compare_level;
        copy_param     = Memo.create (**)
           make_param
           (fun _ t -> ToRefiner.Term.make_param t)
           compare_param;
        copy_operator  = Memo.create (**)
           make_operator
           (fun _ t -> ToRefiner.Term.make_op t)
           compare_operator;
        copy_term      = Memo.create (**)
           make_term
           do_make_term
           compare_tterm;
        copy_bterm     = Memo.create (**)
           make_bterm
           (fun _ t -> ToRefiner.Term.make_bterm t)
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
      FromRefiner.TermType.MetaTheorem t ->
         ToRefiner.TermType.MetaTheorem (Memo.apply info.copy_term info t)
    | FromRefiner.TermType.MetaImplies (t1, t2) ->
         ToRefiner.TermType.MetaImplies (copy_meta_term info t1,
                             copy_meta_term info t2)
    | FromRefiner.TermType.MetaFunction (t1, mt1, mt2) ->
         ToRefiner.TermType.MetaFunction (Memo.apply info.copy_term info t1,
                              copy_meta_term info mt1,
                              copy_meta_term info mt2)
    | FromRefiner.TermType.MetaIff (mt1, mt2) ->
         ToRefiner.TermType.MetaIff (copy_meta_term info mt1,
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
   MakeTermCopy (Refiner_std_verb.Refiner) (Refiner.Refiner)

module DenormalizeTerm =
   MakeTermCopy (Refiner.Refiner) (Refiner_std_verb.Refiner)

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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
