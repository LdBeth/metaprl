open List
open Term_sig
open Termmod_sig
open Infinite_ro_array
open Ar_memo
open Opname
open Memo
open Simplehashtbl

module ArrayTermCopy =
  functor(Hash : Simplehash_sig.SimpleHashSig) ->
  functor(FromTerm : Termmod_sig.TermModuleSig) ->
  functor(ToTerm : Termmod_sig.TermModuleSig) ->
struct
   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType
   module FTerm = FromTerm.Term
   module FType = FromTerm.TermType

   module ArMemo = ArrayMemo(Simplehashtbl)(InfiniteROArray)

   module IAr = InfiniteROArray

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

(* level_exp_var' = { le_var : string; le_offset : int } 
   level_exp' = { le_const : int; le_vars : level_exp_var list }
*)

   type level_exp_var_header = string * int

   type level_exp_header = int * ( TType.level_exp_var IAr.descriptor list )

   type param_header =
      Number of Mp_num.num
    | String of string
    | Token of string
    | Level of TType.level_exp IAr.descriptor
    | Var of string
    | MNumber of string
    | MString of string
    | MToken of string
    | MLevel of string
    | MVar of string

      (* Special Nuprl5 values *)
    | ObId of TType.param IAr.descriptor list
    | ParamList of TType.param IAr.descriptor list

      (* Num operations *)
    | MSum of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MDiff of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MProduct of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MQuotient of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MRem of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MLessThan of TType.param IAr.descriptor * TType.param IAr.descriptor

      (* Comparisons *)
    | MEqual of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MNotEqual of TType.param IAr.descriptor * TType.param IAr.descriptor


   (*
     term' = dest_term term = { term_op : operator; term_terms : bound_term list }
     bound_term' = dest_bterm term_term = { bvars : string list; bterm : term }
     operator' = dest_op term_op = { op_name : opname; op_params : param list }
     param' = dest_param op_params = ... | ...
   *)

(* operator' = { op_name : opname; op_params : param list } *)
   type operator_header = opname * TType.param IAr.descriptor list

(* bound_term' = { bvars : string list; bterm : term } *)
   type bound_term_header = string list * TType.term IAr.descriptor

   type hypothesis_header =
      Hypothesis of string * TType.term IAr.descriptor
    | Context of string * TType.term IAr.descriptor list

(* term' = { term_op : operator; term_terms : bound_term list } *)
   type term_header = Term of ( TType.operator IAr.descriptor * TType.bound_term IAr.descriptor list ) 
                    | Seq of ( TType.term IAr.descriptor * TType.hypothesis IAr.descriptor list * TType.term IAr.descriptor list )

   type meta_term_header =
      MetaTheorem of TType.term IAr.descriptor
    | MetaImplies of TType.meta_term IAr.descriptor * TType.meta_term IAr.descriptor
    | MetaFunction of TType.term IAr.descriptor * TType.meta_term IAr.descriptor * TType.meta_term IAr.descriptor
    | MetaIff of TType.meta_term IAr.descriptor * TType.meta_term IAr.descriptor

   type t =
      { copy_level_var  : (t, level_exp_var_header, FType.level_exp_var, TType.level_exp_var) ArMemo.t;
        copy_level      : (t, level_exp_header,     FType.level_exp,     TType.level_exp)     ArMemo.t;
        copy_param      : (t, param_header,         FType.param,         TType.param)         ArMemo.t;
        copy_operator   : (t, operator_header,      FType.operator,      TType.operator)      ArMemo.t;
        copy_bterm      : (t, bound_term_header,    FType.bound_term,    TType.bound_term)    ArMemo.t;
        copy_hypothesis : (t, hypothesis_header,    FType.hypothesis,    TType.hypothesis)    ArMemo.t;
        copy_term       : (t, term_header,          FType.term,          TType.term)          ArMemo.t;
        copy_meta_term  : (t, meta_term_header,     FType.meta_term,     TType.meta_term)     ArMemo.t
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Compare that the elements on the lists are equal.
    *)
   let list_mem_eq = List_util.compare_eq

   let make_level_var_header info lvar =
      let { FType.le_var = var; FType.le_offset = offset } = FTerm.dest_level_var lvar in
         ((var, offset), [])

   let compare_level_var_header ( v1, offset1 ) ( v2, offset2 ) =
      v1 == v2 & offset1 == offset2

   let constr_level_var info (var, offset) lst =
      TTerm.make_level_var 
      { TType.le_var = var;
        TType.le_offset = offset
      }

   let make_level_header info level =
      let { FType.le_const = c; FType.le_vars = vars } = FTerm.dest_level level in
         ( ( c, List.map (ArMemo.index info.copy_level_var info) vars ), [] )

   let compare_level_header ( const1, vars1 ) ( const2, vars2 ) =
      const1 = const2 & list_mem_eq vars1 vars2

   let constr_level info ( c, var_indices ) lst =
      TTerm.make_level
      { TType.le_const = c;
        TType.le_vars = List.map (ArMemo.get info.copy_level_var info) var_indices
      }

   let make_param_header info param =
     ((
      match FTerm.dest_param param with
         FType.Number n1 ->            Number n1
       | FType.String s1 ->            String s1
       | FType.Token s1 ->             Token s1
       | FType.Level l1 ->             Level (ArMemo.index info.copy_level info l1)
       | FType.Var v1 ->               Var v1
       | FType.MNumber s1 ->           MNumber s1
       | FType.MString s1 ->           MString s1
       | FType.MToken s1 ->            MToken s1
       | FType.MLevel s1 ->            MLevel s1
       | FType.MVar s1 ->              MVar s1
       | FType.ObId oid1 ->            ObId (List.map (ArMemo.index info.copy_param info) oid1)
       | FType.ParamList p1 ->         ParamList (List.map (ArMemo.index info.copy_param info) p1)
       | FType.MSum (p11, p21) ->      MSum (ArMemo.index info.copy_param info p11, ArMemo.index info.copy_param info p21)
       | FType.MDiff (p11, p21) ->     MDiff (ArMemo.index info.copy_param info p11, ArMemo.index info.copy_param info p21)
       | FType.MProduct (p11, p21) ->  MProduct (ArMemo.index info.copy_param info p11, ArMemo.index info.copy_param info p21)
       | FType.MQuotient (p11, p21) -> MQuotient (ArMemo.index info.copy_param info p11, ArMemo.index info.copy_param info p21)
       | FType.MRem (p11, p21) ->      MRem (ArMemo.index info.copy_param info p11, ArMemo.index info.copy_param info p21)
       | FType.MLessThan (p11, p21) -> MLessThan (ArMemo.index info.copy_param info p11, ArMemo.index info.copy_param info p21)
       | FType.MEqual (p11, p21) ->    MEqual (ArMemo.index info.copy_param info p11, ArMemo.index info.copy_param info p21)
       | FType.MNotEqual (p11, p21) -> MNotEqual (ArMemo.index info.copy_param info p11, ArMemo.index info.copy_param info p21)
    ), [])

   let compare_param_header param1 param2 =
      match param1, param2 with
         Number    n1,         Number    n2         -> Mp_num.eq_num n1 n2
       | String    s1,         String    s2         -> s1 = s2
       | Token     s1,         Token     s2         -> s1 = s2
       | Level     l1,         Level     l2         -> l1 == l2
       | Var       v1,         Var       v2         -> v1 = v2
       | MNumber   s1,         MNumber   s2         -> s1 = s2
       | MString   s1,         MString   s2         -> s1 = s2
       | MToken    s1,         MToken    s2         -> s1 = s2
       | MLevel    s1,         MLevel    s2         -> s1 = s2
       | MVar      s1,         MVar      s2         -> s1 = s2
       | ObId      oid1,       ObId      oid2       -> list_mem_eq oid1 oid2
       | ParamList params1,    ParamList params2    -> list_mem_eq params1 params2
       | MSum      (p11, p12), MSum      (p21, p22) -> p11 == p21 & p12 == p22
       | MDiff     (p11, p12), MDiff     (p21, p22) -> p11 == p21 & p12 == p22
       | MProduct  (p11, p12), MProduct  (p21, p22) -> p11 == p21 & p12 == p22
       | MQuotient (p11, p12), MQuotient (p21, p22) -> p11 == p21 & p12 == p22
       | MRem      (p11, p12), MRem      (p21, p22) -> p11 == p21 & p12 == p22
       | MLessThan (p11, p12), MLessThan (p21, p22) -> p11 == p21 & p12 == p22
       | MEqual    (p11, p12), MEqual    (p21, p22) -> p11 == p21 & p12 == p22
       | MNotEqual (p11, p12), MNotEqual (p21, p22) -> p11 == p21 & p12 == p22
       | _ -> false

   let constr_param info param_header lst =
     TTerm.make_param
     (
      match param_header with
         Number n1 ->            TType.Number n1
       | String s1 ->            TType.String s1
       | Token s1 ->             TType.Token s1
       | Level l1 ->             TType.Level (ArMemo.get info.copy_level info l1)
       | Var v1 ->               TType.Var v1
       | MNumber s1 ->           TType.MNumber s1
       | MString s1 ->           TType.MString s1
       | MToken s1 ->            TType.MToken s1
       | MLevel s1 ->            TType.MLevel s1
       | MVar s1 ->              TType.MVar s1
       | ObId oid1 ->            TType.ObId (List.map (ArMemo.get info.copy_param info) oid1)
       | ParamList p1 ->         TType.ParamList (List.map (ArMemo.get info.copy_param info) p1)
       | MSum (p11, p21) ->      TType.MSum (ArMemo.get info.copy_param info p11, ArMemo.get info.copy_param info p21)
       | MDiff (p11, p21) ->     TType.MDiff (ArMemo.get info.copy_param info p11, ArMemo.get info.copy_param info p21)
       | MProduct (p11, p21) ->  TType.MProduct (ArMemo.get info.copy_param info p11, ArMemo.get info.copy_param info p21)
       | MQuotient (p11, p21) -> TType.MQuotient (ArMemo.get info.copy_param info p11, ArMemo.get info.copy_param info p21)
       | MRem (p11, p21) ->      TType.MRem (ArMemo.get info.copy_param info p11, ArMemo.get info.copy_param info p21)
       | MLessThan (p11, p21) -> TType.MLessThan (ArMemo.get info.copy_param info p11, ArMemo.get info.copy_param info p21)
       | MEqual (p11, p21) ->    TType.MEqual (ArMemo.get info.copy_param info p11, ArMemo.get info.copy_param info p21)
       | MNotEqual (p11, p21) -> TType.MNotEqual (ArMemo.get info.copy_param info p11, ArMemo.get info.copy_param info p21)
     )

   let make_operator_header info op =
      let { FType.op_name = opname; FType.op_params = params } = FTerm.dest_op op in
         (( normalize_opname opname, List.map (ArMemo.index info.copy_param info) params ), [])

   let compare_operator_header (opname1, params1 ) ( opname2, params2 ) =
      opname1 == opname2 & list_mem_eq params1 params2

   let constr_operator info ( opname, param_indices ) lst =
      TTerm.make_op
      { TType.op_name = opname;
        TType.op_params = List.map (ArMemo.get info.copy_param info) param_indices
      }

   let make_bterm_header info bterm =
      let { FType.bvars = bvars; FType.bterm = bterm } = FTerm.dest_bterm bterm in
         (( bvars, ArMemo.index info.copy_term info bterm ), [])

   let compare_bterm_header ( bvars1, bterm1 ) ( bvars2, bterm2 ) =
      bvars1 = bvars2 & bterm1 == bterm2

   let constr_bterm info ( bvars, term_index ) lst =
      TTerm.make_bterm
      { TType.bvars = bvars;
        TType.bterm = ArMemo.get info.copy_term info term_index
       }

   let make_hyp_header info hyp =
     ((
      match hyp with
         FType.Hypothesis (v, t) -> Hypothesis (v, ArMemo.index info.copy_term info t)
       | FType.Context (v, trms) -> Context (v, List.map (ArMemo.index info.copy_term info) trms)
      ), [])

   let compare_hyp_header hyp1 hyp2 =
      (match hyp1, hyp2 with
           Hypothesis (v1,t1),  Hypothesis (v2,t2)   -> v1 = v2 && t1 == t2
         | Context    (v1,ts1), Context    (v2, ts2) -> v1 = v2 && list_mem_eq ts1 ts2
         | _ -> false)

   let constr_hyp info hyp lst =
      match hyp with
         Hypothesis (v, t) -> TType.Hypothesis (v, ArMemo.get info.copy_term info t)
       | Context (v, trms) -> TType.Context (v, List.map (ArMemo.get info.copy_term info) trms)

   let make_term_header info t =
     ((
      if FromTerm.TermMan.is_sequent_term t then
         let { FType.sequent_args = args;
               FType.sequent_hyps = hyps;
               FType.sequent_goals = goals } = (FromTerm.TermMan.explode_sequent t)
         in 
            Seq
               ( ArMemo.index info.copy_term info args,
                 map (ArMemo.index info.copy_hypothesis info) (FTerm.SeqHyp.to_list hyps),
                 map (ArMemo.index info.copy_term info) (FTerm.SeqGoal.to_list goals)
               )
      else
         let { FType.term_op = op; FType.term_terms = bterms } = FTerm.dest_term t
         in 
            Term
               ( ArMemo.index info.copy_operator info op,
                 List.map (ArMemo.index info.copy_bterm info) bterms )
      ), [])

   let compare_term_header ( op1, bterms1 ) ( op2, bterms2 ) =
      op1 == op2 & list_mem_eq bterms1 bterms2

   let compare_tterm_header t1 t2 =
      match (t1,t2) with
         Term t1, Term t2 ->
            compare_term_header t1 t2
       | Seq ( arg1, hyp1, goal1 ),
         Seq ( arg2, hyp2, goal2 ) ->
            (arg1 == arg2) &&
            (try fold_right (&&) (map2 (==) hyp1 hyp2) true 
             with Invalid_argument lst -> false) &&
            (try fold_right (&&) (map2 (==) goal1 goal2) true
             with Invalid_argument lst -> false)
       | _ -> false

   let constr_term info ( op, bterm_indices ) =
      TTerm.make_term
      { TType.term_op = ArMemo.get info.copy_operator info op;
        TType.term_terms = List.map (ArMemo.get info.copy_bterm info) bterm_indices }

   let constr_tterm info t lst =
      match t with
         Seq ( args, hyps, goals ) ->
               ToTerm.TermMan.mk_sequent_term
               { TType.sequent_args = ArMemo.get info.copy_term info args;
                 TType.sequent_hyps  = TTerm.SeqHyp.of_list  (List.map (ArMemo.get info.copy_hypothesis info) hyps  );
                 TType.sequent_goals = TTerm.SeqGoal.of_list (List.map (ArMemo.get info.copy_term       info) goals )
               }
       | Term t -> constr_term info t

   let make_meta_term_header_aux info mt =
      match mt with
      FType.MetaTheorem t ->
         MetaTheorem (ArMemo.index info.copy_term info t)
    | FType.MetaImplies (t1, t2) ->
         MetaImplies (ArMemo.index info.copy_meta_term info t1,
                      ArMemo.index info.copy_meta_term info t2)
    | FType.MetaFunction (t1, mt1, mt2) ->
         MetaFunction (ArMemo.index info.copy_term info t1,
                       ArMemo.index info.copy_meta_term info mt1,
                       ArMemo.index info.copy_meta_term info mt2)
    | FType.MetaIff (mt1, mt2) ->
         MetaIff (ArMemo.index info.copy_meta_term info mt1,
                  ArMemo.index info.copy_meta_term info mt2)

   let make_meta_term_header info mt = (make_meta_term_header_aux info mt, [])

   let compare_meta_term_header mt_a mt_b =
      match mt_a, mt_b with
      MetaTheorem t1, MetaTheorem t2 -> t1==t2
    | MetaImplies (t11, t12), MetaImplies (t21, t22) -> t11==t21 && t12==t22
    | MetaFunction (t1, mt11, mt12), MetaFunction (t2, mt21, mt22) -> t1==t2 && mt11==mt21 && mt12==mt22
    | MetaIff (mt11, mt12), MetaIff (mt21, mt22) -> mt11==mt21 && mt12==mt22
    | _ -> false

   let constr_meta_term info mt lst =
      match mt with
      MetaTheorem t ->
         TType.MetaTheorem (ArMemo.get info.copy_term info t)
    | MetaImplies (t1, t2) ->
         TType.MetaImplies (ArMemo.get info.copy_meta_term info t1,
                            ArMemo.get info.copy_meta_term info t2)
    | MetaFunction (t1, mt1, mt2) ->
         TType.MetaFunction (ArMemo.get info.copy_term info t1,
                             ArMemo.get info.copy_meta_term info mt1,
                             ArMemo.get info.copy_meta_term info mt2)
    | MetaIff (mt1, mt2) ->
         TType.MetaIff (ArMemo.get info.copy_meta_term info mt1,
                        ArMemo.get info.copy_meta_term info mt2)

   let create () =
      {
        copy_level_var = ArMemo.create make_level_var_header compare_level_var_header constr_level_var;
        copy_level = ArMemo.create make_level_header compare_level_header constr_level;
        copy_param = ArMemo.create make_param_header compare_param_header constr_param;
        copy_operator = ArMemo.create make_operator_header compare_operator_header constr_operator;
        copy_bterm = ArMemo.create make_bterm_header compare_bterm_header constr_bterm;
        copy_hypothesis = ArMemo.create make_hyp_header compare_hyp_header constr_hyp;
        copy_term = ArMemo.create make_term_header compare_tterm_header constr_tterm;
        copy_meta_term = ArMemo.create make_meta_term_header compare_meta_term_header constr_meta_term
      }

   let copy_term info t = ArMemo.apply info.copy_term info t
   let copy_meta_term info mt = ArMemo.apply info.copy_meta_term info mt

   let copy_term_single t = copy_term (create ()) t
   let copy_meta_term_single mt = copy_meta_term (create ()) mt
end   

(*
 * Common cases.
 *)
module NormalizeTerm =
   ArrayTermCopy (Simplehashtbl) (Refiner_std_verb.Refiner) (Refiner.Refiner)

module DenormalizeTerm =
   ArrayTermCopy (Simplehashtbl) (Refiner.Refiner) (Refiner_std_verb.Refiner)

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



