(*
 * Run two refiners in parallel for debugging purposes.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Term_sig
open Refiner_sig
open Termmod_sig

open Lm_symbol
open Lm_printf
open Lm_array_util
open Opname

module MakeRefinerDebug (Refiner1 : RefinerSig) (Refiner2 : RefinerSig) = struct
   module TermType = struct
      type term = Refiner1.TermType.term * Refiner2.TermType.term
      type bound_term = Refiner1.TermType.bound_term * Refiner2.TermType.bound_term
      type operator = Refiner1.TermType.operator * Refiner2.TermType.operator
      type param = Refiner1.TermType.param * Refiner2.TermType.param
      type address = Refiner1.TermAddr.address * Refiner2.TermAddr.address
      type level_exp_var = Refiner1.TermType.level_exp_var * Refiner2.TermType.level_exp_var
      type level_exp = Refiner1.TermType.level_exp * Refiner2.TermType.level_exp
      type seq_hyps = Refiner1.TermType.seq_hyps * Refiner2.TermType.seq_hyps

      type level_exp_var' = { le_var : var; le_offset : int }
      type level_exp' = { le_const : int; le_vars : level_exp_var list }
      type operator' = { op_name : opname; op_params : param list }
      type term' = { term_op : operator; term_terms : bound_term list }
      type bound_term' = { bvars : var list; bterm : term }
      type object_id = param list
      type param' = (level_exp, param) poly_param
      type meta_term = term poly_meta_term
      type hypothesis = term poly_hypothesis
      type esequent = { sequent_args : term; sequent_hyps : seq_hyps; sequent_concl : term }

      type match_param =
         MatchNumber of Lm_num.num * int option
       | MatchString of string
       | MatchToken of string
       | MatchVar of var
       | MatchLevel of level_exp
       | MatchUnsupported

      type match_term =
         MatchTerm of string list * match_param list * bound_term' list
       | MatchSequent of string list * match_term list * hypothesis list * term

   end

   open TermType

   (* Helper functions *)
   module Type1 = Refiner1.TermType
   module Type2 = Refiner2.TermType
   module Term1 = Refiner1.Term
   module Term2 = Refiner2.Term
   module TermAddr1 = Refiner1.TermAddr
   module TermAddr2 = Refiner2.TermAddr
   module TermOp1 = Refiner1.TermOp
   module TermOp2 = Refiner2.TermOp
   module Subst1 = Refiner1.TermSubst
   module Subst2 = Refiner2.TermSubst
   module SeqHyp1 = Refiner1.Term.SeqHyp
   module SeqHyp2 = Refiner2.Term.SeqHyp

   let lev2_of_lev1 lev =
      let { Type1.le_var = v; Type1.le_offset = i } = Term1.dest_level_var lev in
         Term2.mk_level_var v i

   let levex2_of_levex1 levex =
      let { Type1.le_const = c; Type1.le_vars = vs } = Term1.dest_level levex in
      Term2.mk_level c (List.map lev2_of_lev1 vs)

   let rec param2_of_param1 p =
      let p =
         match Term1.dest_param p with
            (Number _ | String _ | Token _ | Var _ | MNumber _ | MString _ | MToken _ | Quote) as p -> p
          | MLevel l -> MLevel (levex2_of_levex1 l)
          | ObId pl -> ObId (List.map param2_of_param1 pl)
          | ParamList pl -> ParamList (List.map param2_of_param1 pl)
      in
         Term2.make_param p

   let op2_of_op1 op =
      let { Type1.op_name = name; Type1.op_params = pl } = Term1.dest_op op in
         Term2.mk_op name (List.map param2_of_param1 pl)

   let rec term2_of_term1 t =
      let { Type1.term_op = op; Type1.term_terms = btl } = Term1.dest_term t in
         Term2.mk_term (op2_of_op1 op) (List.map bterm2_of_bterm1 btl)

   and bterm2_of_bterm1 bt =
      let { Type1.bvars = vs; Type1.bterm = bt } = Term1.dest_bterm bt in
         Term2.mk_bterm vs (term2_of_term1 bt)

   let lev1_of_lev2 lev =
      let { Type2.le_var = v; Type2.le_offset = i } = Term2.dest_level_var lev in
         Term1.mk_level_var v i

   let levex1_of_levex2 levex =
      let { Type2.le_const = c; Type2.le_vars = vs } = Term2.dest_level levex in
      Term1.mk_level c (List.map lev1_of_lev2 vs)

   let rec param1_of_param2 p =
      let p =
         match Term2.dest_param p with
            (Number _ | String _ | Token _ | Var _ | MNumber _ | MString _ | MToken _ | Quote) as p -> p
          | MLevel l -> MLevel (levex1_of_levex2 l)
          | ObId pl -> ObId (List.map param1_of_param2 pl)
          | ParamList pl -> ParamList (List.map param1_of_param2 pl)
      in
         Term1.make_param p

   let op1_of_op2 op =
      let { Type2.op_name = name; Type2.op_params = pl } = Term2.dest_op op in
         Term1.mk_op name (List.map param1_of_param2 pl)

   let rec term1_of_term2 t =
      let { Type2.term_op = op; Type2.term_terms = btl } = Term2.dest_term t in
         Term1.mk_term (op1_of_op2 op) (List.map bterm1_of_bterm2 btl)

   and bterm1_of_bterm2 bt =
      let { Type2.bvars = vs; Type2.bterm = bt } = Term2.dest_bterm bt in
         Term1.mk_bterm vs (term1_of_term2 bt)

   let term_of_term1 t = t, (term2_of_term1 t)
   let term_of_term2 t = (term1_of_term2 t), t

   let print_term_ref = ref (fun _ _ -> raise (Failure "Refiner_debug.Term.print_term: printer not installed"))

   let print_term out (t: term) =
      !print_term_ref out t

   let rec print_term_list out = function
      [t] ->
         print_term out t
    | h::t ->
         print_term out h;
         output_string out ", ";
         print_term_list out t
    | [] ->
         ()

   (*
    * We use a separate error reporting function to have a single breakpoint
    * location that can be used to catch _all_ error in the debugger
    *)
   let report_error x msg =
      raise (Invalid_argument ("Found a mismatch in function " ^ x ^ ": " ^ msg))

   let split = List.split

   let split_term' { term_op = (op1, op2); term_terms = btl } =
      let btl1, btl2 = split btl in
         { Type1.term_op = op1; Type1.term_terms = btl1 },
         { Type2.term_op = op2; Type2.term_terms = btl2 }

   let split_bterm' { bvars = vs; bterm = bt1, bt2 } =
      { Type1.bvars = vs; Type1.bterm = bt1 },
      { Type2.bvars = vs; Type2.bterm = bt2 }

   let split_op' { op_name = name; op_params = pars } =
      let pl1, pl2 = split pars in
         { Type1.op_name = name; Type1.op_params = pl1 },
         { Type2.op_name = name; Type2.op_params = pl2 }

   let split_level_exp_var' { le_var = v; le_offset = i } =
      { Type1.le_var = v; Type1.le_offset = i },
      { Type2.le_var = v; Type2.le_offset = i }

   let split_level_exp' { le_const = c; le_vars = vs } =
      let vs1, vs2 = split vs in
         { Type1.le_const = c; Type1.le_vars = vs1 },
         { Type2.le_const = c; Type2.le_vars = vs2 }

   let split_param' = function
      (Number _ | String _ | Token _ | Var _ | MNumber _ | MString _ | MToken _ | Quote) as p -> p, p
    | MLevel (l1, l2) -> MLevel l1, MLevel l2
    | ObId pl -> let pl1, pl2 = split pl in ObId pl1, ObId pl2
    | ParamList pl -> let pl1, pl2 = split pl in ParamList pl1, ParamList pl2

   let split_hyp = function
      Hypothesis (v, (t1, t2)) ->
         Hypothesis (v, t1), Hypothesis (v, t2)
    | Context(v, vs, ts) ->
         let ts1, ts2 = split ts in
            Context (v, vs, ts1), Context(v, vs, ts2)

   let split_hyps hs =
      split (List.map split_hyp hs)

   let split_ttf f =
      (fun t1 -> fst (f (term_of_term1 t1))),
      (fun t2 -> snd (f (term_of_term2 t2)))

   let split_attf f =
      (fun a t1 -> fst (f a (term_of_term1 t1))),
      (fun a t2 -> snd (f a (term_of_term2 t2)))

   let split_atf f =
      (fun a -> fst (f a)), (fun a -> snd (f a))

   let split_ttaf f =
      (fun t1 -> let t, res = f (term_of_term1 t1) in fst t, res),
      (fun t2 -> let t, res = f (term_of_term2 t2) in snd t, res)

   let split_attaf f =
      (fun a t1 -> let t, res = f a (term_of_term1 t1) in fst t, res),
      (fun a t2 -> let t, res = f a (term_of_term2 t2) in snd t, res)

   let merge_poly x v1 v2 =
      if v1 <> v2 then
         report_error x "Polymorphic merge";
      v1

   let merge_bool x (b1:bool) b2 =
      if b1 <> b2 then
         report_error x "Booleans mismatch";
      b1

   let merge_int x (i1: int) i2 =
      if i1 <> i2 then
         report_error x "Integers mismatch";
      i1

   let merge_var x (v1:var) v2 =
      if v1 <> v2 then
         report_error x "Variable mismatch";
      v1

   let merge_string x s1 s2 =
      if s1 <> s2 then
         report_error x ("Strings mismatch: \"" ^s1^"\" vs \""^s2^"\"");
      s1

   let merge_num x n1 n2 =
      if not (Lm_num.eq_num n1 n2) then
          report_error x "nums mismatch";
      n1

   let merge_unit x () () = ()

   let merge_ints x is1 is2 =
      if not (List.length is1 = List.length is2) then
         report_error x "integer lists length mismatch";
      List.map2 (merge_int x) is1 is2

   let merge_ss x (s1 : SymbolSet.t) s2 in
      if not (SymbolSet.equal s1 s2) then
         report_error x "Symbol sets mismatch";
      s1

   let merge_param x p1 p2 =
      (* XXX: TODO: need some consistency checks *)
      p1, p2

   let merge_level_exp_var x v1 v2 =
      (* XXX: TODO: need some consistency checks *)
      v1, v2

   let merge_level_exp x le1 le2 =
      (* XXX: TODO: need some consistency checks *)
      le1, le2

   let merge_address x a1 a2 =
      (* XXX: TODO: need some consistency checks *)
      a1, a2

   let merge_addresss x as1 as2 =
      if not (List.length as1 = List.length as2) then
         report_error x "address list length mismatch";
      List.map2 (merge_address x) as1 as2

   let merge_params x pl1 pl2 =
      if not (List.length pl1 = List.length pl2) then
         report_error x "param list length mismatch";
      List.map2 (merge_param x) pl1 pl2

   let merge_param' x p1 p2 =
      match p1, p2 with
         (Number _ | String _ | Token _ | Var _ | MNumber _ | MString _ | MToken _ | Quote as p1),
         (Number _ | String _ | Token _ | Var _ | MNumber _ | MString _ | MToken _ | Quote as p2)
         when p1 = p2 ->
            p1
       | MLevel l1, MLevel l2 -> MLevel (l1, l2)
       | ObId pl1, ObId pl2 -> ObId (merge_params x pl1 pl2)
       | ParamList pl1, ParamList pl2 -> ParamList (merge_params x pl1 pl2)
       | _ -> report_error x "incompatible param'"

   let merge_params' x pl1 pl2 =
      if not (List.length pl1 = List.length pl2) then
         report_error x "param' list length mismatch";
      List.map2 (merge_param' x) pl1 pl2

   let merge_level_exp_vars x vs1 vs2 =
      if not (List.length vs1 = List.length vs2) then
         report_error x "level_exp_var list length mismatch";
      List.map2 (merge_level_exp_var x) vs1 vs2

   let merge_level_exp_var' x { Type1.le_var = v1; Type1.le_offset = i1 } { Type2.le_var = v2; Type2.le_offset = i2 } =
      if not (v1 = v2) then
         report_error x "le_var field mismatch";
      if not (i1 = i2) then
         report_error x "le_offset field mismatch";
      { le_var = v1; le_offset = i1 }

   let merge_level_exp_vars' x vs1 vs2 =
      if not (List.length vs1 = List.length vs2) then
         report_error x "level_exp_var' list length mismatch";
      List.map2 (merge_level_exp_var' x) vs1 vs2

   let merge_level_exp' x { Type1.le_const = c1; Type1.le_vars = vs1 } { Type2.le_const = c2; Type2.le_vars = vs2 } =
      if not (c1 = c2) then
         report_error x "le_const field mismatch";
      { le_const = c1; le_vars = merge_level_exp_vars x vs1 vs2 }

   let merge_op' x { Type1.op_name = name1; Type1.op_params = pl1 } { Type2.op_name = name2; Type2.op_params = pl2 } =
      if not (Opname.eq name1 name2) then
         report_error x "operator' opname mismatch";
      { op_name = name1; op_params = merge_params x pl1 pl2 }

   let merge_op x op1 op2 =
      (* XXX: TODO: need some consistency checks *)
      op1, op2

   let merge_opname x op1 op2 =
      if not (Opname.eq op1 op2) then
         report_error x "opnames mismatch";
      op1

   let merge_term x t1 t2 =
      if not (Opname.eq (Term1.opname_of_term t1) (Term2.opname_of_term t2)) then
         report_error x "term opname mismatch"
      else
         (t1, t2)

   let merge_terms x tl1 tl2 =
      if not (List.length tl1 = List.length tl2) then
         report_error x "term list length mismatch";
      List.map2 (merge_term x) tl1 tl2

   let merge_bterm' x { Type1.bvars = bv1; Type1.bterm = t1 } { Type2.bvars = bv2; Type2.bterm = t2 } =
      if not (List.length bv1 = List.length bv2) then
         report_error x "bvar length mismatch";
      { bvars = bv1; bterm = merge_term x t1 (Subst2.subst t2 (List.rev bv2) (List.rev_map Term2.mk_var_term bv1)) }

   let merge_bterm x bt1 bt2 =
      (* XXX: TODO: need some consistency checks *)
      bt1, bt2

   let merge_bterms x btl1 btl2 =
      if not (List.length btl1 = List.length btl2) then
         report_error x "bterm list length mismatch";
      List.map2 (merge_bterm x) btl1 btl2

   let merge_term' x { Type1.term_op = op1; Type1.term_terms = btl1 } { Type2.term_op = op2; Type2.term_terms = btl2 } =
      { term_op = merge_op x op1 op2; term_terms = merge_bterms x btl1 btl2 }

   let merge_hyp x h1 h2 =
      match h1, h2 with
         Hypothesis (v1, t1), Hypothesis (v2, t2) ->
            if not (v1 = v2) then
               report_error x "Hyp variable mismatch";
            Hypothesis (v1, merge_term x t1 t2)
       | Context (v1, vs1, ts1), Context (v2, vs2, ts2) ->
            if not (v1 = v2) then
               report_error x "Context variable mismatch";
            if not (vs1 = vs2) then
               report_error x "Contexts of a context mismatch";
            Context (v1, vs1, merge_terms x ts1 ts2)
       | _ ->
            report_error x "hypothesis kind mismatch"

   let merge_hyps x hs1 hs2 =
      if not (List.length hs1 = List.length hs2) then
         report_error x "hyp list length mismatch";
      List.map2 (merge_hyp x) hs1 hs2

   let merge_SeqHyp x hyps1 hyps2 =
      if not (SeqHyp1.length hyps1 = SeqHyp2.length hyps2) then
         report_error x "SeqHyp.length mismatch on merge";
      hyps1, hyps2

   module SeqHyp = struct
      type elt = hypothesis
      type t = seq_hyps
      type index = int

      let empty =
         merge_SeqHyp "SeqHyp.empty" SeqHyp1.empty SeqHyp2.empty

      let singleton h =
         let h1, h2 = split_hyp h in
            merge_SeqHyp "SeqHyp.singleton" (SeqHyp1.singleton h1) (SeqHyp2.singleton h2)

      let length (t1, t2) =
         merge_int "SeqHyp.length" (SeqHyp1.length t1) (SeqHyp2.length t2)

      let make i h =
         let h1, h2 = split_hyp h in
            merge_SeqHyp "SeqHyp.make" (SeqHyp1.make i h1) (SeqHyp2.make i h2)

      let create = make

      let get (t1, t2) i =
         merge_hyp "SeqHyp.get" (SeqHyp1.get t1 i) (SeqHyp2.get t2 i)

      let to_list (t1, t2) =
         merge_hyps "SeqHyp.to_list" (SeqHyp1.to_list t1) (SeqHyp2.to_list t2)

      let of_list hl =
         let hl1, hl2 = split_hyps hl in
            merge_SeqHyp "SeqHyp.of_list" (SeqHyp1.of_list hl1) (SeqHyp2.of_list hl2)

      let append (t1, t2) h (tt1, tt2) =
         let h1, h2 = split_hyp h in
            merge_SeqHyp "SeqHyp.append" (SeqHyp1.append t1 h1 tt1) (SeqHyp2.append t2 h2 tt2)

      let append_list (t1, t2) (hs : elt list) (tt1, tt2) =
         let hs1, hs2 = split_hyps hs in
            merge_SeqHyp "SeqHyp.append_list" (SeqHyp1.append_list t1 hs1 tt1) (SeqHyp2.append_list t2 hs2 tt2)

      let split (t1, t2) i =
         let l1, h1, r1 = SeqHyp1.split t1 i in
         let l2, h2, r2 = SeqHyp2.split t2 i in
            (merge_SeqHyp "SeqHyp.split - 1" l1 l2),
            (merge_hyp "SeqHyp.split - 2" h1 h2),
            (merge_SeqHyp "SeqHyp.split - 3" r1 r2)

      let init i f =
         merge_SeqHyp "SeqHyp.init" (SeqHyp1.init i (fun i -> fst (split_hyp (f i)))) (SeqHyp2.init i (fun i -> snd (split_hyp (f i))))

      (* XXX: TODO: we do not use the underlying implementation here, so it is not fully tested *)
      let iter f t = List.iter f (to_list t)
      let map f t = of_list (List.map f (to_list t))
      let lazy_apply = map
      let mapi f t = init (length t) (fun i -> f i (get t i))
      let lazy_sub_map f t i len = of_list (Array.to_list (Lm_array_util.sub_map f (Array.of_list (to_list t)) i len))
      let map_part = function
         ArrayElement a -> ArrayElement a
       | ArrayArray (t, i, j) -> ArrayArray (Array.of_list (to_list t), i, j)
      let collect ps = of_list (Array.to_list (Lm_array_util.collect (List.map map_part ps)))
   end

   module Term = struct
      module TermTypes = TermType
      module SeqHyp = SeqHyp

      let debug_print = print_term
      let print_term = print_term
      let print_term_list = print_term_list

      let install_debug_printer f =
         let print_term1 out t = f out (term_of_term1 t) in
         let print_term2 out t = f out (term_of_term2 t) in
         let print_both_terms out (t1, t2) =
            fprintf out "Implementation 1: %a\nImplementation 2: %a%t" print_term1 t1 print_term2 t2 eflush
         in
            Term1.install_debug_printer print_term1;
            Term2.install_debug_printer print_term2;
            print_term_ref := print_both_terms

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let mk_term (p0 : operator) (p1 : bound_term list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split p1 in
         merge_term "Term.mk_term" (Term1.mk_term p0_1 p1_1) (Term2.mk_term p0_2 p1_2)

      let make_term (p0 : term') =
         let p0_1, p0_2 = split_term' p0 in
         merge_term "Term.make_term" (Term1.make_term p0_1) (Term2.make_term p0_2)

      let dest_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_term' "Term.dest_term" (Term1.dest_term p0_1) (Term2.dest_term p0_2)

      let mk_op (p0 : opname) (p1 : param list) =
         let p1_1, p1_2 = split p1 in
         merge_op "Term.mk_op" (Term1.mk_op p0 p1_1) (Term2.mk_op p0 p1_2)

      let make_op (p0 : operator') =
         let p0_1, p0_2 = split_op' p0 in
         merge_op "Term.make_op" (Term1.make_op p0_1) (Term2.make_op p0_2)

      let dest_op (p0 : operator) =
         let p0_1, p0_2 = p0 in
         merge_op' "Term.dest_op" (Term1.dest_op p0_1) (Term2.dest_op p0_2)

      let ops_eq (p0 : operator) (p1 : operator) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge_bool "Term.ops_eq" (Term1.ops_eq p0_1 p1_1) (Term2.ops_eq p0_2 p1_2)

      let mk_bterm (p0 : var list) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bterm "Term.mk_bterm" (Term1.mk_bterm p0 p1_1) (Term2.mk_bterm p0 p1_2)

      let make_bterm (p0 : bound_term') =
         let p0_1, p0_2 = split_bterm' p0 in
         merge_bterm "Term.make_bterm" (Term1.make_bterm p0_1) (Term2.make_bterm p0_2)

      let dest_bterm (p0 : bound_term) =
         let p0_1, p0_2 = p0 in
         merge_bterm' "Term.dest_bterm" (Term1.dest_bterm p0_1) (Term2.dest_bterm p0_2)

      let make_param (p0 : param') =
         let p0_1, p0_2 = split_param' p0 in
         merge_param "Term.make_param" (Term1.make_param p0_1) (Term2.make_param p0_2)

      let dest_param (p0 : param) =
         let p0_1, p0_2 = p0 in
         merge_param' "Term.dest_param" (Term1.dest_param p0_1) (Term2.dest_param p0_2)

      let dest_params (p0 : param list) =
         let p0_1, p0_2 = split p0 in
         merge_params' "Term.dest_params" (Term1.dest_params p0_1) (Term2.dest_params p0_2)

      let mk_level (p0 : int) (p1 : level_exp_var list) =
         let p1_1, p1_2 = split p1 in
         merge_level_exp "Term.mk_level" (Term1.mk_level p0 p1_1) (Term2.mk_level p0 p1_2)

      let make_level (p0 : level_exp') =
         let p0_1, p0_2 = split_level_exp' p0 in
         merge_level_exp "Term.make_level" (Term1.make_level p0_1) (Term2.make_level p0_2)

      let dest_level (p0 : level_exp) =
         let p0_1, p0_2 = p0 in
         merge_level_exp' "Term.dest_level" (Term1.dest_level p0_1) (Term2.dest_level p0_2)

      let mk_level_var (p0 : var) (p1 : int) =
         merge_level_exp_var "Term.mk_level_var" (Term1.mk_level_var p0 p1) (Term2.mk_level_var p0 p1)

      let make_level_var (p0 : level_exp_var') =
         let p0_1, p0_2 = split_level_exp_var' p0 in
         merge_level_exp_var "Term.make_level_var" (Term1.make_level_var p0_1) (Term2.make_level_var p0_2)

      let dest_level_var (p0 : level_exp_var) =
         let p0_1, p0_2 = p0 in
         merge_level_exp_var' "Term.dest_level_var" (Term1.dest_level_var p0_1) (Term2.dest_level_var p0_2)

      let make_object_id (p0 : param list) =
         let p0_1, p0_2 = split p0 in
         merge_params "Term.make_object_id" (Term1.make_object_id p0_1) (Term2.make_object_id p0_2)

      let dest_object_id (p0 : object_id) =
         let p0_1, p0_2 = split p0 in
         merge_params "Term.dest_object_id" (Term1.dest_object_id p0_1) (Term2.dest_object_id p0_2)

      let opname_of_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_opname "Term.opname_of_term" (Term1.opname_of_term p0_1) (Term2.opname_of_term p0_2)

      let subterms_of_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_terms "Term.subterms_of_term" (Term1.subterms_of_term p0_1) (Term2.subterms_of_term p0_2)

      let subterm_arities (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_ints "Term.subterm_arities" (Term1.subterm_arities p0_1) (Term2.subterm_arities p0_2)

      let is_var_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_bool "Term.is_var_term" (Term1.is_var_term p0_1) (Term2.is_var_term p0_2)

      let dest_var (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_var "Term.dest_var" (Term1.dest_var p0_1) (Term2.dest_var p0_2)

      let mk_var_term (p0 : var) =
         merge_term "Term.mk_var_term" (Term1.mk_var_term p0) (Term2.mk_var_term p0)

      let mk_any_term (p0 : operator) (p1 : term list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split p1 in
         merge_term "Term.mk_any_term" (Term1.mk_any_term p0_1 p1_1) (Term2.mk_any_term p0_2 p1_2)

      let mk_simple_term (p0 : opname) (p1 : term list) =
         let p1_1, p1_2 = split p1 in
         merge_term "Term.mk_simple_term" (Term1.mk_simple_term p0 p1_1) (Term2.mk_simple_term p0 p1_2)

      let dest_simple_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1 = Term1.dest_simple_term p0_1 in
         let res0_2, res1_2 = Term2.dest_simple_term p0_2 in
         (merge_opname "Term.dest_simple_term - 0" res0_1 res0_2),
         (merge_terms "Term.dest_simple_term - 1" res1_1 res1_2)

      let is_simple_term_opname (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "Term.is_simple_term_opname" (Term1.is_simple_term_opname p0 p1_1) (Term2.is_simple_term_opname p0 p1_2)

      let dest_simple_term_opname (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_terms "Term.dest_simple_term_opname" (Term1.dest_simple_term_opname p0 p1_1) (Term2.dest_simple_term_opname p0 p1_2)

      let is_simple_bterm (p0 : bound_term) =
         let p0_1, p0_2 = p0 in
         merge_bool "Term.is_simple_bterm" (Term1.is_simple_bterm p0_1) (Term2.is_simple_bterm p0_2)

      let mk_simple_bterm (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_bterm "Term.mk_simple_bterm" (Term1.mk_simple_bterm p0_1) (Term2.mk_simple_bterm p0_2)

      let dest_simple_bterm (p0 : bound_term) =
         let p0_1, p0_2 = p0 in
         merge_term "Term.dest_simple_bterm" (Term1.dest_simple_bterm p0_1) (Term2.dest_simple_bterm p0_2)

   end

   module TermOp = struct
      module OpTypes = TermType

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let map_down (p0 : (term -> term)) (p1 : term) =
         let p0_1, p0_2 = split_ttf p0 in
         let p1_1, p1_2 = p1 in
         merge_term "TermOp.map_down" (TermOp1.map_down p0_1 p1_1) (TermOp2.map_down p0_2 p1_2)

      let map_up (p0 : (term -> term)) (p1 : term) =
         let p0_1, p0_2 = split_ttf p0 in
         let p1_1, p1_2 = p1 in
         merge_term "TermOp.map_up" (TermOp1.map_up p0_1 p1_1) (TermOp2.map_up p0_2 p1_2)

      let is_quoted_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_bool "TermOp.is_quoted_term" (TermOp1.is_quoted_term p0_1) (TermOp2.is_quoted_term p0_2)

      let quote_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_term "TermOp.quote_term" (TermOp1.quote_term p0_1) (TermOp2.quote_term p0_2)

      let unquote_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_term "TermOp.unquote_term" (TermOp1.unquote_term p0_1) (TermOp2.unquote_term p0_2)

      let is_no_subterms_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_no_subterms_term" (TermOp1.is_no_subterms_term p0 p1_1) (TermOp2.is_no_subterms_term p0 p1_2)

      let is_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_term" (TermOp1.is_dep0_term p0 p1_1) (TermOp2.is_dep0_term p0 p1_2)

      let mk_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_term "TermOp.mk_dep0_term" (TermOp1.mk_dep0_term p0 p1_1) (TermOp2.mk_dep0_term p0 p1_2)

      let dest_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_term "TermOp.dest_dep0_term" (TermOp1.dest_dep0_term p0 p1_1) (TermOp2.dest_dep0_term p0 p1_2)

      let one_subterm (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_term "TermOp.one_subterm" (TermOp1.one_subterm p0_1) (TermOp2.one_subterm p0_2)

      let one_subterm_opname (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_term "TermOp.one_subterm_opname" (TermOp1.one_subterm_opname p0 p1_1) (TermOp2.one_subterm_opname p0 p1_2)

      let is_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep0_term" (TermOp1.is_dep0_dep0_term p0 p1_1) (TermOp2.is_dep0_dep0_term p0 p1_2)

      let mk_dep0_dep0_term (p0 : opname) (p1 : term) (p2 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge_term "TermOp.mk_dep0_dep0_term" (TermOp1.mk_dep0_dep0_term p0 p1_1 p2_1) (TermOp2.mk_dep0_dep0_term p0 p1_2 p2_2)

      let dest_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1 = TermOp1.dest_dep0_dep0_term p0 p1_1 in
         let res0_2, res1_2 = TermOp2.dest_dep0_dep0_term p0 p1_2 in
         (merge_term "TermOp.dest_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_term - 1" res1_1 res1_2)

      let two_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1 = TermOp1.two_subterms p0_1 in
         let res0_2, res1_2 = TermOp2.two_subterms p0_2 in
         (merge_term "TermOp.two_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.two_subterms - 1" res1_1 res1_2)

      let two_subterms_opname (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1 = TermOp1.two_subterms_opname p0 p1_1 in
         let res0_2, res1_2 = TermOp2.two_subterms_opname p0 p1_2 in
         (merge_term "TermOp.two_subterms_opname - 0" res0_1 res0_2),
         (merge_term "TermOp.two_subterms_opname - 1" res1_1 res1_2)

      let is_dep0_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep0_dep0_term" (TermOp1.is_dep0_dep0_dep0_term p0 p1_1) (TermOp2.is_dep0_dep0_dep0_term p0 p1_2)

      let mk_dep0_dep0_dep0_term (p0 : opname) (p1 : term) (p2 : term) (p3 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge_term "TermOp.mk_dep0_dep0_dep0_term" (TermOp1.mk_dep0_dep0_dep0_term p0 p1_1 p2_1 p3_1) (TermOp2.mk_dep0_dep0_dep0_term p0 p1_2 p2_2 p3_2)

      let dest_dep0_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_dep0_dep0_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_dep0_dep0_dep0_term p0 p1_2 in
         (merge_term "TermOp.dest_dep0_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_term - 2" res2_1 res2_2)

      let is_dep0_dep0_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep0_dep0_dep0_term" (TermOp1.is_dep0_dep0_dep0_dep0_term p0 p1_1) (TermOp2.is_dep0_dep0_dep0_dep0_term p0 p1_2)

      let mk_dep0_dep0_dep0_dep0_term (p0 : opname) (p1 : term) (p2 : term) (p3 : term) (p4 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge_term "TermOp.mk_dep0_dep0_dep0_dep0_term" (TermOp1.mk_dep0_dep0_dep0_dep0_term p0 p1_1 p2_1 p3_1 p4_1) (TermOp2.mk_dep0_dep0_dep0_dep0_term p0 p1_2 p2_2 p3_2 p4_2)

      let dest_dep0_dep0_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_dep0_dep0_dep0_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_dep0_dep0_dep0_dep0_term p0 p1_2 in
         (merge_term "TermOp.dest_dep0_dep0_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_dep0_term - 3" res3_1 res3_2)

      let is_two_subterm (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_two_subterm" (TermOp1.is_two_subterm p0 p1_1) (TermOp2.is_two_subterm p0 p1_2)

      let is_three_subterm (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_three_subterm" (TermOp1.is_three_subterm p0 p1_1) (TermOp2.is_three_subterm p0 p1_2)

      let is_five_subterm (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_five_subterm" (TermOp1.is_five_subterm p0 p1_1) (TermOp2.is_five_subterm p0 p1_2)

      let three_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1 = TermOp1.three_subterms p0_1 in
         let res0_2, res1_2, res2_2 = TermOp2.three_subterms p0_2 in
         (merge_term "TermOp.three_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.three_subterms - 1" res1_1 res1_2),
         (merge_term "TermOp.three_subterms - 2" res2_1 res2_2)

      let four_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.four_subterms p0_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.four_subterms p0_2 in
         (merge_term "TermOp.four_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.four_subterms - 1" res1_1 res1_2),
         (merge_term "TermOp.four_subterms - 2" res2_1 res2_2),
         (merge_term "TermOp.four_subterms - 3" res3_1 res3_2)

      let five_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1, res3_1, res4_1 = TermOp1.five_subterms p0_1 in
         let res0_2, res1_2, res2_2, res3_2, res4_2 = TermOp2.five_subterms p0_2 in
         (merge_term "TermOp.five_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.five_subterms - 1" res1_1 res1_2),
         (merge_term "TermOp.five_subterms - 2" res2_1 res2_2),
         (merge_term "TermOp.five_subterms - 3" res3_1 res3_2),
         (merge_term "TermOp.five_subterms - 4" res4_1 res4_2)

      let six_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1, res3_1, res4_1, res5_1 = TermOp1.six_subterms p0_1 in
         let res0_2, res1_2, res2_2, res3_2, res4_2, res5_2 = TermOp2.six_subterms p0_2 in
         (merge_term "TermOp.six_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.six_subterms - 1" res1_1 res1_2),
         (merge_term "TermOp.six_subterms - 2" res2_1 res2_2),
         (merge_term "TermOp.six_subterms - 3" res3_1 res3_2),
         (merge_term "TermOp.six_subterms - 4" res4_1 res4_2),
         (merge_term "TermOp.six_subterms - 5" res5_1 res5_2)

      let is_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep1_term" (TermOp1.is_dep1_term p0 p1_1) (TermOp2.is_dep1_term p0 p1_2)

      let mk_dep1_term (p0 : opname) (p1 : var) (p2 : term) =
         let p2_1, p2_2 = p2 in
         merge_term "TermOp.mk_dep1_term" (TermOp1.mk_dep1_term p0 p1 p2_1) (TermOp2.mk_dep1_term p0 p1 p2_2)

      let dest_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1 = TermOp1.dest_dep1_term p0 p1_1 in
         let res0_2, res1_2 = TermOp2.dest_dep1_term p0 p1_2 in
         (merge_var "TermOp.dest_dep1_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep1_term - 1" res1_1 res1_2)

      let is_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep2_term" (TermOp1.is_dep2_term p0 p1_1) (TermOp2.is_dep2_term p0 p1_2)

      let mk_dep2_term (p0 : opname) (p1 : var) (p2 : var) (p3 : term) =
         let p3_1, p3_2 = p3 in
         merge_term "TermOp.mk_dep2_term" (TermOp1.mk_dep2_term p0 p1 p2 p3_1) (TermOp2.mk_dep2_term p0 p1 p2 p3_2)

      let dest_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_dep2_term p0 p1_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_dep2_term p0 p1_2 in
         (merge_var "TermOp.dest_dep2_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep2_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep2_term - 2" res2_1 res2_2)

      let is_dep1_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep1_dep1_term" (TermOp1.is_dep1_dep1_term p0 p1_1) (TermOp2.is_dep1_dep1_term p0 p1_2)

      let mk_dep1_dep1_term (p0 : opname) (p1 : var) (p2 : term) (p3 : var) (p4 : term) =
         let p2_1, p2_2 = p2 in
         let p4_1, p4_2 = p4 in
         merge_term "TermOp.mk_dep1_dep1_term" (TermOp1.mk_dep1_dep1_term p0 p1 p2_1 p3 p4_1) (TermOp2.mk_dep1_dep1_term p0 p1 p2_2 p3 p4_2)

      let dest_dep1_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_dep1_dep1_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_dep1_dep1_term p0 p1_2 in
         (merge_var "TermOp.dest_dep1_dep1_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep1_dep1_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep1_dep1_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep1_dep1_term - 3" res3_1 res3_2)

      let is_dep0_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep1_term" (TermOp1.is_dep0_dep1_term p0 p1_1) (TermOp2.is_dep0_dep1_term p0 p1_2)

      let is_dep0_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_bool "TermOp.is_dep0_dep1_any_term" (TermOp1.is_dep0_dep1_any_term p0_1) (TermOp2.is_dep0_dep1_any_term p0_2)

      let mk_dep0_dep1_term (p0 : opname) (p1 : var) (p2 : term) (p3 : term) =
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge_term "TermOp.mk_dep0_dep1_term" (TermOp1.mk_dep0_dep1_term p0 p1 p2_1 p3_1) (TermOp2.mk_dep0_dep1_term p0 p1 p2_2 p3_2)

      let mk_dep0_dep1_any_term (p0 : operator) (p1 : var) (p2 : term) (p3 : term) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge_term "TermOp.mk_dep0_dep1_any_term" (TermOp1.mk_dep0_dep1_any_term p0_1 p1 p2_1 p3_1) (TermOp2.mk_dep0_dep1_any_term p0_2 p1 p2_2 p3_2)

      let dest_dep0_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_dep0_dep1_term p0 p1_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_dep0_dep1_term p0 p1_2 in
         (merge_var "TermOp.dest_dep0_dep1_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep1_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep1_term - 2" res2_1 res2_2)

      let dest_dep0_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_dep0_dep1_any_term p0_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_dep0_dep1_any_term p0_2 in
         (merge_var "TermOp.dest_dep0_dep1_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep1_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep1_any_term - 2" res2_1 res2_2)

      let is_dep1_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep1_dep0_term" (TermOp1.is_dep1_dep0_term p0 p1_1) (TermOp2.is_dep1_dep0_term p0 p1_2)

      let mk_dep1_dep0_term (p0 : opname) (p1 : var) (p2 : term) (p3 : term) =
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge_term "TermOp.mk_dep1_dep0_term" (TermOp1.mk_dep1_dep0_term p0 p1 p2_1 p3_1) (TermOp2.mk_dep1_dep0_term p0 p1 p2_2 p3_2)

      let dest_dep1_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_dep1_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_dep1_dep0_term p0 p1_2 in
         (merge_var "TermOp.dest_dep1_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep1_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep1_dep0_term - 2" res2_1 res2_2)

      let is_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep2_term" (TermOp1.is_dep0_dep2_term p0 p1_1) (TermOp2.is_dep0_dep2_term p0 p1_2)

      let mk_dep0_dep2_term (p0 : opname) (p1 : var) (p2 : var) (p3 : term) (p4 : term) =
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge_term "TermOp.mk_dep0_dep2_term" (TermOp1.mk_dep0_dep2_term p0 p1 p2 p3_1 p4_1) (TermOp2.mk_dep0_dep2_term p0 p1 p2 p3_2 p4_2)

      let dest_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_dep0_dep2_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_dep0_dep2_term p0 p1_2 in
         (merge_var "TermOp.dest_dep0_dep2_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep0_dep2_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep2_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep2_term - 3" res3_1 res3_2)

      let is_dep0_dep3_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep3_term" (TermOp1.is_dep0_dep3_term p0 p1_1) (TermOp2.is_dep0_dep3_term p0 p1_2)

      let mk_dep0_dep3_term (p0 : opname) (p1 : var) (p2 : var) (p3 : var) (p4 : term) (p5 : term) =
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         merge_term "TermOp.mk_dep0_dep3_term" (TermOp1.mk_dep0_dep3_term p0 p1 p2 p3 p4_1 p5_1) (TermOp2.mk_dep0_dep3_term p0 p1 p2 p3 p4_2 p5_2)

      let dest_dep0_dep3_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1, res4_1 = TermOp1.dest_dep0_dep3_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2, res4_2 = TermOp2.dest_dep0_dep3_term p0 p1_2 in
         (merge_var "TermOp.dest_dep0_dep3_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep0_dep3_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep3_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep3_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_dep0_dep3_term - 4" res4_1 res4_2)

      let is_dep2_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep2_dep0_term" (TermOp1.is_dep2_dep0_term p0 p1_1) (TermOp2.is_dep2_dep0_term p0 p1_2)

      let mk_dep2_dep0_term (p0 : opname) (p1 : var) (p2 : var) (p3 : term) (p4 : term) =
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge_term "TermOp.mk_dep2_dep0_term" (TermOp1.mk_dep2_dep0_term p0 p1 p2 p3_1 p4_1) (TermOp2.mk_dep2_dep0_term p0 p1 p2 p3_2 p4_2)

      let dest_dep2_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_dep2_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_dep2_dep0_term p0 p1_2 in
         (merge_var "TermOp.dest_dep2_dep0_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep2_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep2_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep2_dep0_term - 3" res3_1 res3_2)

      let is_dep0_dep0_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep0_dep1_term" (TermOp1.is_dep0_dep0_dep1_term p0 p1_1) (TermOp2.is_dep0_dep0_dep1_term p0 p1_2)

      let mk_dep0_dep0_dep1_term (p0 : opname) (p1 : term) (p2 : term) (p3 : var) (p4 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p4_1, p4_2 = p4 in
         merge_term "TermOp.mk_dep0_dep0_dep1_term" (TermOp1.mk_dep0_dep0_dep1_term p0 p1_1 p2_1 p3 p4_1) (TermOp2.mk_dep0_dep0_dep1_term p0 p1_2 p2_2 p3 p4_2)

      let dest_dep0_dep0_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_dep0_dep0_dep1_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_dep0_dep0_dep1_term p0 p1_2 in
         (merge_term "TermOp.dest_dep0_dep0_dep1_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep1_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep0_dep1_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep0_dep1_term - 3" res3_1 res3_2)

      let is_dep0_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep0_dep2_term" (TermOp1.is_dep0_dep0_dep2_term p0 p1_1) (TermOp2.is_dep0_dep0_dep2_term p0 p1_2)

      let mk_dep0_dep0_dep2_term (p0 : opname) (p1 : term) (p2 : term) (p3 : var) (p4 : var) (p5 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p5_1, p5_2 = p5 in
         merge_term "TermOp.mk_dep0_dep0_dep2_term" (TermOp1.mk_dep0_dep0_dep2_term p0 p1_1 p2_1 p3 p4 p5_1) (TermOp2.mk_dep0_dep0_dep2_term p0 p1_2 p2_2 p3 p4 p5_2)

      let dest_dep0_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1, res4_1 = TermOp1.dest_dep0_dep0_dep2_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2, res4_2 = TermOp2.dest_dep0_dep0_dep2_term p0 p1_2 in
         (merge_term "TermOp.dest_dep0_dep0_dep2_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep2_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep0_dep2_term - 2" res2_1 res2_2),
         (merge_var "TermOp.dest_dep0_dep0_dep2_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_dep0_dep0_dep2_term - 4" res4_1 res4_2)

      let is_dep0_dep0_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_bool "TermOp.is_dep0_dep0_dep1_any_term" (TermOp1.is_dep0_dep0_dep1_any_term p0_1) (TermOp2.is_dep0_dep0_dep1_any_term p0_2)

      let mk_dep0_dep0_dep1_any_term (p0 : operator) (p1 : term) (p2 : term) (p3 : var) (p4 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p4_1, p4_2 = p4 in
         merge_term "TermOp.mk_dep0_dep0_dep1_any_term" (TermOp1.mk_dep0_dep0_dep1_any_term p0_1 p1_1 p2_1 p3 p4_1) (TermOp2.mk_dep0_dep0_dep1_any_term p0_2 p1_2 p2_2 p3 p4_2)

      let dest_dep0_dep0_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_dep0_dep0_dep1_any_term p0_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_dep0_dep0_dep1_any_term p0_2 in
         (merge_term "TermOp.dest_dep0_dep0_dep1_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep1_any_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep0_dep1_any_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep0_dep1_any_term - 3" res3_1 res3_2)

      let is_dep0_dep1_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep1_dep1_term" (TermOp1.is_dep0_dep1_dep1_term p0 p1_1) (TermOp2.is_dep0_dep1_dep1_term p0 p1_2)

      let mk_dep0_dep1_dep1_term (p0 : opname) (p1 : term) (p2 : var) (p3 : term) (p4 : var) (p5 : term) =
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = p3 in
         let p5_1, p5_2 = p5 in
         merge_term "TermOp.mk_dep0_dep1_dep1_term" (TermOp1.mk_dep0_dep1_dep1_term p0 p1_1 p2 p3_1 p4 p5_1) (TermOp2.mk_dep0_dep1_dep1_term p0 p1_2 p2 p3_2 p4 p5_2)

      let dest_dep0_dep1_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1, res4_1 = TermOp1.dest_dep0_dep1_dep1_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2, res4_2 = TermOp2.dest_dep0_dep1_dep1_term p0 p1_2 in
         (merge_term "TermOp.dest_dep0_dep1_dep1_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep0_dep1_dep1_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep1_dep1_term - 2" res2_1 res2_2),
         (merge_var "TermOp.dest_dep0_dep1_dep1_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_dep0_dep1_dep1_term - 4" res4_1 res4_2)

      let is_dep0_dep2_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep2_dep0_dep2_term" (TermOp1.is_dep0_dep2_dep0_dep2_term p0 p1_1) (TermOp2.is_dep0_dep2_dep0_dep2_term p0 p1_2)

      let mk_dep0_dep2_dep0_dep2_term (p0 : opname) (p1 : term) (p2 : var) (p3 : var) (p4 : term) (p5 : term) (p6 : var) (p7 : var) (p8 : term) =
         let p1_1, p1_2 = p1 in
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         let p8_1, p8_2 = p8 in
         merge_term "TermOp.mk_dep0_dep2_dep0_dep2_term" (TermOp1.mk_dep0_dep2_dep0_dep2_term p0 p1_1 p2 p3 p4_1 p5_1 p6 p7 p8_1) (TermOp2.mk_dep0_dep2_dep0_dep2_term p0 p1_2 p2 p3 p4_2 p5_2 p6 p7 p8_2)

      let dest_dep0_dep2_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1, res4_1, res5_1, res6_1, res7_1 = TermOp1.dest_dep0_dep2_dep0_dep2_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2, res4_2, res5_2, res6_2, res7_2 = TermOp2.dest_dep0_dep2_dep0_dep2_term p0 p1_2 in
         (merge_term "TermOp.dest_dep0_dep2_dep0_dep2_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep0_dep2_dep0_dep2_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep2_dep0_dep2_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep2_dep0_dep2_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_dep0_dep2_dep0_dep2_term - 4" res4_1 res4_2),
         (merge_var "TermOp.dest_dep0_dep2_dep0_dep2_term - 5" res5_1 res5_2),
         (merge_var "TermOp.dest_dep0_dep2_dep0_dep2_term - 6" res6_1 res6_2),
         (merge_term "TermOp.dest_dep0_dep2_dep0_dep2_term - 7" res7_1 res7_2)

      let is_dep0_dep0_dep3_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep0_dep0_dep3_term" (TermOp1.is_dep0_dep0_dep3_term p0 p1_1) (TermOp2.is_dep0_dep0_dep3_term p0 p1_2)

      let mk_dep0_dep0_dep3_term (p0 : opname) (p1 : term) (p2 : term) (p3 : var) (p4 : var) (p5 : var) (p6 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p6_1, p6_2 = p6 in
         merge_term "TermOp.mk_dep0_dep0_dep3_term" (TermOp1.mk_dep0_dep0_dep3_term p0 p1_1 p2_1 p3 p4 p5 p6_1) (TermOp2.mk_dep0_dep0_dep3_term p0 p1_2 p2_2 p3 p4 p5 p6_2)

      let dest_dep0_dep0_dep3_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1, res4_1, res5_1 = TermOp1.dest_dep0_dep0_dep3_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2, res4_2, res5_2 = TermOp2.dest_dep0_dep0_dep3_term p0 p1_2 in
         (merge_term "TermOp.dest_dep0_dep0_dep3_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep3_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep0_dep3_term - 2" res2_1 res2_2),
         (merge_var "TermOp.dest_dep0_dep0_dep3_term - 3" res3_1 res3_2),
         (merge_var "TermOp.dest_dep0_dep0_dep3_term - 4" res4_1 res4_2),
         (merge_term "TermOp.dest_dep0_dep0_dep3_term - 5" res5_1 res5_2)

      let is_dep2_dep2_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_dep2_dep2_dep0_dep0_term" (TermOp1.is_dep2_dep2_dep0_dep0_term p0 p1_1) (TermOp2.is_dep2_dep2_dep0_dep0_term p0 p1_2)

      let mk_dep2_dep2_dep0_dep0_term (p0 : opname) (p1 : var) (p2 : var) (p3 : term) (p4 : var) (p5 : var) (p6 : term) (p7 : term) (p8 : term) =
         let p3_1, p3_2 = p3 in
         let p6_1, p6_2 = p6 in
         let p7_1, p7_2 = p7 in
         let p8_1, p8_2 = p8 in
         merge_term "TermOp.mk_dep2_dep2_dep0_dep0_term" (TermOp1.mk_dep2_dep2_dep0_dep0_term p0 p1 p2 p3_1 p4 p5 p6_1 p7_1 p8_1) (TermOp2.mk_dep2_dep2_dep0_dep0_term p0 p1 p2 p3_2 p4 p5 p6_2 p7_2 p8_2)

      let dest_dep2_dep2_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1, res4_1, res5_1, res6_1, res7_1 = TermOp1.dest_dep2_dep2_dep0_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2, res4_2, res5_2, res6_2, res7_2 = TermOp2.dest_dep2_dep2_dep0_dep0_term p0 p1_2 in
         (merge_var "TermOp.dest_dep2_dep2_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep2_dep2_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep2_dep2_dep0_dep0_term - 2" res2_1 res2_2),
         (merge_var "TermOp.dest_dep2_dep2_dep0_dep0_term - 3" res3_1 res3_2),
         (merge_var "TermOp.dest_dep2_dep2_dep0_dep0_term - 4" res4_1 res4_2),
         (merge_term "TermOp.dest_dep2_dep2_dep0_dep0_term - 5" res5_1 res5_2),
         (merge_term "TermOp.dest_dep2_dep2_dep0_dep0_term - 6" res6_1 res6_2),
         (merge_term "TermOp.dest_dep2_dep2_dep0_dep0_term - 7" res7_1 res7_2)

      let is_string_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_string_term" (TermOp1.is_string_term p0 p1_1) (TermOp2.is_string_term p0 p1_2)

      let mk_string_term (p0 : opname) (p1 : string) =
         merge_term "TermOp.mk_string_term" (TermOp1.mk_string_term p0 p1) (TermOp2.mk_string_term p0 p1)

      let dest_string_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_string "TermOp.dest_string_term" (TermOp1.dest_string_term p0 p1_1) (TermOp2.dest_string_term p0 p1_2)

      let dest_string_param (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_string "TermOp.dest_string_param" (TermOp1.dest_string_param p0_1) (TermOp2.dest_string_param p0_2)

      let is_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_string_dep0_term" (TermOp1.is_string_dep0_term p0 p1_1) (TermOp2.is_string_dep0_term p0 p1_2)

      let mk_string_dep0_term (p0 : opname) (p1 : string) (p2 : term) =
         let p2_1, p2_2 = p2 in
         merge_term "TermOp.mk_string_dep0_term" (TermOp1.mk_string_dep0_term p0 p1 p2_1) (TermOp2.mk_string_dep0_term p0 p1 p2_2)

      let dest_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1 = TermOp1.dest_string_dep0_term p0 p1_1 in
         let res0_2, res1_2 = TermOp2.dest_string_dep0_term p0 p1_2 in
         (merge_string "TermOp.dest_string_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_string_dep0_term - 1" res1_1 res1_2)

      let is_string_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_string_string_dep0_term" (TermOp1.is_string_string_dep0_term p0 p1_1) (TermOp2.is_string_string_dep0_term p0 p1_2)

      let mk_string_string_dep0_term (p0 : opname) (p1 : string) (p2 : string) (p3 : term) =
         let p3_1, p3_2 = p3 in
         merge_term "TermOp.mk_string_string_dep0_term" (TermOp1.mk_string_string_dep0_term p0 p1 p2 p3_1) (TermOp2.mk_string_string_dep0_term p0 p1 p2 p3_2)

      let dest_string_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_string_string_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_string_string_dep0_term p0 p1_2 in
         (merge_string "TermOp.dest_string_string_dep0_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_string_dep0_term - 2" res2_1 res2_2)

      let dest_string_string_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_string_string_dep0_any_term p0_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_string_string_dep0_any_term p0_2 in
         (merge_string "TermOp.dest_string_string_dep0_any_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_dep0_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_string_dep0_any_term - 2" res2_1 res2_2)

      let is_number_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_number_dep0_term" (TermOp1.is_number_dep0_term p0 p1_1) (TermOp2.is_number_dep0_term p0 p1_2)

      let mk_number_dep0_term (p0 : opname) (p1 : Lm_num.num) (p2 : term) =
         let p2_1, p2_2 = p2 in
         merge_term "TermOp.mk_number_dep0_term" (TermOp1.mk_number_dep0_term p0 p1 p2_1) (TermOp2.mk_number_dep0_term p0 p1 p2_2)

      let dest_number_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1 = TermOp1.dest_number_dep0_term p0 p1_1 in
         let res0_2, res1_2 = TermOp2.dest_number_dep0_term p0 p1_2 in
         (merge_num "TermOp.dest_number_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_number_dep0_term - 1" res1_1 res1_2)

      let dest_number_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1 = TermOp1.dest_number_dep0_any_term p0_1 in
         let res0_2, res1_2 = TermOp2.dest_number_dep0_any_term p0_2 in
         (merge_num "TermOp.dest_number_dep0_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_number_dep0_any_term - 1" res1_1 res1_2)

      let is_number_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_number_dep1_term" (TermOp1.is_number_dep1_term p0 p1_1) (TermOp2.is_number_dep1_term p0 p1_2)

      let mk_number_dep1_term (p0 : opname) (p1 : Lm_num.num) (p2 : var) (p3 : term) =
         let p3_1, p3_2 = p3 in
         merge_term "TermOp.mk_number_dep1_term" (TermOp1.mk_number_dep1_term p0 p1 p2 p3_1) (TermOp2.mk_number_dep1_term p0 p1 p2 p3_2)

      let dest_number_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_number_dep1_term p0 p1_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_number_dep1_term p0 p1_2 in
         (merge_num "TermOp.dest_number_dep1_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_number_dep1_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_number_dep1_term - 2" res2_1 res2_2)

      let dest_number_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_number_dep1_any_term p0_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_number_dep1_any_term p0_2 in
         (merge_num "TermOp.dest_number_dep1_any_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_number_dep1_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_number_dep1_any_term - 2" res2_1 res2_2)

      let is_number_number_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_number_number_dep0_term" (TermOp1.is_number_number_dep0_term p0 p1_1) (TermOp2.is_number_number_dep0_term p0 p1_2)

      let mk_number_number_dep0_term (p0 : opname) (p1 : Lm_num.num) (p2 : Lm_num.num) (p3 : term) =
         let p3_1, p3_2 = p3 in
         merge_term "TermOp.mk_number_number_dep0_term" (TermOp1.mk_number_number_dep0_term p0 p1 p2 p3_1) (TermOp2.mk_number_number_dep0_term p0 p1 p2 p3_2)

      let dest_number_number_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_number_number_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_number_number_dep0_term p0 p1_2 in
         (merge_num "TermOp.dest_number_number_dep0_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_number_number_dep0_term - 2" res2_1 res2_2)

      let dest_number_number_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1 = TermOp1.dest_number_number_dep0_any_term p0_1 in
         let res0_2, res1_2, res2_2 = TermOp2.dest_number_number_dep0_any_term p0_2 in
         (merge_num "TermOp.dest_number_number_dep0_any_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_dep0_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_number_number_dep0_any_term - 2" res2_1 res2_2)

      let is_number_number_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_number_number_string_dep0_term" (TermOp1.is_number_number_string_dep0_term p0 p1_1) (TermOp2.is_number_number_string_dep0_term p0 p1_2)

      let mk_number_number_string_dep0_term (p0 : opname) (p1 : Lm_num.num) (p2 : Lm_num.num) (p3 : string) (p4 : term) =
         let p4_1, p4_2 = p4 in
         merge_term "TermOp.mk_number_number_string_dep0_term" (TermOp1.mk_number_number_string_dep0_term p0 p1 p2 p3 p4_1) (TermOp2.mk_number_number_string_dep0_term p0 p1 p2 p3 p4_2)

      let dest_number_number_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_number_number_string_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_number_number_string_dep0_term p0 p1_2 in
         (merge_num "TermOp.dest_number_number_string_dep0_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_string_dep0_term - 1" res1_1 res1_2),
         (merge_string "TermOp.dest_number_number_string_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_number_number_string_dep0_term - 3" res3_1 res3_2)

      let dest_number_number_string_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_number_number_string_dep0_any_term p0_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_number_number_string_dep0_any_term p0_2 in
         (merge_num "TermOp.dest_number_number_string_dep0_any_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_string_dep0_any_term - 1" res1_1 res1_2),
         (merge_string "TermOp.dest_number_number_string_dep0_any_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_number_number_string_dep0_any_term - 3" res3_1 res3_2)

      let is_string_string_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_string_string_dep0_dep0_term" (TermOp1.is_string_string_dep0_dep0_term p0 p1_1) (TermOp2.is_string_string_dep0_dep0_term p0 p1_2)

      let mk_string_string_dep0_dep0_term (p0 : opname) (p1 : string) (p2 : string) (p3 : term) (p4 : term) =
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge_term "TermOp.mk_string_string_dep0_dep0_term" (TermOp1.mk_string_string_dep0_dep0_term p0 p1 p2 p3_1 p4_1) (TermOp2.mk_string_string_dep0_dep0_term p0 p1 p2 p3_2 p4_2)

      let dest_string_string_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_string_string_dep0_dep0_term p0 p1_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_string_string_dep0_dep0_term p0 p1_2 in
         (merge_string "TermOp.dest_string_string_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_string_dep0_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_string_string_dep0_dep0_term - 3" res3_1 res3_2)

      let dest_string_string_dep0_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1, res2_1, res3_1 = TermOp1.dest_string_string_dep0_dep0_any_term p0_1 in
         let res0_2, res1_2, res2_2, res3_2 = TermOp2.dest_string_string_dep0_dep0_any_term p0_2 in
         (merge_string "TermOp.dest_string_string_dep0_dep0_any_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_dep0_dep0_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_string_dep0_dep0_any_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_string_string_dep0_dep0_any_term - 3" res3_1 res3_2)

      let is_number_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_number_term" (TermOp1.is_number_term p0 p1_1) (TermOp2.is_number_term p0 p1_2)

      let mk_number_term (p0 : opname) (p1 : Lm_num.num) =
         merge_term "TermOp.mk_number_term" (TermOp1.mk_number_term p0 p1) (TermOp2.mk_number_term p0 p1)

      let dest_number_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_num "TermOp.dest_number_term" (TermOp1.dest_number_term p0 p1_1) (TermOp2.dest_number_term p0 p1_2)

      let dest_number_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_num "TermOp.dest_number_any_term" (TermOp1.dest_number_any_term p0_1) (TermOp2.dest_number_any_term p0_2)

      let is_univ_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_univ_term" (TermOp1.is_univ_term p0 p1_1) (TermOp2.is_univ_term p0 p1_2)

      let mk_univ_term (p0 : opname) (p1 : level_exp) =
         let p1_1, p1_2 = p1 in
         merge_term "TermOp.mk_univ_term" (TermOp1.mk_univ_term p0 p1_1) (TermOp2.mk_univ_term p0 p1_2)

      let dest_univ_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_level_exp "TermOp.dest_univ_term" (TermOp1.dest_univ_term p0 p1_1) (TermOp2.dest_univ_term p0 p1_2)

      let is_token_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_token_term" (TermOp1.is_token_term p0 p1_1) (TermOp2.is_token_term p0 p1_2)

      let mk_token_term (p0 : opname) (p1 : string) =
         merge_term "TermOp.mk_token_term" (TermOp1.mk_token_term p0 p1) (TermOp2.mk_token_term p0 p1)

      let dest_token_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_string "TermOp.dest_token_term" (TermOp1.dest_token_term p0 p1_1) (TermOp2.dest_token_term p0 p1_2)

      let is_token_simple_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge_bool "TermOp.is_token_simple_term" (TermOp1.is_token_simple_term p0 p1_1) (TermOp2.is_token_simple_term p0 p1_2)

      let mk_token_simple_term (p0 : opname) (p1 : string) (p2 : term list) =
         let p2_1, p2_2 = split p2 in
         merge_term "TermOp.mk_token_simple_term" (TermOp1.mk_token_simple_term p0 p1 p2_1) (TermOp2.mk_token_simple_term p0 p1 p2_2)

      let dest_token_simple_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1 = TermOp1.dest_token_simple_term p0 p1_1 in
         let res0_2, res1_2 = TermOp2.dest_token_simple_term p0 p1_2 in
         (merge_string "TermOp.dest_token_simple_term - 0" res0_1 res0_2),
         (merge_terms "TermOp.dest_token_simple_term - 1" res1_1 res1_2)

      end

   module TermAddr = struct
      module AddrTypes = TermType
      type address = TermType.address

      let string_of_address (a1, a2) =
         sprintf "Impl1 addr: %s; Impl2 addr: %s" (TermAddr1.string_of_address a1) (TermAddr2.string_of_address a2)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let make_address (p0 : int list) =
         merge_address "TermAddr.make_address" (TermAddr1.make_address p0) (TermAddr2.make_address p0)

      let compose_address (p0 : address) (p1 : address) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge_address "TermAddr.compose_address" (TermAddr1.compose_address p0_1 p1_1) (TermAddr2.compose_address p0_2 p1_2)

      let split_clause_address (p0 : address) =
         let p0_1, p0_2 = p0 in
         let res0_1, res1_1 = TermAddr1.split_clause_address p0_1 in
         let res0_2, res1_2 = TermAddr2.split_clause_address p0_2 in
         (merge_address "TermAddr.split_clause_address - 0" res0_1 res0_2),
         (merge_address "TermAddr.split_clause_address - 1" res1_1 res1_2)

      let find_subterm (p0 : term) (p1 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge_address "TermAddr.find_subterm" (TermAddr1.find_subterm p0_1 p1_1) (TermAddr2.find_subterm p0_2 p1_2)

      let term_subterm (p0 : term) (p1 : address) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge_term "TermAddr.term_subterm" (TermAddr1.term_subterm p0_1 p1_1) (TermAddr2.term_subterm p0_2 p1_2)

      let replace_subterm (p0 : term) (p1 : address) (p2 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge_term "TermAddr.replace_subterm" (TermAddr1.replace_subterm p0_1 p1_1 p2_1) (TermAddr2.replace_subterm p0_2 p1_2 p2_2)

      let replace_bound_subterm (p0 : term) (p1 : address) (p2 : SymbolSet.t) (p3 : (SymbolSet.t -> term)) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = split_atf p3 in
         merge_term "TermAddr.replace_bound_subterm" (TermAddr1.replace_bound_subterm p0_1 p1_1 p2 p3_1) (TermAddr2.replace_bound_subterm p0_2 p1_2 p2 p3_2)

      let apply_fun_at_addr (p0 : (term -> term)) (p1 : address) (p2 : term) =
         let p0_1, p0_2 = split_ttf p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge_term "TermAddr.apply_fun_at_addr" (TermAddr1.apply_fun_at_addr p0_1 p1_1 p2_1) (TermAddr2.apply_fun_at_addr p0_2 p1_2 p2_2)

      let apply_fun_arg_at_addr (p0 : (term -> term * 'a)) (p1 : address) (p2 : term) =
         let p0_1, p0_2 = split_ttaf p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let res0_1, res1_1 = TermAddr1.apply_fun_arg_at_addr p0_1 p1_1 p2_1 in
         let res0_2, res1_2 = TermAddr2.apply_fun_arg_at_addr p0_2 p1_2 p2_2 in
         (merge_term "TermAddr.apply_fun_arg_at_addr - 0" res0_1 res0_2),
         (merge_poly "TermAddr.apply_fun_arg_at_addr - 1" res1_1 res1_2)

      let apply_var_fun_at_addr (p0 : (SymbolSet.t -> term -> term)) (p1 : address) (p2 : SymbolSet.t) (p3 : term) =
         let p0_1, p0_2 = split_attf p0 in
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = p3 in
         merge_term "TermAddr.apply_var_fun_at_addr" (TermAddr1.apply_var_fun_at_addr p0_1 p1_1 p2 p3_1) (TermAddr2.apply_var_fun_at_addr p0_2 p1_2 p2 p3_2)

      let apply_var_fun_arg_at_addr (p0 : (SymbolSet.t -> term -> term * 'a)) (p1 : address) (p2 : SymbolSet.t) (p3 : term) =
         let p0_1, p0_2 = split_attaf p0 in
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = p3 in
         let res0_1, res1_1 = TermAddr1.apply_var_fun_arg_at_addr p0_1 p1_1 p2 p3_1 in
         let res0_2, res1_2 = TermAddr2.apply_var_fun_arg_at_addr p0_2 p1_2 p2 p3_2 in
         (merge_term "TermAddr.apply_var_fun_arg_at_addr - 0" res0_1 res0_2),
         (merge_poly "TermAddr.apply_var_fun_arg_at_addr - 1" res1_1 res1_2)

      let subterm_addresses (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_addresss "TermAddr.subterm_addresses" (TermAddr1.subterm_addresses p0_1) (TermAddr2.subterm_addresses p0_2)

      let apply_fun_higher (p0 : (term -> term * 'a)) (p1 : term) =
         let p0_1, p0_2 = split_ttaf p0 in
         let p1_1, p1_2 = p1 in
         let res0_1, res1_1 = TermAddr1.apply_fun_higher p0_1 p1_1 in
         let res0_2, res1_2 = TermAddr2.apply_fun_higher p0_2 p1_2 in
         (merge_term "TermAddr.apply_fun_higher - 0" res0_1 res0_2),
         (merge_poly "TermAddr.apply_fun_higher - 1" res1_1 res1_2)

      let apply_var_fun_higher (p0 : (SymbolSet.t -> term -> term * 'a)) (p1 : SymbolSet.t) (p2 : term) =
         let p0_1, p0_2 = split_attaf p0 in
         let p2_1, p2_2 = p2 in
         let res0_1, res1_1 = TermAddr1.apply_var_fun_higher p0_1 p1 p2_1 in
         let res0_2, res1_2 = TermAddr2.apply_var_fun_higher p0_2 p1 p2_2 in
         (merge_term "TermAddr.apply_var_fun_higher - 0" res0_1 res0_2),
         (merge_poly "TermAddr.apply_var_fun_higher - 1" res1_1 res1_2)

      let nth_hyp_addr (p0 : term) (p1 : int) =
         let p0_1, p0_2 = p0 in
         merge_address "TermAddr.nth_hyp_addr" (TermAddr1.nth_hyp_addr p0_1 p1) (TermAddr2.nth_hyp_addr p0_2 p1)

      let concl_addr (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge_address "TermAddr.concl_addr" (TermAddr1.concl_addr p0_1) (TermAddr2.concl_addr p0_2)

      let nth_clause_addr (p0 : term) (p1 : int) =
         let p0_1, p0_2 = p0 in
         merge_address "TermAddr.nth_clause_addr" (TermAddr1.nth_clause_addr p0_1 p1) (TermAddr2.nth_clause_addr p0_2 p1)

   end

   module TermMan = struct
      module ManTypes = TermType

   end

   module TermSubst = struct
      module SubstTypes = TermType
   end

   module TermShape = struct
      include TermType
   end

   module TermMeta = struct
      module MetaTypes = TermType
   end

   module TermEval = struct
      type term = TermType.term
   end

   module RefineError = struct
      module ErrTypes = struct
         module Types = TermType
         type address = TermType.address
      end
   end

   module Rewrite = struct
      include TermType
   end

   module Refine = struct
      include TermType
   end

   module TermHash = struct
      include TermType
   end

   module TermNorm = struct
      include TermType
   end

   module TermHeaderConstr (FromTerm : TermModuleSig) = struct

   end


end

