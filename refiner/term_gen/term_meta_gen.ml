(*
 * Meta terms include implications, etc.
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
 * Copyright (C) 1998-2004 MetaPRL Group
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

INCLUDE "refine_error.mlh"

open Refine_error_sig
open Term_sig
open Term_base_sig
open Term_man_sig
open Term_subst_sig

open Lm_symbol

module TermMeta (**)
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (RefineError : RefineErrorSig with module ErrTypes.Types = TermType) =
struct
   open TermType
   open Term
   open TermMan
   open TermSubst
   open RefineError

   module MetaTypes = TermType

   (************************************************************************
    * META-TERMS                                                           *
    ************************************************************************)

   (*
    * Unzip a metaimplication into a list of terms.
    *)
   let rec unfold_mlabeled name = function
      MetaLabeled (_, t) ->
         unfold_mlabeled name t
    | MetaTheorem a ->
         a
    | t ->
         REF_RAISE(RefineError (name, MetaTermMatchError t))

   let rec unzip_mlabeled name labels = function
      MetaLabeled (l, t) ->
         unzip_mlabeled name (l :: labels) t
    | MetaTheorem a ->
         List.rev labels, a
    | t ->
         REF_RAISE(RefineError (name, MetaTermMatchError t))

   let rec unzip_mimplies = function
      MetaTheorem t ->
         [], t
    | MetaImplies (a, t) ->
         let hyps, goal = unzip_mimplies t in
            unfold_mlabeled "unzip_mimplies" a :: hyps, goal
    | MetaLabeled (_, t) ->
         unzip_mimplies t
    | MetaIff _ as t ->
         REF_RAISE(RefineError ("unzip_mimplies got a rewrite where a rule was expected", MetaTermMatchError t))
    | MetaFunction _ ->
         raise (Invalid_argument "Term_meta_gen.unzip_mimplies: internal error: function was called w/o strip_mfunction applied first")

   let rec zip_mimplies assums goal =
      match assums with
         [] -> MetaTheorem goal
       | h::t -> MetaImplies (MetaTheorem h, zip_mimplies t goal)

   (*
    * Implication with bindings.
    *)
   let rec strip_mfunction = function
      (MetaTheorem _) as t ->
         t
    | MetaImplies (a, t)
    | MetaFunction (_, a, t) ->
         MetaImplies (a, strip_mfunction t)
    | MetaIff (t1, t2) ->
         MetaIff (strip_mfunction t1, strip_mfunction t2)
    | MetaLabeled (_, (MetaTheorem _ | MetaIff _)) as t ->
         raise(RefineError("Term_meta_gen.strip_mfunction",StringWrapError("Labels on rule and rewrite statements are not allowed",MetaTermMatchError t)))
    | MetaLabeled (l, t) ->
         MetaLabeled (l, strip_mfunction t)

   let unzip_mfunction t =
      let rec collect l = function
         MetaImplies (a, t) ->
            let labels, a = unzip_mlabeled "unzip_mfunction" [] a in
               collect ((labels, None, a) :: l) t
       | MetaFunction (v, a, t) ->
            let labels, a = unzip_mlabeled "unzip_mfunction" [] a in
               collect ((labels, Some v, a) :: l) t
       | MetaTheorem a ->
            List.rev l, a
       | t ->
            REF_RAISE(RefineError ("unzip_mfunction", MetaTermMatchError t))
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
    | MetaImplies (MetaTheorem a, t) ->
         let l, redex, contractum = unzip_mrewrite t in
            a::l, redex, contractum
    | MetaLabeled (_, t) ->
         unzip_mrewrite t
    | t ->
         REF_RAISE(RefineError ("unzip_mrewrite", MetaTermMatchError t))

   (*
    * Calculate context vars.
    *)
   let rec context_vars = function
      MetaTheorem t ->
         TermSubst.context_vars t
    | MetaImplies (a, b)
    | MetaFunction (_, a, b)
    | MetaIff (a, b) ->
         Lm_list_util.union (context_vars a) (context_vars b)
    | MetaLabeled (_, t) ->
         context_vars t

   (*
    * Alpha equality.
    *)
   let rec meta_alpha_equal t1 t2 =
      match t1, t2 with
         MetaTheorem a1, MetaTheorem a2 ->
            TermSubst.alpha_equal a1 a2
       | MetaImplies (a1, b1), MetaImplies (a2, b2)
       | MetaFunction (_, a1, b1), MetaFunction (_, a2, b2)
       | MetaIff (a1, b1), MetaIff (a2, b2) ->
            meta_alpha_equal a1 a2 & meta_alpha_equal b1 b2
       | MetaLabeled (_, t), _ ->
            meta_alpha_equal t t2
       | _, MetaLabeled (_, t) ->
            meta_alpha_equal t1 t
       | _ ->
            false

   (************************************************************************
    * BOUND CONTEXTS "MAGIC"                                               *
    ************************************************************************)

   let vv = Lm_symbol.make "v" 0
   let bang = [Lm_symbol.make "!" 0]

   let encode_free_var v = mk_so_var_term v bang []

   let is_encoded_free_var t =
      is_so_var_term t && let v,conts,terms = dest_so_var t in conts = bang && terms = []

   let decode_free_var t =
      let v,conts,terms = dest_so_var t in
         if conts = bang && terms = [] then v else
            raise (RefineError ("decode_free_var", StringTermError ("not an encoded free var", t)))

   (*
    * Rename hypothesis bound variables --- use empty names for variables not actually used
    *)
   let rec rename_hyp_bvars parsing bvars goals seqfvars count sub = function
      [] -> [], Lm_list_util.smap (apply_subst sub) goals
    | (Context(c, conts, terms) as hd :: tl) as hyps ->
         let tl', goals' = rename_hyp_bvars parsing (SymbolSet.add bvars c) goals seqfvars count sub tl in
         let hd' = if sub = [] || terms = [] then hd else Context(c, conts, Lm_list_util.smap (apply_subst sub) terms) in
         (if hd' == hd && tl' == tl then hyps else hd'::tl'), goals'
    | (Hypothesis (v,t) as hd :: tl) as hyps ->
         let mem = SymbolSet.mem seqfvars v in
         let empt = (Lm_symbol.to_string v = "") in
         let v' =
            if mem && empt then Lm_symbol.new_name vv (SymbolSet.mem (SymbolSet.union bvars seqfvars))
            else if (not mem) && ((not (parsing || empt)) || SymbolSet.mem bvars v) then begin
               incr count;
               Lm_symbol.new_name (Lm_symbol.make "" !count) (SymbolSet.mem (SymbolSet.union bvars seqfvars))
            end else v
         in
         let t' = apply_subst sub t in
         let sub = if v == v' then sub else (v, mk_var_term v') :: sub in
         let tl', goals' = rename_hyp_bvars parsing (SymbolSet.add bvars v') goals seqfvars count sub tl in
         let hd' = if v==v' && t==t' then hd else Hypothesis (v',t') in
         (if hd' == hd && tl' == tl then hyps else hd'::tl'), goals'

   (*
    * Go down the term, mapping the context bindings
    * f knows how to map contexts; f' knows what to do with free FO variables; f'' handles sequents.
    *)
   let convert_contexts f f' parsing =
      let rec convert_term bvars fvars bconts t =
         if is_var_term t then
            let v = dest_var t in if SymbolSet.mem fvars v then f' t v bconts else t
         else if is_so_var_term t then
            let v,conts,terms = dest_so_var t in
            if parsing && terms = [] && conts = bang then
               if SymbolSet.mem bvars v then let s = Lm_symbol.string_of_symbol v in
                  raise (Invalid_argument("Parsing: !" ^ s ^ " is used, but the variable " ^ s ^ " is bound"))
               else
                  mk_var_term v
            else if terms = [] && SymbolSet.mem bvars v then
               mk_var_term v
            else
               let conts' = f bconts v conts in
               let terms' = Lm_list_util.smap (convert_term bvars fvars bconts) terms in
               if conts' == conts && terms == terms' then t else mk_so_var_term v conts' terms'
          else if is_context_term t then
            let v,ct,conts,terms = dest_context t in
            let ct' = convert_term bvars fvars (v::bconts) ct in
            let conts' = f bconts v conts in
            let terms' = Lm_list_util.smap (convert_term bvars fvars bconts) terms in
            if ct == ct && conts' == conts && terms == terms' then t else mk_context_term v ct' conts' terms'
          else if is_sequent_term t then
            let eseq = explode_sequent t in
            let arg' = convert_term bvars fvars bconts eseq.sequent_args in
            let hyps = SeqHyp.to_list eseq.sequent_hyps in
            let goals = SeqGoal.to_list eseq.sequent_goals in
            let hyps', goals', seqfvars = convert_hyps bvars fvars bconts goals hyps in
            let hyps', goals' = rename_hyp_bvars parsing (SymbolSet.add_list bvars bconts) goals' seqfvars (ref (-1)) [] hyps' in
            if arg' == eseq.sequent_args && hyps' == hyps && goals' == goals then t
            else mk_sequent_term {
               sequent_args = arg'; sequent_hyps = SeqHyp.of_list hyps'; sequent_goals = SeqGoal.of_list goals'
            }
         else
            let t' = dest_term t in
            let bts' = Lm_list_util.smap (convert_bterm bvars fvars bconts) t'.term_terms in
            if bts' == t'.term_terms then t else mk_term t'.term_op bts'

      and convert_bterm bvars fvars bconts bt =
         let bt' = dest_bterm bt in
         let t' = convert_term (SymbolSet.add_list bvars bt'.bvars)(SymbolSet.subtract_list fvars bt'.bvars) bconts bt'.bterm in
         if t' == bt'.bterm then bt else mk_bterm bt'.bvars t'

      and convert_hyps bvars fvars bconts goals = function
         [] ->
            let goals = Lm_list_util.smap (convert_term bvars fvars bconts) goals in
               [], goals, free_vars_terms goals
       | ((Hypothesis (v, t)) as hd :: tl) as hyps ->
            let t' = convert_term bvars fvars bconts t in
            let tl', goals', seqfvars = convert_hyps (SymbolSet.add bvars v) (SymbolSet.remove fvars v) bconts goals tl in
            (if t'==t && tl' == tl then hyps else
            let hd' = if t' == t then hd else Hypothesis(v, t') in
               hd' :: tl'), goals', SymbolSet.union seqfvars (free_vars_set t')
      | (Context (c, conts, terms) as hd :: tl) as hyps ->
            let conts' = f bconts c conts in
            let terms' = Lm_list_util.smap (convert_term bvars fvars bconts) terms in
            let tl', goals', seqfvars = convert_hyps bvars fvars (c::bconts) goals tl in
            let hd' = if conts' == conts && terms == terms' then hd else Context (c, conts', terms') in
            (if hd' == hd && tl' == tl then hyps else hd'::tl'), goals', SymbolSet.union seqfvars (free_vars_terms terms')

      in
         fun t -> convert_term SymbolSet.empty (free_vars_set t) [] t

   let rec convert_mterm f = function
      (MetaTheorem t) as mt ->
         let t' = f t in if t' == t then mt else MetaTheorem t'
    | (MetaImplies(mt1, mt2)) as mt ->
         let mt2' = convert_mterm f mt2 in let mt1' = convert_mterm f mt1 in
            if mt1' == mt1 && mt2' == mt2 then mt else MetaImplies(mt1',mt2')
    | (MetaFunction(t,mt1, mt2)) as mt ->
         let mt2' = convert_mterm f mt2 in let mt1' = convert_mterm f mt1 in let t' = f t in
            if t' == t && mt1' == mt1 && mt2' == mt2 then mt else MetaFunction(t',mt1',mt2')
    | (MetaIff(mt1, mt2)) as mt ->
         let mt1' = convert_mterm f mt1 in let mt2' = convert_mterm f mt2 in
            if mt1' == mt1 && mt2' == mt2 then mt else MetaIff(mt1',mt2')
    | (MetaLabeled(l,mt1)) as mt ->
         let mt1' = convert_mterm f mt1 in if mt1' == mt1 then mt else MetaLabeled(l,mt1')

   (* Diring parsing and display, the default contexts are "encoded" as a singleton list containing just the variable itself *)
   let context_of_parsed_contexts bconts v = function
      [v'] when Lm_symbol.eq v v' -> bconts
    | conts -> conts

   let display_context_of_contexts bconts v conts =
      if bconts = conts then [v] else conts

   (* Actual term convertors *)
   let term_of_parsed_term =
      convert_contexts context_of_parsed_contexts (fun _ v conts -> mk_so_var_term v conts []) true

   let term_of_parsed_term_with_vars =
      convert_contexts context_of_parsed_contexts (fun t _ _ -> t) true

   let display_term_of_term =
      convert_contexts display_context_of_contexts (fun _ v _ -> encode_free_var v) false

   (* create a fresh term convertor with memoization *)
   let create_term_parser () =
      let map = ref SymbolTable.empty in
      let context_of_parsed_contexts bconts v = function
         [v'] when Lm_symbol.eq v v' ->
            if SymbolTable.mem !map v then SymbolTable.find !map v else begin
               map:=SymbolTable.add !map v bconts;
               bconts
            end
       | conts ->
            if not (SymbolTable.mem !map v) then
               map:=SymbolTable.add !map v conts;
            conts
      and deal_with_a_var _ v conts =
         let conts =
            if SymbolTable.mem !map v then SymbolTable.find !map v else begin
               map:=SymbolTable.add !map v conts;
               conts
            end
         in mk_so_var_term v conts []
      in
         convert_contexts context_of_parsed_contexts deal_with_a_var true

   let mterm_of_parsed_mterm mt =
      convert_mterm (create_term_parser ()) mt

   let mterms_of_parsed_mterms mt ts =
      let parse = create_term_parser () in
      let mt = convert_mterm parse mt in (* The order is important here! *)
         mt, List.map parse ts, parse

   let context_subst_of_terms ts =
      let map = ref SymbolTable.empty in
      let rec scan_term t =
         if not (is_var_term t) then begin
            if is_so_var_term t then
               let v, conts, ts = dest_so_var t in upd_map v conts ts
            else if is_sequent_term t then begin
               let eseq = explode_sequent t in
                  scan_term eseq.sequent_args;
                  SeqHyp.iter scan_hyp eseq.sequent_hyps;
                  SeqGoal.iter scan_term eseq.sequent_goals
            end else
               List.iter scan_bterm (dest_term t).term_terms
         end
      and scan_bterm bt = scan_term (dest_bterm bt).bterm
      and scan_hyp = function
         Hypothesis (_, t) -> scan_term t
       | Context (v, conts, ts) -> upd_map v conts ts
      and upd_map v conts ts =
         if SymbolTable.mem !map v then begin
            let i, conts' = SymbolTable.find !map v in
               if i <> (List.length ts) || conts' <> conts then
                  raise (Invalid_argument "context_subst_of_terms: mismatch")
         end else
            map:=SymbolTable.add !map v (List.length ts, conts);
         List.iter scan_term ts
      in
      let _ = List.iter scan_term ts in
      let map = ! map in
         fun v i ->
            if SymbolTable.mem map v then
               let i', conts = SymbolTable.find map v in
                  if i <> i' then
                     raise(Failure "Variable arity mismatch");
                  Some conts
            else None
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
