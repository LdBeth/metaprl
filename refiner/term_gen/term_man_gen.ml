(*
 * Manifest terms.
 *)

open Opname
open Term_simple_sig
open Term_op_sig
open Term_subst_sig

module TermMan (**)
   (Term : TermSimpleSig)
   (TermOp : TermOpSig
    with type term = Term.term)
   (TermSubst : TermSubstSig
    with type term = Term.term
    with type param = Term.param) =
struct
   open Term
   open TermOp
   open TermSubst

   type term = Term.term
   type operator = Term.operator
   type level_exp = Term.level_exp

   (*
    * Manifest terms are injected into the "perv" module.
    *)
   let xperv = make_opname ["Perv"]

   (************************************************************************
    * Level expressions                                                    *
    ************************************************************************)
   
   (* Simplified level expression constructors *)
   let mk_const_level_exp i =
      { le_const = i; le_vars = [] }

   let mk_var_level_exp v =
      { le_const = 0; le_vars = [{ le_var = v; le_offset = 0 }] }

   (*
    * Increment a level exp
    *)
   let incr_level_exp = function
      ({ le_const = c; le_vars = vars } : level_exp) ->
         let add1 = function
            { le_var = v; le_offset = o } ->
               { le_var = v; le_offset = o + 1 }
         in
            { le_const = c + 1; le_vars = List.map add1 vars }

   (*
    * Build a level expression out of the max of two level
    * expressions.
    *)
   let max_level_exp = fun
      ({ le_const = c1; le_vars = l1 } : level_exp)
      ({ le_const = c2; le_vars = l2 } : level_exp) ->
         (* Max of two expressions; sort the variables *)
         let rec join = function
            ({ le_var = v1; le_offset = o1 }::t1 as l1),
            ({ le_var = v2; le_offset = o2 }::t2 as l2) ->
               if v1 = v2 then
                  { le_var = v1; le_offset = max o1 o2 }::(join (t1, t2))
               else if v1 < v2 then
                  { le_var = v1; le_offset = o1 }::(join (t1, l2))
               else
                  { le_var = v2; le_offset = o2 }::(join (l1, t2))
          | [], l2 -> l2
          | l1, [] -> l1
         in
            { le_const = max c1 c2; le_vars = join (l1, l2) }

   (*
    * See if the first level is contained in the second.
    *)
   let level_cumulativity = fun
      { le_const = const1; le_vars = vars1 }
      { le_const = const2; le_vars = vars2 } ->
         let rec caux = function
            ({ le_var = v1; le_offset = o1 }::t1 as l1),
            { le_var = v2; le_offset = o2 }::t2 ->
               if v1 = v2 then
                  if o1 <= o2 then
                     caux (t1, t2)
                  else
                     false
               else if v1 < v2 then
                  caux (l1, t2)
               else
                  false
          | [], _ -> true
          | _, [] -> false
         in
            if const1 <= const2 then
               caux (vars1, vars2)
            else
               false

   (************************************************************************
    * PRIMITIVE FORMS                                                      *
    ************************************************************************)

   let xperv = make_opname ["Perv"]

   (*
    * Lists.
    *)
   let xnil_opname = mk_opname "nil" xperv
   let xcons_opname = mk_opname "cons" xperv

   let xnil_term = mk_simple_term xnil_opname []
   let is_xnil_term t = t = xnil_term

   let is_xcons_term = is_dep0_dep0_term xcons_opname
   let mk_xcons_term = mk_dep0_dep0_term xcons_opname
   let dest_xcons = dest_dep0_dep0_term xcons_opname

   let rec is_xlist_term t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [bt1; bt2]
         } when opname == xcons_opname ->
            begin
               match (dest_bterm bt1, dest_bterm bt2) with
                  ({ bvars = []; bterm = _ }, { bvars = []; bterm = b }) ->
                     is_xlist_term b
                | _ -> false
            end
       | { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when opname == xnil_opname
      -> true
       | _ -> false

   let dest_xlist t =
      let rec aux trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == xcons_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = []; bterm = a },
                      { bvars = []; bterm = b }) -> a::(aux b)
                   | _ -> raise (TermMatch ("dest_xlist", t, "not a list"))
               end
          | { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when opname == xnil_opname -> []
          | _ -> raise (TermMatch ("dest_xlist", t, "not a list"))
      in
         aux t

   let rec mk_xlist_term = function
      h::t ->
         mk_term
         { op_name = xcons_opname; op_params = [] }
         [mk_simple_bterm h ; mk_simple_bterm (mk_xlist_term t)]
    | [] ->
         xnil_term

   (*
    * Strings.
    *)
   let string_opname = mk_opname "string" xperv

   let is_xstring_term t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [String _] };
           term_terms = []
         } when opname == string_opname ->
            true
       | _ ->
            false

   let dest_xstring t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = []
         } when opname == string_opname ->
            s
       | _ ->
            raise (TermMatch ("dest_xstring", t, "not a string"))

   let mk_xstring_term s =
      let op = { op_name = string_opname; op_params = [String s] } in
         mk_term op []

   (****************************************
    * LAMBDA                               *
    ****************************************)

   let xlambda_opname = mk_opname "lambda" xperv

   let mk_xlambda_term = mk_dep1_term xlambda_opname

   (*************************
    * Sequents              *                                              *
    *************************)

   (* Sequents operator name *)
   let hyp_opname = mk_opname "hyp" xperv
   let concl_opname = mk_opname "concl" xperv
   let sequent_opname = mk_opname "sequent" xperv

   (* Dependent hypotheses *)
   let is_hyp_term = is_dep0_dep1_term hyp_opname
   let mk_hyp_term = mk_dep0_dep1_term hyp_opname
   let dest_hyp = dest_dep0_dep1_term hyp_opname

   (* Conclusions *)
   let is_concl_term = is_dep0_dep0_term concl_opname
   let mk_concl_term = mk_dep0_dep0_term concl_opname
   let dest_concl = dest_dep0_dep0_term concl_opname

   (* Sequent wrapper *)
   let is_sequent_term = is_simple_term_opname sequent_opname
   let mk_sequent_term = mk_simple_term sequent_opname
   let dest_sequent = dest_simple_term_opname sequent_opname
   let goal_of_sequent t =
      match dest_term t with
         { term_op = { op_name = name; op_params = [] };
           term_terms = bt::_
         } when name == sequent_opname ->
            dest_simple_bterm t bt
       | _ -> raise (TermMatch ("goal_of_sequent", t, ""))

   let null_concl = mk_simple_term concl_opname []

   (*
    * Find the address of the conclusion.
    *)
   let concl_addr t =
      let rec aux' i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == concl_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = [] }, { bvars = []; bterm = term }) -> aux' (i + 1) term
                   | _ -> i
               end
          | _ ->
               i
      in
      let rec aux i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == hyp_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = [] }, { bvars = [_]; bterm = term }) -> aux (i + 1) term
                   | ({ bvars = [] }, { bvars = []; bterm = term }) -> aux (i + 1) term
                   | _ -> raise (TermMatch ("concl_addr", t, ""))
               end
          | { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == concl_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = [] }, { bvars = []; bterm = term }) -> (i, aux' 0 term)
                   | _ -> raise (TermMatch ("concl_addr", t, ""))
               end
          | _ -> raise (TermMatch ("concl_addr", t, ""))
      in
         aux 0 (goal_of_sequent t)

   (*
    * Fast access to hyp and concl.
    *)
   let nth_hyp t i =
      let rec aux i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == hyp_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = []; bterm = t }, { bvars = [x]; bterm = term }) ->
                        if i = 0 then
                           x, t
                        else
                           aux (i - 1) term
                   | _ -> raise (TermMatch ("nth_hyp", t, ""))
               end
          | { term_op = { op_name = opname } } when opname == concl_opname ->
               raise Not_found
          | _ -> raise (TermMatch ("nth_hyp", t, ""))
      in
         aux i (goal_of_sequent t)

   let nth_concl t i =
      let rec aux i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == hyp_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = []; bterm = _ }, { bvars = [_]; bterm = term }) ->
                        aux i term
                   | _ -> raise (TermMatch ("nth_concl", t, ""))
               end
          | { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == concl_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = []; bterm = t }, { bvars = []; bterm = term }) ->
                        if i = 0 then
                           t
                        else
                           aux (i - 1) term
                   | _ -> raise Not_found
               end
          | { term_op = { op_name = opname } } when opname == concl_opname ->
               raise Not_found
          | _ -> raise (TermMatch ("nth_concl", t, ""))
      in
         aux i (goal_of_sequent t)

   (*
    * Count the hyps.
    *)
   let num_hyps t =
      let rec aux i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == hyp_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = []; bterm = t }, { bvars = [x]; bterm = term }) ->
                        aux (i + 1) term
                   | _ -> i
               end
          | _ -> i
      in
         aux 0 (goal_of_sequent t)

   (*
    * Collect the vars.
    *)
   let declared_vars t =
      let rec aux vars trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == hyp_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = []; bterm = t }, { bvars = [x]; bterm = term }) ->
                        aux (x::vars) term
                   | _ -> vars
               end
          | _ -> vars
      in
         aux [] (goal_of_sequent t)

   (*
    * Collect the vars.
    *)
   let declarations t =
      let rec aux vars trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == hyp_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = []; bterm = t }, { bvars = [x]; bterm = term }) ->
                        aux ((x, t)::vars) term
                   | _ -> vars
               end
          | _ -> vars
      in
         aux [] (goal_of_sequent t)

   (*
    * Get the number of the hyp with the given var.
    *)
   let get_decl_number t v =
      let rec aux i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bt1; bt2]
            } when opname == hyp_opname ->
               begin
                  match (dest_bterm bt1, dest_bterm bt2) with
                     ({ bvars = []; bterm = t }, { bvars = [x]; bterm = term }) ->
                        if x = v then
                           i
                        else
                           aux (i + 1) term
                   | _ -> raise Not_found
               end
          | _ -> raise Not_found
      in
         aux 0 (goal_of_sequent t)

   (*
    * See if a var is free in the rest of the sequent.
    *)
   let is_free_seq_var i v t =
      let rec aux i t =
         if i = 0 then
            is_free_var v t
         else
            match dest_term t with
               { term_op = { op_name = opname; op_params = [] };
                 term_terms = [bt1; bt2]
               } when opname == hyp_opname ->
                  begin
                     match (dest_bterm bt1, dest_bterm bt2) with
                        ({ bvars = []; bterm = _ }, { bvars = [_]; bterm = term }) ->
                           aux (i - 1) term
                      | _ -> raise (Invalid_argument "is_free_seq_var")
                  end
             | _ ->
                  raise (Invalid_argument "is_free_seq_var")
      in
         aux i (goal_of_sequent t)

   (*
    * Generate a list of sequents with replaced goals.
    *)
   let rec replace_concl seq goal =
      match dest_term seq with
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [bt1; bt2]
         } when opname == hyp_opname ->
            begin
               match (dest_bterm bt1, dest_bterm bt2) with
                  ({ bvars = []; bterm = t1 }, { bvars = v1; bterm = t2 }) ->
                     mk_term
                     { op_name = hyp_opname; op_params = [] }
                     [ mk_simple_bterm t1;
                       mk_bterm v1 (replace_concl t2 goal) ]
                | _ -> goal
            end
       | _ ->
            goal

   let replace_goal seq goal =
      replace_concl (mk_concl_term goal null_concl) seq

   (*
    * Rewrite
    *)
   let xrewrite_op = mk_opname "rewrite" xperv

   let is_xrewrite_term = is_dep0_dep0_term xrewrite_op
   let mk_xrewrite_term = mk_dep0_dep0_term xrewrite_op
   let dest_xrewrite = dest_dep0_dep0_term xrewrite_op

   (************************************************************************
    * Rewrite rules                                                        *
    ************************************************************************)

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

end
