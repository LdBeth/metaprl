(*
 * Alexey's ``special'' terms to be used in reduction rules
 *
 * "canon_var" plays the same role as "var" in reduction rules but
 * the correspondig subterm should be evaluated before the reduction
 * ("call by value" instead of "call by name")
 *
 * subst (v1,v2,v3,...,vm.T;t1;t2;t3;...;tn)
 * it is an error if m!=n
 * if n=m then subst(...) is T with v1 substituted to t2, v2 - to t2, etc.
 *
 *)

open Opname
open Term_ds

module TermEval =
struct
   open Term

   type term = Term.term

   (*
    * Manifest terms are injected into the "perv" module.
    *)
   let xperv = make_opname ["Perv"]

   let canon_var_opname = mk_opname "canon_var" xperv

   (*
    * See if a term is a "canon_var".
    *)
   let is_canon_var_term t = match dest_term t with
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = []
      } when opname == canon_var_opname -> true
    | _ -> false

   (*
    * Destructor for a "canon_var".
    *)
   let dest_canon_var t = match dest_term t with
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = []
      } when opname == canon_var_opname -> v
     | _ -> raise (TermMatch ("dest_canon_var", t, ""))

   (*
    * Make a "canon_var".
    *)
   let mk_canon_var_term v =
      { free_vars = StringSet.add v StringSet.empty;
        core = Term
         { term_op = { op_name = canon_var_opname; op_params = [Var v] };
           term_terms = []}}

   let subst_opname = mk_opname "subst" xperv

   (*
    * See if a term is a "subst" term.
    *)

   let rec is_subst_term_args = function
      [],[] -> true
    | var::vars,bt::bterms ->
         (dest_bterm bt).bvars = [] && is_subst_term_args (vars,bterms)
    | _ -> false

   let is_subst_term t = match dest_term t with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = bt::bterms
      } when opname == subst_opname ->
         is_subst_term_args ((dest_bterm bt).bvars,bterms)
    | _ -> false

   (*
    * Destructor for a "subst" term.
    *)

   let rec dest_subst_args t = function
      [],[] -> [],[]
    | var::vars,bt::bterms ->
         let tail = dest_subst_args t (vars,bterms) in
         (var::fst tail),((dest_simple_bterm t bt)::snd tail)
    | _ -> raise (TermMatch ("dest_subst", t, ""))

   let dest_subst t = match dest_term t with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = btrm::bterms
      } when opname == subst_opname ->
         let bt = dest_bterm btrm in
         (bt.bterm, dest_subst_args t (bt.bvars,bterms))
     | _ -> raise (TermMatch ("dest_subst", t, ""))

   (*
    * Make a "subst" term.
    *)

   let mk_subst_term main_term subst =
      let sub = List.split subst in
      let vars = fst sub in
      let terms = List.map mk_simple_bterm (snd sub) in
      mk_term
         { op_name = subst_opname; op_params = [] }
         (mk_bterm vars main_term::terms)

   let make_subst_term main_term vars trms =
      if List.length vars != List.length trms then raise (Invalid_argument "make_subst_term") else
      let terms = List.map mk_simple_bterm trms in
      mk_term
         { op_name = subst_opname; op_params = [] }
         (mk_bterm vars main_term::terms)

   let make_1subst_term main_term v t =
      let fvm = StringSet.remove v main_term.free_vars in
      { free_vars = StringSet.union fvm t.free_vars;
        core = Term
         { term_op = { op_name = subst_opname; op_params = [] };
           term_terms =
            [ { bfree_vars = fvm;
                bcore = BTerm {bvars=[v]; bterm=main_term}};
              { bfree_vars = t.free_vars;
                bcore = BTerm {bvars=[]; bterm=t}}]}}

   let make_2subst_term main_term v1 v2 t1 t2 =
      let fvm = StringSet.remove v1 (StringSet.remove v2 main_term.free_vars) in
      { free_vars = StringSet.union fvm (StringSet.union t1.free_vars t2.free_vars);
        core = Term
         { term_op = { op_name = subst_opname; op_params = [] };
           term_terms =
            [ { bfree_vars = fvm;
                bcore = BTerm {bvars=[v1;v2]; bterm=main_term}};
              { bfree_vars = t1.free_vars;
                bcore = BTerm {bvars=[]; bterm=t1}};
              { bfree_vars = t2.free_vars;
                bcore = BTerm {bvars=[]; bterm=t2}}]}}

end
