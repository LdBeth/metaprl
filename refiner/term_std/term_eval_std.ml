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
open Term_std

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
   let is_canon_var_term = function
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
        term_terms = []
      } when opname == canon_var_opname -> true
    | _ -> false

   (*
    * Destructor for a "canon_var".
    *)
   let dest_canon_var = function
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
        term_terms = []
      } when opname == canon_var_opname -> v
     | t -> raise (TermMatch ("dest_canon_var", t, ""))

   (*
    * Make a "canon_var".
    *)
   let mk_canon_var_term v =
      { term_op = { imp_op_name = canon_var_opname; imp_op_params = [Var v] };
        term_terms = []
      }

   let subst_opname = mk_opname "subst" xperv

   (*
    * See if a term is a "subst" term.
    *)

   let rec is_subst_term_args = function
      [],[] -> true
    | var::vars,{bvars = []}::bterms -> is_subst_term_args (vars,bterms)
    | _ -> false

   let is_subst_term = function
      { term_op = { imp_op_name = opname; imp_op_params = [] };
        term_terms = {bvars = vars; bterm = main_term}::bterms
      } when opname == subst_opname -> is_subst_term_args (vars,bterms)
    | _ -> false

   (*
    * Destructor for a "subst" term.
    *)

   let rec dest_subst_args t = function
      [],[] -> [],[]
    | var::vars,{bvars = []; bterm = term}::bterms ->
         let tail = dest_subst_args t (vars,bterms) in
         (var::fst tail),(term::snd tail)
    | _ -> raise (TermMatch ("dest_subst", t, ""))

   let dest_subst = function
      { term_op = { imp_op_name = opname; imp_op_params = [] };
        term_terms = {bvars = vars; bterm = main_term}::bterms
      } as t when opname == subst_opname -> (main_term, dest_subst_args t (vars,bterms))
     | t -> raise (TermMatch ("dest_subst", t, ""))

   (*
    * Make a "subst" term.
    *)

   let mk_subst_term main_term subst =
      let sub = List.split subst in
      let vars = fst sub in
      let terms = List.map (function term -> {bvars=[]; bterm = term}) (snd sub) in
      { term_op = { imp_op_name = subst_opname; imp_op_params = [] };
        term_terms = {bvars=vars; bterm=main_term}::terms
      }

   let make_subst_term main_term vars trms =
      if List.length vars != List.length trms then raise (Invalid_argument "make_subst_term") else
      let terms = List.map (function term -> {bvars=[]; bterm = term}) trms in
      { term_op = { imp_op_name = subst_opname; imp_op_params = [] };
        term_terms = {bvars=vars; bterm=main_term}::terms
      }

   let make_1subst_term main_term v t =
      { term_op = { imp_op_name = subst_opname; imp_op_params = [] };
        term_terms = [ {bvars=[v]; bterm=main_term};{bvars=[]; bterm=t} ]
      }

   let make_2subst_term main_term v1 v2 t1 t2 =
      { term_op = { imp_op_name = subst_opname; imp_op_params = [] };
        term_terms = [ {bvars=[v1;v2]; bterm=main_term};{bvars=[]; bterm=t1};{bvars=[]; bterm=t2} ]
      }
end

(*
 * $Log$
 * Revision 1.3  1998/06/15 21:57:18  jyh
 * Added a few new functions.
 *
 * Revision 1.2  1998/05/30 19:18:47  nogin
 * Eliminated white space in empty lines.
 *
 * Revision 1.1  1998/05/28 15:02:25  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:26  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
