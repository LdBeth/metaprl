(*
 * This is the module that implements delayed substitution,
 * keeps track of free variables and does some sharing.
 *)

open Printf
open Debug
open Opname

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading TermDs%t" eflush

module Term =
struct

   (************************************************************************
    * Sets of strings                                                      *
    ************************************************************************)

   module OrderedString =
      struct
         type t = string
         let compare = Pervasives.compare
       end

   module StringSet = Splay_set.Make (OrderedString)

   (************************************************************************
    * Type definitions                                                     *
    ************************************************************************)

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   type level_exp_var' = { le_var : string; le_offset : int }
   type level_exp_var = level_exp_var'

   type level_exp' = { le_const : int; le_vars : level_exp_var list }
   type level_exp = level_exp'

   (*
    * Parameters have a number of simple types.
    *)
   type param' =
      Number of Num.num
    | String of string
    | Token of string
    | Level of level_exp
    | Var of string
    | MNumber of string
    | MString of string
    | MToken of string
    | MLevel of string
    | MVar of string

      (* Special Nuprl5 values *)
    | ObId of object_id
    | ParamList of param list

      (* Num operations *)
    | MSum of param * param
    | MDiff of param * param
    | MProduct of param * param
    | MQuotient of param * param
    | MRem of param * param
    | MLessThan of param * param

      (* Comparisons *)
    | MEqual of param * param
    | MNotEqual of param * param

   and object_id = param list
   and param = param'

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   type operator' = { op_name : opname; op_params : param list }
   type operator = { mutable imp_op_name : opname; imp_op_params : param list }

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *
    * free_vars - set of the free variables
    *
    * Subst - delayed substitution
    *)

   type term_subst = (string * term) list
   and term_core =
      Term of term'
    | Subst of term * term_subst
   and term = { free_vars : StringSet.t; mutable core : term_core }
   and bound_term_core =
      BTerm of bound_term'
    | BSubst of bound_term * term_subst
   and bound_term = { bfree_vars : StringSet.t; mutable bcore: bound_term_core }
   and term' = { term_op : operator; term_terms : bound_term list }
   and bound_term' = { bvars : string list; bterm : term }

   exception BadMatch of term * term
   exception TermMatch of string * term * string

   (************************************************************************
    * Free variables, substitution                                         *
    ************************************************************************)

   let rec bterms_free_vars = function
      [] -> StringSet.empty
    | [bt] -> bt.bfree_vars
    | bt::tl -> StringSet.union (bterms_free_vars tl) bt.bfree_vars

   let rec subst_free_vars = function
      [] -> StringSet.empty
    | [(v,t)] -> t.free_vars
    | (v,t)::tl -> StringSet.union (subst_free_vars tl) t.free_vars

   let do_term_subst sub t =
      match List_util.filter (fun (v,_) -> StringSet.mem v t.free_vars) sub with
         [] -> t
       | sub' ->
            {free_vars =
               StringSet.union
                  (List.fold_right
                     StringSet.remove
                     (List_util.fst_split sub')
                     t.free_vars)
                  (subst_free_vars sub');
             core = Subst (t,sub')}

   let do_bterm_subst sub bt =
      match List_util.filter (fun (v,_) -> StringSet.mem v bt.bfree_vars) sub with
         [] -> bt
       | sub' ->
            {bfree_vars =
               StringSet.union
                  (List.fold_right
                     StringSet.remove
                     (List_util.fst_split sub')
                  bt.bfree_vars)
                     (subst_free_vars sub');
             bcore = BSubst (bt,sub')}

   (************************************************************************
    * Variables in pure terms                                              *
    ************************************************************************)

   exception Not_var

   let var_opname = make_opname ["var"]

   (*
    * See if a term is a variable.
    *)
   let is_var_term_nods = function
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
        term_terms = []
      } when opname == var_opname -> true
    | _ -> false

   (*
    * Destructor for a variable.
    *)
   let dest_var_nods = function
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
        term_terms = []
      } when opname == var_opname -> v
     | _ -> raise Not_var

   (************************************************************************
    * De/Constructors                                                 *
    ************************************************************************)

   let rec dest_term t =
      match t.core with
         Term tt -> tt
       | Subst (tt,sub) ->
            let ttt = dest_term tt in
            let t4 =
               try dest_term (List.assoc (dest_var_nods ttt) sub)
               with _ ->
                  {term_op = ttt.term_op;
                   term_terms = List.map (do_bterm_subst sub) ttt.term_terms}
            in
               t.core <- Term t4;
               t4

   (*
    * Make a variable.
    *)
   let mk_var_term v =
      { free_vars = StringSet.make v;
        core = Term
         { term_op = { imp_op_name = var_opname; imp_op_params = [Var v] };
           term_terms = [] }}

   let make_term t =
      try mk_var_term (dest_var_nods t)
      with _ ->
         {free_vars = bterms_free_vars t.term_terms;
          core = Term t}

   let mk_term op bterms =
      {free_vars = bterms_free_vars bterms;
       core = Term { term_op = op; term_terms = bterms }}

   let mk_op name params = { imp_op_name = name; imp_op_params = params }

   let make_bterm bt =
      { bfree_vars = List.fold_right StringSet.remove bt.bvars bt.bterm.free_vars;
        bcore = BTerm bt }

   let mk_bterm vars term =
      { bfree_vars = List.fold_right StringSet.remove vars term.free_vars;
        bcore = BTerm {bvars = vars; bterm = term }}

   (*
    * New variable production.
    * renames are the variables to be renamed,
    * and av is a list list of variables to avoid.
    * Our algorithm is slow and simple: just append an
    * index and increment until no more collisions.
    *)

   let rec new_var av v i =
      let v' = v ^ "_" ^ (string_of_int i) in
      if (StringSet.mem v' av)
         then new_var av v (succ i)
         else v'

   and new_vars av = function
      [] -> ([],[])
    | v::vt ->
         let (vs,ts) = (new_vars av vt) in
         let v' = new_var av v 0 in
            ((v,v')::vs, (v,mk_var_term v')::ts)

   let rec dest_bterm bt =
      match bt.bcore with
         BTerm tt -> tt
       | BSubst (tt,sub) ->
            let ttt = dest_bterm tt in
            let btrm = ttt.bterm in
            let t4 =
               match ttt.bvars with
                  [] ->
                     { bvars = [];
                       bterm =
                        { free_vars = bt.bfree_vars;
                          core = Subst (btrm,sub) }}
                | bvrs ->
                     let sub_fvars = subst_free_vars sub in
                     let capt_vars = List_util.filter (function v -> StringSet.mem v sub_fvars) bvrs in
                     match capt_vars with
                        [] ->
                           let rec aux = function
                              [] -> bt.bfree_vars
                            | v::t ->
                               if StringSet.mem v btrm.free_vars
                                  then StringSet.add v (aux t)
                                  else aux t
                           in
                              { bvars = bvrs;
                                bterm =
                                 { free_vars = aux bvrs;
                                   core = Subst (btrm,sub) }}
                      | _ ->
                           let avoidvars = StringSet.union sub_fvars btrm.free_vars in
                           let (vs,ts) = new_vars avoidvars capt_vars in
                           let new_t = do_term_subst ts btrm in
                           { bvars =
                              List.map
                                 (function v ->
                                    try List.assoc v vs
                                    with Not_found -> v)
                                 bvrs;
                              bterm =
                                 { free_vars =
                                    StringSet.union
                                       (List.fold_right
                                          StringSet.remove
                                          (List_util.fst_split sub)
                                          new_t.free_vars)
                                       sub_fvars;
                                   core = Subst (new_t,sub) }}
            in
               bt.bcore <- BTerm t4;
               t4

   let no_bvars =
      List.for_all
         (function bt ->
            match dest_bterm bt with
               { bvars = [] } -> true
             | _ -> false)

   let dest_simple_bterm term bt =
      match dest_bterm bt with
         { bvars = []; bterm = tt } -> tt
       | _ -> raise (TermMatch ("dest_simple_bterm", term, "bvars exist"))

   let dest_simple_bterms term =
      List.map (dest_simple_bterm term)

   let mk_simple_bterm term =
      { bfree_vars = term.free_vars;
        bcore = BTerm { bvars = []; bterm = term }}

   let mk_level_var v i =
      { le_var = v; le_offset = i }

   let mk_level i l =
      { le_const = i; le_vars = l }

   let subterms_of_term t =
      List.map (fun bt -> (dest_bterm bt).bterm) (dest_term t).term_terms

   let subterm_count t =
      List.length (dest_term t).term_terms

   let subterm_arities term =
      let aux bterm = List.length (dest_bterm bterm).bvars in
         List.map aux (dest_term term).term_terms
   (*
    * Operator names.
    *)
   let opname_of_term t = (dest_term t).term_op.imp_op_name

   (* These are trivial identity functions *)

   let make_op { op_name = opname; op_params = params } =
      { imp_op_name = opname; imp_op_params = params }
   let dest_op { imp_op_name = opname; imp_op_params = params } =
      { op_name = opname; op_params = params}
   let make_param p = p
   let dest_param p = p
   let make_level l = l
   let dest_level l = l
   let make_level_var v = v
   let dest_level_var v = v
   let make_object_id i = i
   let dest_object_id i = i

   (************************************************************************
    * Variables                                                            *
    ************************************************************************)

   (*
    * See if a term is a variable.
    *)
   let is_var_term t = is_var_term_nods (dest_term t)

   (*
    * Destructor for a variable.
    *)
   let dest_var t =
      try
         dest_var_nods (dest_term t)
      with
         Not_var -> raise (TermMatch ("dest_var_nods",t,"not a var"))

   let mk_var_op v = { imp_op_name = var_opname; imp_op_params = [Var v] }

   (*
    * Second order variables have subterms.
    *)
   let is_so_var_term t = match dest_term t with
      { term_op = { imp_op_name = opname; imp_op_params = [Var(_)] }; term_terms = bterms }
      when opname == var_opname -> no_bvars bterms
    | _ -> false

   let dest_so_var t = match dest_term t with
      { term_op = { imp_op_name = opname; imp_op_params = [Var(v)] };
         term_terms = bterms
      } when opname == var_opname ->
         v, dest_simple_bterms t bterms
    | _ -> raise (TermMatch ("dest_so_var", t, "not a so_var"))

   (*
    * Second order variable.
    *)
   let mk_so_var_term v terms =
      mk_term
         { imp_op_name = var_opname; imp_op_params = [Var(v)] }
         (List.map mk_simple_bterm terms)

   (*
    * Second order context, contains a context term, plus
    * binding variables like so vars.
    *)
   let context_opname = make_opname ["context"]

   let is_context_term t = match dest_term t with
      { term_op = { imp_op_name = opname; imp_op_params = [Var _] }; term_terms = bterms }
      when opname == context_opname -> no_bvars bterms
    | _ -> false

   let dest_context term = match dest_term term with
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
         term_terms = bterm :: bterms
       } when opname == context_opname ->
         v, dest_simple_bterm term bterm,
            dest_simple_bterms term bterms
    | _ -> raise (TermMatch ("dest_context", term, "not a context"))

   let mk_context_term v term terms =
      mk_term
         { imp_op_name = context_opname; imp_op_params = [Var v] }
         ((mk_simple_bterm term)::(List.map mk_simple_bterm terms))

   (************************************************************************
    * NORMALIZATION                                                        *
    ************************************************************************)

   (*
    * "Normalization" means producing a canonical version of the term,
    * not reduction. Right now, this just means rehashing the opname,
    * doing all substitutions and copying it all into an unshared location.
    *)
   let rec normalize_term trm =
      let t = dest_term trm in
      let op = t.term_op in
         op.imp_op_name <- normalize_opname op.imp_op_name;
         List.iter normalize_bterm t.term_terms

   and normalize_bterm btrm =
      let bt = dest_bterm btrm in
         normalize_term bt.bterm

   (************************************************************************
    * Tools for "simple" terms                                             *
    ************************************************************************)

   (*
    * "Simple" terms have no parameters and no binding variables.
    *)
   let is_simple_term_opname name t = match dest_term t with
      { term_op = { imp_op_name = name'; imp_op_params = [] };
        term_terms = bterms
      } when name' = name -> no_bvars bterms
    | _ -> false

   let mk_any_term op terms = mk_term op (List.map mk_simple_bterm terms)

   let mk_simple_term name terms =
      mk_any_term { imp_op_name = name; imp_op_params = [] } terms

   let dest_simple_term t = match dest_term t with
      { term_op = { imp_op_name = name; imp_op_params = [] };
         term_terms = bterms } ->
            name, dest_simple_bterms t bterms
    | _ -> raise (TermMatch ("dest_simple_term", t, "params exist"))

   let dest_simple_term_opname name t = match dest_term t with
      { term_op = { imp_op_name = name'; imp_op_params = [] };
         term_terms = bterms } ->
         if name == name' then dest_simple_bterms t bterms
         else
            raise (TermMatch ("dest_simple_term_opname", t, "opname mismatch"))
    | _ -> raise (TermMatch ("dest_simple_term_opname", t, "params exist"))
end
