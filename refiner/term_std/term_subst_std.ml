(*
 * Substitution, alpha equality, and unification.
 *)

open Term_std

module TermSubst =
struct
   open Term
   
   type term = Term.term
   type param = Term.param

   type term_subst = (string * term) list

   (************************************************************************
    * Free variable calculations                                           *
    ************************************************************************)
   
   (*
    * Calculate the list of free variables.
    * Also count second order variables.
    * Just recursively descend the term, keeping track
    * of bound variables.
    *)
   let rec free_vars_term gvars bvars = function
      { term_op = { op_name = opname; op_params = [Var(v)] }; term_terms = bterms } when opname == var_opname ->
         (* This is a variable *)
         let gvars' = if List.mem v bvars or List.mem v gvars then
                         gvars
                      else
                         v::gvars
         in
            free_vars_bterms gvars' bvars bterms
    | { term_terms = bterms } ->
         free_vars_bterms gvars bvars bterms
   
   and free_vars_bterms gvars bvars = function
      { bvars = vars; bterm = term}::l ->
         let bvars' = vars @ bvars in
         let gvars' = free_vars_term gvars bvars' term in
            free_vars_bterms gvars' bvars l
   
    | [] -> gvars
   
   (* Actual function *)
   let free_vars = free_vars_term [] []
   
   (* Collect over a list of terms *)
   let free_vars_terms =
      let rec aux gvars = function
         [] -> gvars
       | t::r -> aux (free_vars_term gvars [] t) r
      in
         aux []
   
   (*
    * See if a variable is free.
    *)
   let is_free_var v =
      let rec free_vars_term bvars = function
         { term_op = { op_name = opname; op_params = [Var v'] };
           term_terms = []
         } when opname == var_opname ->
            v' = v
       | { term_terms = bterms } ->
            free_vars_bterms bvars bterms
   
      and free_vars_bterms bvars = function
         { bvars = bvars'; bterm = term }::t ->
            if List.mem v bvars' then
               free_vars_bterms bvars t
            else
               (free_vars_term (bvars' @ bvars) term) or (free_vars_bterms bvars t)
               
       | [] -> false
      in
         free_vars_term []
   
   (*
    * Similar operation on contexts.
    *)
   let rec context_vars_term cvars = function
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = bterms
      } when opname == context_opname ->
         let cvars' =
            if List.mem v cvars then
               cvars
            else
               v::cvars
         in
            context_vars_bterms cvars' bterms
    | { term_terms = bterms } ->
         context_vars_bterms cvars bterms
   
   and context_vars_bterms cvars = function
      { bterm = t }::l ->
         context_vars_bterms (context_vars_term cvars t) l
    | [] -> cvars
   
   let context_vars = context_vars_term []
   
   (*
    * Collect all binding vars.
    *)
   let rec binding_vars_term bvars = function
      { term_terms = bterms } ->
         binding_vars_bterms bvars bterms
   
   and binding_vars_bterms bvars = function
      { bvars = vars; bterm = t }::l ->
         binding_vars_bterms (binding_vars_term (List_util.union vars bvars) t) l
    | [] -> bvars
   
   let binding_vars = binding_vars_term []

   (************************************************************************
    * ALPHA EQUALITY                                                       *
    ************************************************************************)
   
   (*
    * Recursive computation of alpha equality.
    *)
   let equal_params p1 p2 =
      match p1, p2 with
         Number n1, Number n2 ->
            Num.eq_num n1 n2
       | _ ->
            p1 = p2
   
   let rec equal_term vars t t' =
      match t, t' with
         { term_op = { op_name = opname1; op_params = [Var v] };
           term_terms = []
         },
         { term_op = { op_name = opname2; op_params = [Var v'] };
           term_terms = []
         } when opname1 == var_opname & opname2 == var_opname ->
            begin
               try List.assoc v vars = v' with
                  Not_found -> v = v'
            end
       | { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            name1 = name2
                    & List_util.for_all2 equal_params params1 params2
                    & equal_bterms vars bterms1 bterms2
   
   and equal_bterms vars bterms1 bterms2 =
      let equal_bterm = fun
         { bvars = bvars1; bterm = term1 }
         { bvars = bvars2; bterm = term2 } ->
            equal_term (List_util.zip_list vars bvars1 bvars2) term1 term2
      in
         List_util.for_all2 equal_bterm bterms1 bterms2
   
   let alpha_equal t1 t2 =
      try equal_term [] t1 t2 with
         Invalid_argument _ -> false
   
   let alpha_equal_vars (t, v) (t', v') =
      try equal_term (List_util.zip v v') t t' with
         Invalid_argument _ -> false
   
   (*
    * Check the following:
    *   that t' = t[terms[v''/v''']/v]
    *)
   let equal_comp vars' =
      let rec equal_comp_term vars = function
         { term_op = { op_name = opname; op_params = [Var v] };
           term_terms = []
         }, t' when opname == var_opname ->
            begin
               try equal_term vars' t' (List.assoc v vars) with
                  Not_found ->
                     begin
                        match t' with
                           { term_op = { op_name = opname; op_params = [Var v'] };
                             term_terms = []
                           } when opname == var_opname -> v = v'
                         | _ -> false
                     end
            end
       | { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            name1 = name2 & params1 = params2 & equal_comp_bterms vars bterms1 bterms2
   
      and equal_comp_bterms vars bterms1 bterms2 =
         let equal_comp_bterm = fun
            { bvars = bvars1; bterm = term1 }
            { bvars = bvars2; bterm = term2 } ->
               equal_comp_term (List_util.zip_list vars bvars1 (List.map mk_var_term bvars2)) (term1, term2)
         in
            List_util.for_all2 equal_comp_bterm bterms1 bterms2
      in
          equal_comp_term
      
   let alpha_equal_match (t, v) (t', v'', v''', terms) =
      try equal_comp (List_util.zip v''' v'') (List_util.zip v terms) (t, t') with
         Invalid_argument _ -> false
   
   (************************************************************************
    * Substitution                                                         *
    ************************************************************************)
   
   (*
    * Utilities for subst.
    *)
   let rec fsubtract l = function
      [] -> l
    | h::t ->
         fsubtract (List_util.subtract l h) t
   
   (*
    * Add a var list.
    *)
   let add_renames_terms r l =
      let rec aux = function
         [] -> l
       | v::t -> (mk_var_term v)::(aux t)
      in
         aux r
   
   (*
    * Add a var list onto free vars.
    *)
   let add_renames_fv r l =
      let rec aux = function
         [] -> l
       | v::t -> [v]::(aux t)
      in
         aux r
   
   (*
    * New variable production.
    * renames are the variables to be renamed,
    * and fv is a list list of variables to avoid.
    * Our algorithm is slow and simple: just append an
    * index and increment until no more collisions.
    *)
   let new_vars renames fv =
      let rec new_var v i =
         (* Try the new value *)
         let rec mem' v = function
            [] -> false
          | h::t -> List.mem v h
         in
         let v' = v ^ (string_of_int i) in
            if mem' v' fv then
               new_var v (i + 1)
            else
               v'
      in
      let rec aux fv = function
         [] -> []
       | v::t ->
            (* Rename the first one, then add it to free vars *)
            let v' = new_var v 1 in
               v'::(aux ([v']::fv) t)
      in
         aux fv renames
   
   (*
    * First order simultaneous substitution.
    *)
   let subst term terms vars =
      let rec subst_term terms fv vars = function
         { term_op = { op_name = opname; op_params = [Var(v)] }; term_terms = [] } as t
         when opname == var_opname->
            (* Var case *)
            begin
               try List.nth terms (List_util.find_index v vars) with
                  Not_found ->
                     t
            end
       | { term_op = op; term_terms = bterms } ->
            (* Other term *)
            { term_op = op; term_terms = subst_bterms terms fv vars bterms }
   
      and subst_bterms terms fv vars bterms =
         (* When subst through bterms, catch binding occurrences *)
         let rec subst_bterm = function
            { bvars = []; bterm = term } ->
               (* Optimize the common case *)
               { bvars = []; bterm = subst_term terms fv vars term }
   
          | { bvars = bvars; bterm = term } ->
               (* First subtract bound instances *)
               let flags = List.map (function v -> List.mem v bvars) vars in
               let vars' = List_util.remove_elements flags vars in
               let fv' = List_util.remove_elements flags fv in
               let terms' = List_util.remove_elements flags terms in
   
               (* If any of the binding variables are free, rename them *)
               let renames = List_util.subtract bvars (fsubtract bvars fv') in
                  if renames <> [] then
                     let fv'' = (free_vars term)::fv' in
                     let renames' = new_vars renames fv'' in
                        { bvars = subst_bvars renames' renames bvars;
                          bterm = subst_term
                              (add_renames_terms renames' terms')
                              (add_renames_fv renames' fv')
                              (renames @ vars')
                              term
                        }
                  else
                     { bvars = bvars;
                       bterm = subst_term terms' fv' vars' term
                     }
         in
            List.map subst_bterm bterms
   
      and subst_bvars renames' renames bvars =
         let subst_bvar v =
            try List.nth renames' (List_util.find_index v renames) with
               Not_found -> v
         in
            List.map subst_bvar bvars
   
      in
         subst_term terms (List.map free_vars terms) vars term
   
   (*
    * Inverse substitution.
    *)
   let var_subst t t' v =
      let { term_op = { op_name = opname } } = t' in
      let vt = mk_var_term v in
      let rec subst_term = function
         { term_op = { op_name = opname'; op_params = params };
           term_terms = bterms
         } as t ->
            (* Check if this is the same *)
            if opname' == opname & alpha_equal t t' then
               vt
            else
               { term_op = { op_name = opname'; op_params = params };
                 term_terms = List.map subst_bterm bterms
               }
               
      and subst_bterm { bvars = vars; bterm = term } =
         { bvars = vars; bterm = subst_term term }
      in
         subst_term t
                        
   (************************************************************************
    * UNIFICATION                                                          *
    ************************************************************************)
   
   (*
    * Utilities.
    *)
   let rev_assoc v =
      let rec aux = function
         (v1, v2)::t ->
            if v2 = v then
               v1
            else
               aux t
       | [] -> raise Not_found
      in
         aux
   
   let rec zip_cons l = function
      v1::t1, v2::t2 -> zip_cons ((v1, v2)::l) (t1, t2)
    | [], [] -> l
    | _ -> raise (Invalid_argument "zip_cons")
   
   (*
    * Unify two terms.
    *)
   let rec unify_terms subst bvars = function
      ({ term_op = { op_name = opname; op_params = [Var v] };
         term_terms = []
       } as t1), t2
      when opname == var_opname ->
         (* t1 is a variable *)
         begin
            try
               if (List.assoc v bvars) = (dest_var t2) then
                  subst
               else
                  raise (BadMatch (t1, t2))
            with
               Not_found ->
                  begin
                     try unify_terms subst bvars (List.assoc v subst, t2) with
                        Not_found ->
                           (v, t2)::subst
                  end
             | TermMatch _ ->
                  raise (BadMatch (t1, t2))
         end
            
    | t1, ({ term_op = { op_name = opname; op_params = [Var v] };
             term_terms = []
           } as t2)
      when opname == var_opname ->
         (* t2 is a variable *)
         begin
            try
               if (rev_assoc v bvars) = (dest_var t1) then
                  subst
               else
                  raise (BadMatch (t1, t2))
            with
               Not_found ->
                  begin
                     try unify_terms subst bvars (t1, List.assoc v subst) with
                        Not_found ->
                           (v, t1)::subst
                  end
             | TermMatch _ ->
                  raise (BadMatch (t1, t2))
         end
            
    | ({ term_op = { op_name = opname1; op_params = params1 };
         term_terms = bterms1
       } as t1),
      ({ term_op = { op_name = opname2; op_params = params2 };
         term_terms = bterms2
       } as t2) ->
         (* General case *)
         if opname1 == opname2 & params1 = params2 then
            try unify_bterms subst bvars (bterms1, bterms2) with
               Invalid_argument _ -> raise (BadMatch (t1, t2))
         else
            raise (BadMatch (t1, t2))
               
   and unify_bterms subst bvars = function
      ({ bvars = vars1; bterm = term1 }::tl1),
      ({ bvars = vars2; bterm = term2 }::tl2) ->
         let subst' = unify_terms subst (zip_cons bvars (vars1, vars2)) (term1, term2) in
            unify_bterms subst' bvars (tl1, tl2)
    | [], [] -> subst
    | _ -> raise (Invalid_argument "unify_bterms")
   
   let unify subst t1 t2 =
      List.rev (unify_terms subst [] (t1, t2))

   (************************************************************************
    * Term generalization                                                  *
    ************************************************************************)

   (*
    * Generalization computation.
    * See if the first term generalizes the second, and
    * compute the alpha conversion.  If the generalization
    * is _not_ true, then raise the exception:
    * Invalid_argument "generalization".
    *
    * The generalization is with respect to matching:
    *    1. A variable matches anything
    *    2. A second order variable matches
    *       anything bound according to the subterms
    *    3. A meta-parameter matches a parameter of
    *       the same parameter type.
    *)
   let generalization =
      let rec generalizes_term vars t1 t2 =
         if is_so_var_term t1 then
            vars
         else if is_context_term t1 then
            if is_context_term t2 then
               let _, t1', _ = dest_context t1 in
               let _, t2', _ = dest_context t2 in
                  generalizes_term vars t1' t2'
            else
               raise (Invalid_argument "generalization")
         else if is_so_var_term t2 or is_context_term t2 then
            raise (Invalid_argument "generalization")
         else
            (* Regular terms *)
            let { term_op = op1; term_terms = bterms1 } = dest_term t1 in
            let { term_op = op2; term_terms = bterms2 } = dest_term t2 in
            let { op_name = name1; op_params = params1 } = dest_op op1 in
            let { op_name = name2; op_params = params2 } = dest_op op2 in
               if name1 = name2 then
                  try
                     List.iter2 generalizes_param params1 params2;
                     List.fold_left2 generalizes_bterm vars bterms1 bterms2
                  with
                     Invalid_argument _ ->
                        raise (Invalid_argument "generalization")
               else
                  raise (Invalid_argument "generalization")

      and generalizes_param param1 param2 =
         if param1 <> param2 then
            raise (Invalid_argument "generalization")

      and generalizes_bterm vars bterm1 bterm2 =
         (* Keep track of binding vars *)
         let { bvars = vars1; bterm = term1 } = dest_bterm bterm1 in
         let { bvars = vars2; bterm = term2 } = dest_bterm bterm2 in
         let aux vars v1 v2 =
            try
               if v2 = List.assoc v1 vars then
                  vars
               else
                  raise (Invalid_argument "generalization")
            with _ ->
                  (v1, v2)::vars
         in
         let vars' = List.fold_left2 aux vars vars1 vars2 in
            generalizes_term vars' term1 term2
      in
         (* Start it *)
         generalizes_term

   let generalizes t1 t2 =
      try generalization [] t1 t2; true with
         Invalid_argument "generalization" -> false
end

(*
 * $Log$
 * Revision 1.2  1998/05/29 04:11:05  nogin
 * Fixed some typos.
 * Use == instead of = for comparing opnames.
 *
 * Revision 1.1  1998/05/28 15:02:44  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:59  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
