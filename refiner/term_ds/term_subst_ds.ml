open Term_ds

module TermSubst =
struct
   open Term

   type term = Term.term
   type param = Term.param

   type term_subst = (string * term) list

   let subst t tl vl = do_term_subst (List.combine vl tl) t

   let is_free_var v t = StringSet.mem v t.free_vars

   let free_vars t = StringSet.elements t.free_vars

   let var_subst t t2 v =
      if StringSet.mem v t.free_vars
         then
            { free_vars = StringSet.union t2.free_vars (StringSet.remove v t.free_vars);
              core = Subst (t,[(v,t2)])}
         else t

   (*
    * Collect all binding vars.
    *)
   let rec binding_vars_term bvars = function t ->
      binding_vars_bterms bvars (dest_term t).term_terms

   and binding_vars_bterms bvars = function
      btrm::l ->
         let bt = dest_bterm btrm in
         binding_vars_bterms (binding_vars_term (List_util.union bt.bvars bvars) bt.bterm) l
   | [] -> bvars

   let binding_vars = binding_vars_term []

   let free_vars_terms = function
      [] -> []
    | hd::tl ->
         StringSet.elements
            (List.fold_left
               (function vars -> function trm -> StringSet.union vars trm.free_vars)
               hd.free_vars tl)
   let rec context_vars_term cvars t = match dest_term t with
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
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
      bt::l ->
         context_vars_bterms (context_vars_term cvars (dest_bterm bt).bterm) l
    | [] -> cvars

   let context_vars = context_vars_term []

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

   let rec remove_var v = function
      [] -> []
    | (v',_)::tl when v' = v -> remove_var v tl
    | hd::tl -> hd::(remove_var v tl)

   let rec join_vars vars = function
      ([],[]) -> vars
    | (v1::vt1,v2::vt2) ->
         if (v1=v2)
            then join_vars (remove_var v1 vars) (vt1,vt2)
            else (v1,v2)::(join_vars vars (vt1,vt2))
    | _ -> raise (Invalid_argument ("join_vars"))

   let rec equal_term_main vars t t' =
      match (dest_term t, dest_term t') with
         { term_op = { imp_op_name = opname1; imp_op_params = [Var v] };
           term_terms = []
         },
         { term_op = { imp_op_name = opname2; imp_op_params = [Var v'] };
           term_terms = []
         } when opname1 == var_opname & opname2 == var_opname ->
            begin
               try List.assoc v vars = v' with
                  Not_found -> v = v'
            end
       | { term_op = { imp_op_name = name1; imp_op_params = params1 }; term_terms = bterms1 },
         { term_op = { imp_op_name = name2; imp_op_params = params2 }; term_terms = bterms2 } ->
            name1 == name2
                    & List_util.for_all2 equal_params params1 params2
                    & List_util.for_all2 (equal_bterm vars) bterms1 bterms2

   and equal_term = function
      [] ->
         (function t1  ->
            function t2 ->
               (t1 == t2) || equal_term_main [] t1 t2)
    | vars -> equal_term_main vars

   and equal_bterm_main vars btrm1 btrm2 =
      let bt1 = dest_bterm btrm1 in
      let bt2 = dest_bterm btrm2 in
      equal_term (join_vars vars (bt1.bvars,bt2.bvars)) bt1.bterm bt2.bterm

   and equal_bterm = function
      [] ->
         (function bt1 ->
            function bt2 ->
               (bt1 == bt2) || equal_bterm_main [] bt1 bt2)
    | vars -> equal_bterm_main vars

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
   let rec equal_comp vars' vars t t' = match dest_term t with
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
        term_terms = []
      } when opname == var_opname ->
         (try equal_term vars' t' (List.assoc v vars) with
            Not_found ->
               begin
                  match dest_term t' with
                     { term_op = { imp_op_name = opname; imp_op_params = [Var v'] };
                       term_terms = []
                     } when opname == var_opname -> v = v'
                   | _ -> false
               end)
    | { term_op = { imp_op_name = name1; imp_op_params = params1 }; term_terms = bterms1 } ->
         (function
            { term_op = { imp_op_name = name2; imp_op_params = params2 }; term_terms = bterms2 } ->
            name1 == name2 & params1 = params2 & equal_comp_bterms vars' vars bterms1 bterms2)
         (dest_term t')

   and equal_comp_bterms vars' vars bterms1 bterms2 =
      let equal_comp_bterm btrm1 btrm2 =
         let bt1 = dest_bterm btrm1 and
             bt2 = dest_bterm btrm2 in
         equal_comp vars'
            (List_util.zip_list vars bt1.bvars (List.map mk_var_term bt2.bvars))
            bt1.bterm bt2.bterm
      in
         List_util.for_all2 equal_comp_bterm bterms1 bterms2

   let alpha_equal_match (t, v) (t', v'', v''', terms) =
      try equal_comp (List_util.zip v''' v'') (List_util.zip v terms) t t'  with
         Invalid_argument _ -> false

   (************************************************************************
    * UNIFICATION                                                          *
    ************************************************************************)

   (*
    * Utilities.
    *)
   let rec rev_assoc v = function
      (v1, v2)::t ->
         if v2 = v then
            v1
         else
            rev_assoc v t
    | [] -> raise Not_found

   let rec zip_cons l = function
      v1::t1, v2::t2 ->
         if (v1=v2)
            then zip_cons (remove_var v1 l) (t1,t2)
            else zip_cons ((v1, v2)::l) (t1, t2)
    | [], [] -> l
    | _ -> raise (Invalid_argument "zip_cons")

   (*
    * Unify two terms.
    *)
   let rec unify_terms subst bvars tm1 tm2 = match (dest_term tm1, dest_term tm2) with
      ({ term_op = { imp_op_name = opname; imp_op_params = [Var v] };
         term_terms = []
       } as t1), t2
      when opname == var_opname ->
         (* t1 is a variable *)
         begin
            try
               if (List.assoc v bvars) = (dest_var_nods t2) then
                  subst
               else
                  raise (BadMatch (tm1, tm2))
            with
               Not_found ->
                  begin
                     try unify_terms subst bvars (List.assoc v subst) tm2 with
                        Not_found ->
                           (v, tm2)::subst
                  end
             | Not_var ->
                  raise (BadMatch (tm1, tm2))
         end

    | t1, ({ term_op = { imp_op_name = opname; imp_op_params = [Var v] };
             term_terms = []
           } as t2)
      when opname == var_opname ->
         (* t2 is a variable *)
         begin
            try
               if (rev_assoc v bvars) = (dest_var_nods t1) then
                  subst
               else
                  raise (BadMatch (tm1, tm2))
            with
               Not_found ->
                  begin
                     try unify_terms subst bvars tm1 (List.assoc v subst) with
                        Not_found ->
                           (v, tm1)::subst
                  end
             | Not_var ->
                  raise (BadMatch (tm1, tm2))
         end

    | ({ term_op = { imp_op_name = opname1; imp_op_params = params1 };
         term_terms = bterms1
       } as t1),
      ({ term_op = { imp_op_name = opname2; imp_op_params = params2 };
         term_terms = bterms2
       } as t2) ->
         (* General case *)
         if opname1 == opname2 & params1 = params2 then
            try unify_bterms subst bvars (bterms1, bterms2) with
               Invalid_argument _ -> raise (BadMatch (tm1, tm2))
         else
            raise (BadMatch (tm1, tm2))

   and unify_bterms subst bvars = function
      (btrm1::tl1), (btrm2::tl2) ->
         let bt1 = dest_bterm btrm1
         and bt2 = dest_bterm btrm2 in
         let subst' = unify_terms subst (zip_cons bvars (bt1.bvars, bt2.bvars)) bt1.bterm bt2.bterm in
            unify_bterms subst' bvars (tl1, tl2)
    | [], [] -> subst
    | _ -> raise (Invalid_argument "unify_bterms")

   let unify subst t1 t2 =
      List.rev (unify_terms subst [] t1 t2)

   (************************************************************************
    * Term generalization                                                  *
    ************************************************************************)

   (*
    * Generalization computation.
    * See if the first term generalizes the second, and
    * compute the alpha conversion.  If the generalization
    * is _not_ true, then raise the exception:
    * Failurevalid_argument "generalization".
    *
    * The generalization is with respect to matching:
    *    1. A variable matches anything
    *    2. A second order variable matches
    *       anything bound according to the subterms
    *    3. A meta-parameter matches a parameter of
    *       the same parameter type.
    *)
   let rec generalizes_term vars t1 t2 =
      if is_so_var_term t1 then
         vars
      else if is_context_term t1 then
         if is_context_term t2 then
            let _, t1', _ = dest_context t1 in
            let _, t2', _ = dest_context t2 in
               generalizes_term vars t1' t2'
         else
            raise (Failure "generalization")
      else if is_so_var_term t2 or is_context_term t2 then
         raise (Failure "generalization")
      else
         (* Regular terms *)
         let { term_op = op1; term_terms = bterms1 } = dest_term t1 in
         let { term_op = op2; term_terms = bterms2 } = dest_term t2 in
         let { op_name = name1; op_params = params1 } = dest_op op1 in
         let { op_name = name2; op_params = params2 } = dest_op op2 in
            if name1 == name2 then
               try
                  List_util.iter2 generalizes_param params1 params2;
                  List_util.fold_left2 generalizes_bterm vars bterms1 bterms2
               with
                  Failure _ ->
                     raise (Failure "generalization")
            else
               raise (Failure "generalization")

   and generalizes_param param1 param2 =
      if param1 <> param2 then
         raise (Failure "generalization")

   and generalizes_bterm vars bterm1 bterm2 =
      (* Keep track of binding vars *)
      let { bvars = vars1; bterm = term1 } = dest_bterm bterm1 in
      let { bvars = vars2; bterm = term2 } = dest_bterm bterm2 in
      let aux vars v1 v2 =
         try
            if v2 = List.assoc v1 vars then
               vars
            else
               raise (Failure "generalization")
         with _ ->
               (v1, v2)::vars
      in
      let vars' = List.fold_left2 aux vars vars1 vars2 in
         generalizes_term vars' term1 term2

   let generalization = generalizes_term

   let generalizes t1 t2 =
      try generalizes_term [] t1 t2; true with
         Failure "generalization" ->
            false

end
