(*
 * Standard operations on terms.
 *)

open Term_ds

module TermOp =
struct
   open Term

   type term = Term.term
   type operator = Term.operator
   type level_exp = Term.level_exp

   (*
    * Terms with one subterm
    *)
   let is_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt]
      } when opname' = opname -> (dest_bterm bt).bvars = []
    | _ -> false

   let mk_dep0_term opname t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [mk_simple_bterm t]}}

   let dest_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt]
      } when opname' = opname -> dest_simple_bterm t bt
    | _ -> raise (TermMatch ("dest_dep0_term", t, ""))

   let one_subterm t = match dest_term t with
      { term_terms = [bt]}  -> dest_simple_bterm t bt
    | _ -> raise (TermMatch ("one_subterm", t, ""))

   (*
    * Terms with two subterms.
    *)
   let is_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname' = opname ->
         (dest_bterm bt1).bvars = [] && (dest_bterm bt2).bvars = []
    | _ -> false

   let mk_dep0_dep0_term opname t1 t2 =
      { free_vars = StringSet.union t1.free_vars t2.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [mk_simple_bterm t1; mk_simple_bterm t2]}}

   let dest_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1 ; bt2 ]
      } when opname' = opname ->
         let destr = dest_simple_bterm t in
         destr bt1, destr bt2
    | _ -> raise (TermMatch ("dest_dep0_dep0_term", t, ""))

   let two_subterms t = match dest_term t with
      { term_terms = [bt1; bt2]} ->
         let destr = dest_simple_bterm t in
         destr bt1, destr bt2
    | _ -> raise (TermMatch ("two_subterms", t, ""))

   (*
    * Terms with three subterms.
    *)
   let is_dep0_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = ([b1; b2; b3] as bterms)
      } when opname' = opname -> no_bvars bterms
    | _ -> false

   let mk_dep0_dep0_dep0_term opname t1 t2 t3 =
      { free_vars = StringSet.union t1.free_vars (StringSet.union t2.free_vars t3.free_vars);
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t1; mk_simple_bterm t2; mk_simple_bterm t3]}}

   let dest_dep0_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when opname' = opname ->
         let destr = dest_simple_bterm t in
         destr bt1, destr bt2, destr bt3
    | _ -> raise (TermMatch ("dest_dep0_dep0_dep0_term", t, ""))

   let three_subterms t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } ->
         let destr = dest_simple_bterm t in
         destr bt1, destr bt2, destr bt3
    | _ -> raise (TermMatch ("three_subterms", t, ""))

   let four_subterms t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3; bt4]
      } ->
         let destr = dest_simple_bterm t in
         destr bt1, destr bt2, destr bt3, destr bt4
    | _ -> raise (TermMatch ("four_subterms", t, ""))

   let five_subterms t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3; bt4; bt5]
      } ->
         let destr = dest_simple_bterm t in
         destr bt1, destr bt2, destr bt3, destr bt4, destr bt5
    | _ -> raise (TermMatch ("five_subterms", t, ""))

   (************************************************************************
    * Nonsimple but useful forms                                           *
    ************************************************************************)

   (*
    * One string param.
    *)
   let is_string_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String _] };
        term_terms = []
      } when opname == opname' ->
         true
    | _ ->
         false

   let dest_string_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = []
      } when opname == opname' ->
         s
    | _ ->
         raise (TermMatch ("dest_string_term", t, "not a string term"))

   let dest_string_param t = match dest_term t with
      { term_op = { op_params = String s :: _ } } ->
         s
    | _ ->
         raise (TermMatch ("dest_string_param", t, "no string parameter"))

   let mk_string_term opname s =
      { free_vars = StringSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [String s] }; term_terms = [] }}

   (*
    * One string parameter, and one simple subterm.
    *)
   let is_string_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String _] };
        term_terms = [bt]
      } when opname == opname' -> (dest_bterm bt).bvars = []
    | _ -> false

   let dest_string_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = [bt]
      } when opname == opname' -> s, dest_simple_bterm t bt
    | _ -> raise (TermMatch ("dest_string_dep0_term", t, ""))

   let mk_string_dep0_term opname s t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [mk_simple_bterm t] }}

   (*
    * Two string parameters, and one simple subterm.
    *)
   let is_string_string_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String _; String _] };
        term_terms = [bt]
      } when opname == opname' -> (dest_bterm bt).bvars = []
    | _ ->
         false

   let dest_string_string_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [bt]
      } when opname == opname' ->
         s1, s2, dest_simple_bterm t bt
    | _ ->
         raise (TermMatch ("dest_string_string_dep0_term", t, ""))

   let dest_string_string_dep0_any_term t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [bt]
      } ->
         s1, s2, dest_simple_bterm t bt
    | _ ->
         raise (TermMatch ("dest_string_string_dep0_any_term", t, ""))

   let mk_string_string_dep0_term opname s1 s2 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [mk_simple_bterm t] }}

   (*
    * Two number parameters and one subterm.
    *)
   let is_number_number_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number _; Number _] };
        term_terms = [ bt ]
      } when opname == opname' -> (dest_bterm bt).bvars = []
    | _ ->
         false

   let dest_number_number_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
        term_terms = [bt]
      } when opname == opname' ->
         s1, s2, dest_simple_bterm t bt
    | _ ->
         raise (TermMatch ("dest_number_number_dep0_term", t, ""))

   let dest_number_number_dep0_any_term t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
        term_terms = [bt]
      } ->
         s1, s2, dest_simple_bterm t bt
    | _ ->
         raise (TermMatch ("dest_number_number_dep0_any_term", t, ""))

   let mk_number_number_dep0_term opname s1 s2 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [Number s1; Number s2] };
           term_terms = [mk_simple_bterm t]}}

   (*
    * Two string parameters, two subterms.
    *)
   let is_string_string_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String _; String _] };
        term_terms = [bt1;bt2]
      } when opname == opname' ->
         (dest_bterm bt1).bvars = [] && (dest_bterm bt2).bvars = []
    | _ ->
         false

   let dest_string_string_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [bt1;bt2]
      } when opname == opname' ->
         let destr = dest_simple_bterm t in
         s1, s2, destr bt1, destr bt2
    | _ ->
         raise (TermMatch ("dest_string_string_dep0_dep0_term", t, ""))

   let dest_string_string_dep0_dep0_any_term t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [bt1;bt2]
      } ->
         let destr = dest_simple_bterm t in
         s1, s2, destr bt1, destr bt2
    | _ ->
         raise (TermMatch ("dest_string_string_dep0_dep0_any_term", t, ""))

   let mk_string_string_dep0_dep0_term opname s1 s2 t1 t2 =
      { free_vars = StringSet.union t1.free_vars t2.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [mk_simple_bterm t1; mk_simple_bterm t2]}}

   (*
    * One number param.
    *)
   let is_number_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number _] };
        term_terms = []
      } when opname == opname' -> true
    | _ -> false

   let dest_number_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number n] };
        term_terms = []
      } when opname == opname' -> n
    | _ -> raise (TermMatch ("dest_number_term", t, ""))

   let dest_number_any_term t = match dest_term t with
      { term_op = { op_params = [Number n] };
        term_terms = []
      } ->
         n
    | _ ->
         raise (TermMatch ("dest_number_any_term", t, ""))

   let mk_number_term opname n =
      { free_vars = StringSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [Number n] };
           term_terms = [] }}

   (*
    * One universe param.
    *)
   let is_univ_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Level _] };
        term_terms = []
      } when opname == opname' -> true
    | _ -> false

   let dest_univ_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Level n] };
        term_terms = []
      } when opname == opname' -> n
    | _ -> raise (TermMatch ("dest_univ_term", t, ""))

   let mk_univ_term opname n =
      { free_vars = StringSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [Level n] };
           term_terms = [] }}

   (*
    * One token param.
    *)
   let is_token_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Token _] };
        term_terms = []
      } when opname == opname' -> true
    | _ -> false

   let dest_token_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Token n] };
        term_terms = []
      } when opname == opname' -> n
    | _ -> raise (TermMatch ("dest_token_term", t, ""))

   let mk_token_term opname n =
      { free_vars = StringSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [Token n] };
           term_terms = [] }}

   (*
    * Bound term.
    *)
   let is_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt]
      } when opname' = opname ->
         begin
            match (dest_bterm bt).bvars with
               [_] -> true
             | _ -> false
         end
    | _ -> false

   let mk_dep1_term opname v t =
      let fv = StringSet.remove v t.free_vars in
      { free_vars = fv;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [{ bfree_vars = fv;
               bcore = BTerm { bvars = [v]; bterm = t }}]}}

   let dest_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt]
      } when opname' = opname ->
         begin
            match dest_bterm bt with
               { bvars = [v]; bterm = t } -> v,t
             | _ -> raise (TermMatch ("dest_dep1_term", t, ""))
         end
    | _ -> raise (TermMatch ("dest_dep1_term", t, ""))

   let is_dep0_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1;bt2]
      } when opname' = opname ->
         if (dest_bterm bt1).bvars = []
            then match (dest_bterm bt2).bvars with
               [_] -> true
             | _ -> false
            else false
    | _ -> false

   let is_dep0_dep1_any_term t = match dest_term t with
      { term_op = { op_params = [] };
        term_terms = [bt1;bt2] } ->
          if (dest_bterm bt1).bvars = []
            then match (dest_bterm bt2).bvars with
               [_] -> true
             | _ -> false
            else false
    | _ -> false

   let mk_dep0_dep1_term opname v t1 t2 =
      let fv2 = StringSet.remove v t2.free_vars in
      { free_vars = StringSet.union t1.free_vars fv2;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
               [ mk_simple_bterm t1;
                 { bfree_vars = fv2;
                   bcore = BTerm { bvars = [v]; bterm = t2 }}]}}

   let mk_dep0_dep1_any_term op v t1 t2 =
      let fv2 = StringSet.remove v t2.free_vars in
      { free_vars = StringSet.union t1.free_vars fv2;
        core = Term
         { term_op = op;
           term_terms =
               [ mk_simple_bterm t1;
                 { bfree_vars = fv2;
                   bcore = BTerm { bvars = [v]; bterm = t2 }}]}}

   let dest_dep0_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1;bt2]
      } when opname' = opname ->
      begin match (dest_bterm bt1, dest_bterm bt2) with
         ({ bvars = []; bterm = t1 }, { bvars = [v]; bterm = t2 }) ->
            v, t1, t2
       | _ -> raise (TermMatch ("dest_dep0_dep1_term", t, ""))
      end
    | _ -> raise (TermMatch ("dest_dep0_dep1_term", t, ""))

   let dest_dep0_dep1_any_term t = match dest_term t with
      { term_op = { op_params = [] };
        term_terms = [bt1;bt2] } ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
            ({ bvars = []; bterm = t1 }, { bvars = [v]; bterm = t2 }) ->
               v, t1, t2
          | _ -> raise (TermMatch ("dest_dep0_dep1_term", t, ""))
         end
    | _ -> raise (TermMatch ("dest_dep0_dep1_any_term", t, ""))

   (*
    * First subterm of arity 2.
    *)
   let is_dep2_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1;bt2]
      } when opname' = opname ->
         if (dest_bterm bt2).bvars = []
            then match (dest_bterm bt1).bvars with
               [_;_] -> true
             | _ -> false
            else false
    | _ -> false

   let mk_dep2_dep0_term opname v1 v2 t1 t2 =
      let fv1 = StringSet.remove v1 (StringSet.remove v2 t1.free_vars) in
      { free_vars = StringSet.union fv1 t2.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [{ bfree_vars = fv1;
               bcore = BTerm { bvars = [v1; v2]; bterm = t1 }};
             mk_simple_bterm t2]}}

   let dest_dep2_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname' = opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
            ({ bvars = [v1; v2]; bterm = t1 },
             { bvars = []; bterm = t2 }) -> v1, v2, t1, t2
          | _ -> raise (TermMatch ("dest_dep2_dep0_term", t, ""))
         end
    | _ -> raise (TermMatch ("dest_dep2_dep0_term", t, ""))

   (*
    * Second subterm of arity 2.
    *)
   let is_dep0_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname' = opname ->
         if (dest_bterm bt1).bvars = []
            then match (dest_bterm bt2).bvars with
               [_;_] -> true
             | _ -> false
            else false
    | _ -> false

   let mk_dep0_dep2_term opname v1 v2 t1 t2 =
      let fv2 = StringSet.remove v1 (StringSet.remove v2 t2.free_vars) in
      { free_vars = StringSet.union t1.free_vars fv2;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t1;
             { bfree_vars = fv2;
               bcore = BTerm { bvars = [v1; v2]; bterm = t2 }}]}}

   let dest_dep0_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1;bt2]
      } when opname' = opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
            ({ bvars = []; bterm = t1 },
             { bvars = [v1; v2]; bterm = t2 }) -> v1, v2, t1, t2
          | _ -> raise (TermMatch ("dest_dep0_dep2_term", t, ""))
         end
    | _ -> raise (TermMatch ("dest_dep0_dep2_term", t, ""))

   (*
    * Three subterms.
    *)
   let is_dep0_dep2_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1;bt2;bt3]
      } when opname' = opname ->
         if (dest_bterm bt1).bvars = []
            then match ((dest_bterm bt2).bvars, (dest_bterm bt3).bvars) with
               ([_;_], [_;_]) -> true
             | _ -> false
            else false
    | _ -> false

   let mk_dep0_dep2_dep2_term opname t0 v11 v12 t1 v21 v22 t2 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_simple_bterm t0;
          mk_bterm [v11; v12] t1;
          mk_bterm [v21; v22] t2]

   let dest_dep0_dep2_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when opname' = opname ->
         begin
            match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
               ({ bvars = []; bterm = t0 },
                { bvars = [v11; v12]; bterm = t1 },
                { bvars = [v21; v22]; bterm = t2 })
                  -> t0, v11, v12, t1, v21, v22, t2
             | _ -> raise (TermMatch ("dest_dep0_dep2_dep2_term", t, ""))
         end
    | _ -> raise (TermMatch ("dest_dep0_dep2_dep2_term", t, ""))

   (*
    * Four subterms.
    *)
   let is_dep0_dep2_dep0_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3; bt4]
      } when opname' = opname ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3, dest_bterm bt4) with
            ({ bvars = [] }, { bvars = [_; _] }, { bvars = [] }, { bvars = [_; _] }) -> true
          | _ -> false
         end
    | _ -> false

   let mk_dep0_dep2_dep0_dep2_term opname t0 v11 v12 t1 base v21 v22 t2 =
         mk_term
            { op_name = opname; op_params = [] }
            [mk_simple_bterm t0;
             mk_bterm [v11; v12] t1;
             mk_simple_bterm base;
             mk_bterm [v21; v22] t2]

   let dest_dep0_dep2_dep0_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3; bt4]
      } when opname' = opname ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3, dest_bterm bt4) with
            ({ bvars = []; bterm = t0 },
             { bvars = [v11; v12]; bterm = t1 },
             { bvars = []; bterm = base },
             { bvars = [v21; v22]; bterm = t2 }) ->
               t0, v11, v12, t1, base, v21, v22, t2
          | _ -> raise (TermMatch ("dest_dep0_dep2_dep0_dep2_term", t, ""))
         end
    | _ -> raise (TermMatch ("dest_dep0_dep2_dep0_dep2_term", t, ""))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when opname' == opname ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
            ({ bvars = [] }, { bvars = [] }, { bvars = [_] }) -> true
          | _ -> false
         end
    | _ -> false

   let mk_dep0_dep0_dep1_term opname t0 t1 v2 t2 =
      let fv2 = StringSet.remove v2 t2.free_vars in
      { free_vars = StringSet.union t0.free_vars
                                    (StringSet.union t1.free_vars fv2);
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bfree_vars = fv2;
               bcore = BTerm { bvars = [v2]; bterm = t2 }}]}}

   let dest_dep0_dep0_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when opname' == opname ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
            ({ bvars = []; bterm = t0 },
             { bvars = []; bterm = t1 },
             { bvars = [v2]; bterm = t2 }) -> t0, t1, v2, t2
          | _ -> raise (TermMatch ("dest_dep0_dep0_dep1_term", t, ""))
         end
    | _ ->
         raise (TermMatch ("dest_dep0_dep0_dep1_term", t, ""))

   let is_dep0_dep0_dep1_any_term t = match dest_term t with
      { term_terms = [bt1; bt2; bt3] } ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
            ({ bvars = [] }, { bvars = [] }, { bvars = [_] }) -> true
          | _ -> false
         end
    | _ -> false

   let mk_dep0_dep0_dep1_any_term op t0 t1 v2 t2 =
      let fv2 = StringSet.remove v2 t2.free_vars in
      { free_vars = StringSet.union t0.free_vars
                                    (StringSet.union t1.free_vars fv2);
        core = Term
         { term_op = op;
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bfree_vars = fv2;
               bcore = BTerm { bvars = [v2]; bterm = t2 }}]}}

   let dest_dep0_dep0_dep1_any_term t = match dest_term t with
      { term_terms = [bt1; bt2; bt3] } ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
            ({ bvars = []; bterm = t0 },
             { bvars = []; bterm = t1 },
             { bvars = [v2]; bterm = t2 }) -> t0, t1, v2, t2
          | _ -> raise (TermMatch ("dest_dep0_dep0_dep1_any_term", t, ""))
         end
    | _ ->
         raise (TermMatch ("dest_dep0_dep0_dep1_any_term", t, ""))

   let is_dep0_dep1_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when opname' = opname ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
            ({ bvars = [] }, { bvars = [_] }, { bvars = [_] }) -> true
          | _ -> false
         end
    | _ -> false

   let mk_dep0_dep1_dep1_term opname t0 v1 t1 v2 t2 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_simple_bterm t0;
          mk_bterm [v1] t1;
          mk_bterm [v2] t2]

   let dest_dep0_dep1_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when opname' = opname ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
            ({ bvars = []; bterm = t0 },
             { bvars = [v1]; bterm = t1 },
             { bvars = [v2]; bterm = t2 }) -> t0, v1, t1, v2, t2
          | _ -> raise (TermMatch ("dest_dep0_dep1_dep1_term", t, ""))
         end
    | _ -> raise (TermMatch ("dest_dep0_dep1_dep1_term", t, ""))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep3_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when opname' = opname ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
            ({ bvars = [] }, { bvars = [] }, { bvars = [_; _; _] }) -> true
          | _ -> false
         end
    | _ -> false

   let mk_dep0_dep0_dep3_term opname t0 t1 v1 v2 v3 t2 = 
      mk_term
         { op_name = opname; op_params = [] }
         [mk_simple_bterm t0;
          mk_simple_bterm t1;
          mk_bterm [v1; v2; v3] t2]

   let dest_dep0_dep0_dep3_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when opname' = opname ->
         begin match (dest_bterm bt1, dest_bterm bt2, dest_bterm bt3) with
            ({ bvars = []; bterm = t0 },
             { bvars = []; bterm = t1 },
             { bvars = [v1; v2; v3]; bterm = t2 })
               -> t0, t1, v1, v2, v3, t2
          | _ -> raise (TermMatch ("dest_dep0_dep0_dep3_term", t, ""))
         end
    | _ -> raise (TermMatch ("dest_dep0_dep0_dep3_term", t, ""))

   (*
    * One subterm with opname.
    *)
   let is_one_bsubterm opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [_]
      } when opname' = opname -> true
    | _ -> false

   let dest_one_bsubterm opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bterm]
      } when opname' = opname -> bterm
    | _ -> raise (TermMatch ("dest_one_bsubterm", t, ""))

   let mk_one_bsubterm opname bt =
      { free_vars = bt.bfree_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [bt] }}

end
