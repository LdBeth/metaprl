(*
 * This is the module that implements simple functions 
 * for manipuating terms with simple shapes
 *)

open Printf
open Debug
open Opname
open Term_ds

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_ds_simple%t" eflush

(************************************************************************
 * Tools for "simple" terms                                             *
 ************************************************************************)

(*
 * "Simple" terms have no parameters and no binding variables.
 *)
let is_simple_term_opname name t = match dest_term t with
   { term_op = { op_name = name'; op_params = [] };
     term_terms = bterms
   } when name' = name -> no_bvars bterms
 | _ -> false

let mk_any_term op terms = mk_term op (List.map mk_simple_bterm terms)
   
let mk_simple_term name terms =
   mk_any_term { op_name = name; op_params = [] } terms

let dest_simple_term t = match dest_term t with
   { term_op = { op_name = name; op_params = [] };
      term_terms = bterms } -> 
         name, dest_simple_bterms t bterms
 | _ -> raise (TermMatch ("dest_simple_term", t, "params exist"))

let dest_simple_term_opname name t = match dest_term t with
   { term_op = { op_name = name'; op_params = [] };
      term_terms = bterms } -> 
      if name = name' then dest_simple_bterms t bterms
      else
         raise (TermMatch ("dest_simple_term_opname", t, "opname mismatch"))
 | _ -> raise (TermMatch ("dest_simple_term_opname", t, "params exist"))

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

let rec is_xlist_term t = match dest_term t with
   { term_op = { op_name = opname; op_params = [] };
     term_terms = [bt1; bt2] 
   } when opname == xcons_opname -> 
      begin match (dest_bterm bt1, dest_bterm bt2) with
         ({ bvars = []; bterm = _ }, { bvars = []; bterm = b }) ->
            is_xlist_term b
       | _ -> false
      end
 | { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when opname == xnil_opname -> true
 | _ -> false

let dest_xlist t =
   let rec aux trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == xcons_opname -> 
         begin match (dest_bterm bt1, dest_bterm bt2) with 
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

let is_xstring_term t = match dest_term t with
   { term_op = { op_name = opname; op_params = [String _] };
     term_terms = []
   } when opname == string_opname ->
      true
 | _ ->
      false

let dest_xstring t = match dest_term t with
   { term_op = { op_name = opname; op_params = [String s] };
     term_terms = []
   } when opname == string_opname ->
      s
 | _ ->
      raise (TermMatch ("dest_xstring", t, "not a string"))

let mk_xstring_term s =
   { free_vars = StringSet.empty;
     core = Term
      { term_op = { op_name = string_opname; op_params = [String s] };
        term_terms = [] }}

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
let goal_of_sequent t = match dest_term t with
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
   let rec aux' i trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == concl_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
            ({ bvars = [] }, { bvars = []; bterm = term }) -> aux' (i + 1) term
          | _ -> i
         end
    | _ ->
         i
   in
   let rec aux i trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == hyp_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
            ({ bvars = [] }, { bvars = [_]; bterm = term }) -> aux (i + 1) term
          | ({ bvars = [] }, { bvars = []; bterm = term }) -> aux (i + 1) term
          | _ -> raise (TermMatch ("concl_addr", t, ""))
         end
    | { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == concl_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
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
   let rec aux i trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == hyp_opname -> 
         begin match (dest_bterm bt1, dest_bterm bt2) with
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
   let rec aux i trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == hyp_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
            ({ bvars = []; bterm = _ }, { bvars = [_]; bterm = term }) ->
               aux i term
          | _ -> raise (TermMatch ("nth_concl", t, ""))
         end
    | { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == concl_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
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
   let rec aux i trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == hyp_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
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
   let rec aux vars trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == hyp_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
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
   let rec aux vars trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == hyp_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
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
   let rec aux i trm = match dest_term trm with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [bt1; bt2]
      } when opname == hyp_opname ->
         begin match (dest_bterm bt1, dest_bterm bt2) with
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
               begin match (dest_bterm bt1, dest_bterm bt2) with
                  ({ bvars = []; bterm = _ }, { bvars = [_]; bterm = term }) -> 
                     aux (i - 1) term
                | _ -> raise (Invalid_argument "is_free_seq_var")
               end
          | _ -> raise (Invalid_argument "is_free_seq_var")
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
         begin match (dest_bterm bt1, dest_bterm bt2) with
            ({ bvars = []; bterm = t1 }, { bvars = v1; bterm = t2 }) ->
               mk_term 
                  { op_name = hyp_opname; op_params = [] }
                  [ mk_simple_bterm t1; 
                    mk_bterm v1 (replace_concl t2 goal) ]
          | _ -> goal
         end
    | _ -> goal

let replace_goal seq goal =
   replace_concl (mk_concl_term goal null_concl) seq


(*
 * Rewrite
 *)
let xrewrite_op = mk_opname "rewrite" xperv

let is_xrewrite_term = is_dep0_dep0_term xrewrite_op
let mk_xrewrite_term = mk_dep0_dep0_term xrewrite_op
let dest_xrewrite = dest_dep0_dep0_term xrewrite_op

