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
let is_simple_term_opname name = function
   { term_op = { op_name = name'; op_params = [] };
     term_terms = bterms
   } when name' = name ->
      let rec aux = function
         { bvars = []; bterm = _ }::t -> aux t
       | _::t -> false
       | [] -> true
      in
         aux bterms
 | _ -> false

let mk_any_term op terms =
   let aux t =
      { bvars = []; bterm = t }
   in
      { term_op = op; term_terms = List.map aux terms }
   
let mk_simple_term name terms =
   mk_any_term { op_name = name; op_params = [] } terms

let dest_simple_term = function
   ({ term_op = { op_name = name; op_params = [] };
      term_terms = bterms
    } : term) as t -> 
      let aux = function
         { bvars = []; bterm = t } -> t
       | _ -> raise (TermMatch ("dest_simple_term", t, "binding vars exist"))
      in
         name, List.map aux bterms
 | t -> raise (TermMatch ("dest_simple_term", t, "params exist"))

let dest_simple_term_opname name = function
   ({ term_op = { op_name = name'; op_params = [] };
      term_terms = bterms
    } : term) as t -> 
      if name = name' then
         let aux = function
            { bvars = []; bterm = t } -> t
          | _ -> raise (TermMatch ("dest_simple_term_opname", t, "binding vars exist"))
         in
            List.map aux bterms
      else
         raise (TermMatch ("dest_simple_term_opname", t, "opname mismatch"))
 | t -> raise (TermMatch ("dest_simple_term_opname", t, "params exist"))

(*
 * Terms with one subterm
 *)
let is_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }]
   } -> opname' = opname
 | _ -> false

let mk_dep0_term opname t =
   { term_op = { op_name = opname; op_params = [] };
     term_terms = [{ bvars = []; bterm = t }]
   }

let dest_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t }]
   } when opname' = opname -> t
 | t -> raise (TermMatch ("dest_dep0_term", t, ""))
   
let one_subterm = function
   ({ term_terms = [{ bvars = []; bterm = t }]} : term) -> t
 | t -> raise (TermMatch ("one_subterm", t, ""))

(*
 * Terms with two subterms.
 *)
let is_dep0_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [] }]
   } -> opname' = opname
 | _ -> false

let mk_dep0_dep0_term opname = fun
   t1 t2 ->
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = []; bterm = t2 }]
      }

let dest_dep0_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t1 };
                   { bvars = []; bterm = t2 }]
   } when opname' = opname -> t1, t2
 | t -> raise (TermMatch ("dest_dep0_dep0_term", t, ""))
   
let two_subterms = function
   ({ term_terms = [{ bvars = []; bterm = a };
                    { bvars = []; bterm = b }]} : term) -> a, b
 | t -> raise (TermMatch ("two_subterms", t, ""))

(*
 * Terms with three subterms.
 *)
let is_dep0_dep0_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [] }]
   } -> opname' = opname
 | _ -> false

let mk_dep0_dep0_dep0_term opname = fun
   t1 t2 t3  ->
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = []; bterm = t2 };
                      { bvars = []; bterm = t3 }]
      }

let dest_dep0_dep0_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t1 };
                   { bvars = []; bterm = t2 };
                   { bvars = []; bterm = t3 }]
   } when opname' = opname -> t1, t2, t3
 | t -> raise (TermMatch ("dest_dep0_dep0_dep0_term", t, ""))
   
let three_subterms = function
   { term_terms = [{ bvars = []; bterm = a };
                   { bvars = []; bterm = b };
                   { bvars = []; bterm = c }]} ->
      a, b, c
 | t -> raise (TermMatch ("three_subterms", t, ""))

let four_subterms = function
   { term_terms = [{ bvars = []; bterm = a };
                   { bvars = []; bterm = b };
                   { bvars = []; bterm = c };
                   { bvars = []; bterm = d }]} ->
      a, b, c, d
 | t ->
      raise (TermMatch ("four_subterms", t, ""))

let five_subterms = function
   { term_terms = [{ bvars = []; bterm = a };
                   { bvars = []; bterm = b };
                   { bvars = []; bterm = c };
                   { bvars = []; bterm = d };
                   { bvars = []; bterm = e }]} ->
      a, b, c, d, e
 | t ->
      raise (TermMatch ("four_subterms", t, ""))

(************************************************************************
 * Nonsimple but useful forms                                           *
 ************************************************************************)

(*
 * One string param.
 *)
let is_string_term opname = function
   { term_op = { op_name = opname'; op_params = [String _] };
     term_terms = []
   } when opname == opname' ->
      true
 | _ ->
      false

let dest_string_term opname = function
   { term_op = { op_name = opname'; op_params = [String s] };
     term_terms = []
   } when opname == opname' ->
      s
 | t ->
      raise (TermMatch ("dest_string_term", t, "not a string term"))

let dest_string_param = function
   { term_op = { op_params = String s :: _ } } ->
      s
 | t ->
      raise (TermMatch ("dest_string_param", t, "no string parameter"))

let mk_string_term opname s =
   { term_op = { op_name = opname; op_params = [String s] }; term_terms = [] }

(*
 * One string parameter, and one simple subterm.
 *)
let is_string_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [String _] };
     term_terms = [{ bvars = [] }]
   } when opname = opname' -> true
 | _ -> false

let dest_string_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [String s] };
     term_terms = [{ bvars = []; bterm = t }]
   } when opname = opname' -> s, t
 | t -> raise (TermMatch ("dest_string_dep0_term", t, ""))

let mk_string_dep0_term opname = fun
   s t ->
      { term_op = { op_name = opname; op_params = [String s] };
        term_terms = [{ bvars = []; bterm = t }]
      }

(*
 * One string parameter, and one simple subterm.
 *)
let is_string_string_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [String _; String _] };
     term_terms = [{ bvars = [] }]
   } when opname = opname' ->
      true
 | _ ->
      false

let dest_string_string_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [String s1; String s2] };
     term_terms = [{ bvars = []; bterm = t }]
   } when opname = opname' ->
      s1, s2, t
 | t ->
      raise (TermMatch ("dest_string_string_dep0_term", t, ""))

let dest_string_string_dep0_any_term = function
   { term_op = { op_name = opname'; op_params = [String s1; String s2] };
     term_terms = [{ bvars = []; bterm = t }]
   } ->
      s1, s2, t
 | t ->
      raise (TermMatch ("dest_string_string_dep0_term", t, ""))

let mk_string_string_dep0_term opname = fun
   s1 s2 t ->
      { term_op = { op_name = opname; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t }]
      }

(*
 * Two number parameters and one subterm.
 *)
let is_number_number_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [Number _; Number _] };
     term_terms = [{ bvars = [] }]
   } when opname = opname' ->
      true
 | _ ->
      false

let dest_number_number_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
     term_terms = [{ bvars = []; bterm = t }]
   } when opname = opname' ->
      s1, s2, t
 | t ->
      raise (TermMatch ("dest_number_number_dep0_term", t, ""))

let dest_number_number_dep0_any_term = function
   { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
     term_terms = [{ bvars = []; bterm = t }]
   } ->
      s1, s2, t
 | t ->
      raise (TermMatch ("dest_number_number_dep0_term", t, ""))

let mk_number_number_dep0_term opname = fun
   s1 s2 t ->
      { term_op = { op_name = opname; op_params = [Number s1; Number s2] };
        term_terms = [{ bvars = []; bterm = t }]
      }

(*
 * Two string parameters, two subterms.
 *)
let is_string_string_dep0_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [String _; String _] };
     term_terms = [{ bvars = [] }; { bvars = [] }]
   } when opname = opname' ->
      true
 | _ ->
      false

let dest_string_string_dep0_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [String s1; String s2] };
     term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
   } when opname = opname' ->
      s1, s2, t1, t2
 | t ->
      raise (TermMatch ("dest_string_string_dep0_term", t, ""))

let dest_string_string_dep0_dep0_any_term = function
   { term_op = { op_name = opname'; op_params = [String s1; String s2] };
     term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
   } ->
      s1, s2, t1, t2
 | t ->
      raise (TermMatch ("dest_string_string_dep0_term", t, ""))

let mk_string_string_dep0_dep0_term opname = fun
   s1 s2 t1 t2 ->
      { term_op = { op_name = opname; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      }

(*
 * One number param.
 *)
let is_number_term opname = function
   { term_op = { op_name = opname'; op_params = [Number _] };
     term_terms = []
   } when opname = opname' -> true
 | _ -> false

let dest_number_term opname = function
   { term_op = { op_name = opname'; op_params = [Number n] };
     term_terms = []
   } when opname = opname' -> n
 | t -> raise (TermMatch ("dest_number_term", t, ""))

let dest_number_any_term = function
   { term_op = { op_params = [Number n] };
     term_terms = []
   } ->
      n
 | t ->
      raise (TermMatch ("dest_number_any_term", t, ""))

let mk_number_term opname = function
   n ->
      { term_op = { op_name = opname; op_params = [Number n] };
        term_terms = []
      }

(*
 * One universe param.
 *)
let is_univ_term opname = function
   { term_op = { op_name = opname'; op_params = [Level _] };
     term_terms = []
   } when opname = opname' -> true
 | _ -> false

let dest_univ_term opname = function
   { term_op = { op_name = opname'; op_params = [Level n] };
     term_terms = []
   } when opname = opname' -> n
 | t -> raise (TermMatch ("dest_univ_term", t, ""))

let mk_univ_term opname = function
   n ->
      { term_op = { op_name = opname; op_params = [Level n] };
        term_terms = []
      }

(*
 * One token param.
 *)
let is_token_term opname = function
   { term_op = { op_name = opname'; op_params = [Token _] };
     term_terms = []
   } when opname = opname' -> true
 | _ -> false

let dest_token_term opname = function
   { term_op = { op_name = opname'; op_params = [Token n] };
     term_terms = []
   } when opname = opname' -> n
 | t -> raise (TermMatch ("dest_token_term", t, ""))

let mk_token_term opname = function
   n ->
      { term_op = { op_name = opname; op_params = [Token n] };
        term_terms = []
      }

(*
 * Bound term.
 *)
let is_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [_] }]
   } when opname' = opname -> true
 | _ -> false

let mk_dep1_term opname = fun
   v t -> { term_op = { op_name = opname; op_params = [] };
            term_terms = [{ bvars = [v]; bterm = t }]
          }

let dest_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [v]; bterm = t }]
   } when opname' = opname -> v, t
 | t -> raise (TermMatch ("dest_dep1_term", t, ""))

let is_dep0_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [_] }]
   } when opname' = opname -> true
 | _ -> false

let is_dep0_dep1_any_term = function
   { term_op = { op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [_] }]
   } -> true
 | _ -> false

let mk_dep0_dep1_term opname = fun
   v t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                term_terms = [{ bvars = []; bterm = t1 };
                              { bvars = [v]; bterm = t2 }]
              }

let mk_dep0_dep1_any_term op = fun
   v t1 t2 -> { term_op = op;
                term_terms = [{ bvars = []; bterm = t1 };
                              { bvars = [v]; bterm = t2 }]
              }

let dest_dep0_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t1 };
                   { bvars = [v]; bterm = t2 }]
   } when opname' = opname -> v, t1, t2
 | t -> raise (TermMatch ("dest_dep0_dep1_term", t, ""))

let dest_dep0_dep1_any_term = function
   { term_op = { op_params = [] };
     term_terms = [{ bvars = []; bterm = t1 };
                   { bvars = [v]; bterm = t2 }]
   } -> v, t1, t2
 | t -> raise (TermMatch ("dest_dep0_dep1_term", t, ""))

(*
 * First subterm of arity 2.
 *)
let is_dep2_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [_; _] }; { bvars = [] }]
   } when opname' = opname -> true
 | _ -> false

let mk_dep2_dep0_term opname = fun
   v1 v2 t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                term_terms = [{ bvars = [v1; v2]; bterm = t1 };
                              { bvars = []; bterm = t2 }]
              }

let dest_dep2_dep0_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [v1; v2]; bterm = t1 };
                   { bvars = []; bterm = t2 }]
   } when opname' = opname -> v1, v2, t1, t2
 | t -> raise (TermMatch ("dest_dep2_dep0_term", t, ""))

(*
 * Second subterm of arity 2.
 *)
let is_dep0_dep2_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [_; _] }]
   } when opname' = opname -> true
 | _ -> false

let mk_dep0_dep2_term opname = fun
   v1 v2 t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                term_terms = [{ bvars = []; bterm = t1 };
                              { bvars = [v1; v2]; bterm = t2 }]
              }

let dest_dep0_dep2_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t1 };
                   { bvars = [v1; v2]; bterm = t2 }]
   } when opname' = opname -> v1, v2, t1, t2
 | t -> raise (TermMatch ("dest_dep0_dep2_term", t, ""))

(*
 * Three subterms.
 *)
let is_dep0_dep2_dep2_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [_; _] }; { bvars = [_; _] }]
   } when opname' = opname -> true
 | _ -> false

let mk_dep0_dep2_dep2_term opname = fun
   t0 v11 v12 t1 v21 v22 t2 -> { term_op = { op_name = opname; op_params = [] };
                       term_terms = [{ bvars = []; bterm = t1 };
                                     { bvars = [v11; v12]; bterm = t1 };
                                     { bvars = [v21; v22]; bterm = t2 }]
                     }

let dest_dep0_dep2_dep2_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t0 };
                   { bvars = [v11; v12]; bterm = t1 };
                   { bvars = [v21; v22]; bterm = t2 }]
   } when opname' = opname -> t0, v11, v12, t1, v21, v22, t2
 | t -> raise (TermMatch ("dest_dep0_dep2_dep2_term", t, ""))

(*
 * Three subterms.
 *)
let is_dep0_dep2_dep0_dep2_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [_; _] }; { bvars = [] }; { bvars = [_; _] }]
   } when opname' = opname -> true
 | _ -> false

let mk_dep0_dep2_dep0_dep2_term opname = fun
   t0 v11 v12 t1 base v21 v22 t2 -> { term_op = { op_name = opname; op_params = [] };
                                      term_terms = [{ bvars = []; bterm = t0 };
                                                    { bvars = [v11; v12]; bterm = t1 };
                                                    { bvars = []; bterm = base };
                                                    { bvars = [v21; v22]; bterm = t2 }]
                                    }

let dest_dep0_dep2_dep0_dep2_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t0 };
                   { bvars = [v11; v12]; bterm = t1 };
                   { bvars = []; bterm = base };
                   { bvars = [v21; v22]; bterm = t2 }]
   } when opname' = opname -> t0, v11, v12, t1, base, v21, v22, t2
 | t -> raise (TermMatch ("dest_dep0_dep2_dep0_dep2_term", t, ""))

(*
 * Three subterms.
 *)
let is_dep0_dep0_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_] }]
   } when opname' == opname ->
      true
 | _ ->
      false

let mk_dep0_dep0_dep1_term opname = fun
   t0 t1 v2 t2 -> { term_op = { op_name = opname; op_params = [] };
                       term_terms = [{ bvars = []; bterm = t1 };
                                     { bvars = []; bterm = t1 };
                                     { bvars = [v2]; bterm = t2 }]
                     }

let dest_dep0_dep0_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t0 };
                   { bvars = []; bterm = t1 };
                   { bvars = [v2]; bterm = t2 }]
   } when opname' == opname ->
      t0, t1, v2, t2
 | t ->
      raise (TermMatch ("dest_dep0_dep0_dep1_term opname", t, ""))

let is_dep0_dep0_dep1_any_term = function
   { term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_] }] } ->
      true
 | _ ->
      false

let mk_dep0_dep0_dep1_any_term op = fun
   t0 t1 v2 t2 -> { term_op = op;
                    term_terms = [{ bvars = []; bterm = t1 };
                                  { bvars = []; bterm = t1 };
                                  { bvars = [v2]; bterm = t2 }]
                  }

let dest_dep0_dep0_dep1_any_term = function
   { term_terms = [{ bvars = []; bterm = t0 };
                   { bvars = []; bterm = t1 };
                   { bvars = [v2]; bterm = t2 }]
   } ->
      t0, t1, v2, t2
 | t ->
      raise (TermMatch ("dest_dep0_dep0_dep1_any_term opname", t, ""))

let is_dep0_dep1_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [_] }; { bvars = [_] }]
   } when opname' = opname -> true
 | _ -> false

let mk_dep0_dep1_dep1_term opname = fun
   t0 v1 t1 v2 t2 -> { term_op = { op_name = opname; op_params = [] };
                       term_terms = [{ bvars = []; bterm = t1 };
                                     { bvars = [v1]; bterm = t1 };
                                     { bvars = [v2]; bterm = t2 }]
                     }

let dest_dep0_dep1_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t0 };
                   { bvars = [v1]; bterm = t1 };
                   { bvars = [v2]; bterm = t2 }]
   } when opname' = opname -> t0, v1, t1, v2, t2
 | t -> raise (TermMatch ("dest_dep0_dep1_dep1_term opname", t, ""))

(*
 * Three subterms.
 *)
let is_dep0_dep0_dep3_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_; _; _] }]
   } when opname' = opname -> true
 | _ -> false

let mk_dep0_dep0_dep3_term opname = fun
   t0 t1 v1 v2 v3 t2 -> { term_op = { op_name = opname; op_params = [] };
                          term_terms = [{ bvars = []; bterm = t1 };
                                        { bvars = []; bterm = t1 };
                                        { bvars = [v1; v2; v3]; bterm = t2 }]
                        }

let dest_dep0_dep0_dep3_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t0 };
                   { bvars = []; bterm = t1 };
                   { bvars = [v1; v2; v3]; bterm = t2 }]
   } when opname' = opname -> t0, t1, v1, v2, v3, t2
 | t -> raise (TermMatch ("dest_dep0_dep0_dep3_term", t, ""))

(*
 * One subterm with opname.
 *)
let is_one_bsubterm opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [_]
   } when opname' = opname -> true
 | _ -> false

let dest_one_bsubterm opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [bterm]
   } when opname' = opname -> bterm
 | t -> raise (TermMatch ("dest_one_bsubterm", t, ""))

let mk_one_bsubterm opname = fun
   bterm -> { term_op = { op_name = opname; op_params = [] }; term_terms = [bterm] }

(************************************************************************
 * PRIMITIVE FORMS                                                      *
 ************************************************************************)

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

let rec is_xlist_term = function
   { term_op = { op_name = opname; op_params = [] };
     term_terms = [{ bvars = []; bterm = _ };
                   { bvars = []; bterm = b }]
   } when opname == xcons_opname -> is_xlist_term b
 | { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when opname == xnil_opname -> true
 | _ -> false

let dest_xlist t =
   let rec aux = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b }]
      } when opname == xcons_opname -> a::(aux b)
    | { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when opname == xnil_opname -> []
    | _ -> raise (TermMatch ("dest_xlist", t, "not a list"))
   in
       aux t

let rec mk_xlist_term = function
   h::t ->
      { term_op = { op_name = xcons_opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = h };
                      { bvars = []; bterm = mk_xlist_term t }]
      }
 | [] ->
      xnil_term

(*
 * Strings.
 *)
let string_opname = mk_opname "string" xperv

let is_xstring_term = function
   { term_op = { op_name = opname; op_params = [String _] };
     term_terms = []
   } when opname == string_opname ->
      true
 | _ ->
      false

let dest_xstring = function
   { term_op = { op_name = opname; op_params = [String s] };
     term_terms = []
   } when opname == string_opname ->
      s
 | t ->
      raise (TermMatch ("dest_xstring", t, "not a string"))

let mk_xstring_term s =
   { term_op = { op_name = string_opname; op_params = [String s] };
     term_terms = []
   }

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
let goal_of_sequent = function
   { term_op = { op_name = name; op_params = [] };
     term_terms = { bvars = []; bterm = t }::_
   } when name == sequent_opname ->
      t
 | t -> raise (TermMatch ("goal_of_sequent", t, ""))

let null_concl = mk_simple_term concl_opname []

(*
 * Find the address of the conclusion.
 *)
let concl_addr t =
   let rec aux' i = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = []; bterm = term }]
      } ->
         if opname = concl_opname then
            aux' (i + 1) term
         else
            i
    | _ ->
         i
   in
   let rec aux i = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = [_]; bterm = term }]
      } when opname = hyp_opname ->
         aux (i + 1) term
    | { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = [] }; { bvars = []; bterm = term }]
      } ->
         if opname = concl_opname then
            i, aux' 0 term
         else if opname = hyp_opname then
            aux (i + 1) term
         else
            raise (TermMatch ("concl_addr", t, ""))
    | _ -> raise (TermMatch ("concl_addr", t, ""))
   in
      aux 0 (goal_of_sequent t)
   
(*
 * Fast access to hyp and concl.
 *)
let nth_hyp t i =
   let rec aux i = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }; { bvars = [x]; bterm = term }]
      } when opname == hyp_opname ->
         if i = 0 then
            x, t
         else
            aux (i - 1) term
    | { term_op = { op_name = opname } } when opname == concl_opname ->
         raise Not_found
    | _ -> raise (TermMatch ("nth_hyp", t, ""))
   in
      aux i (goal_of_sequent t)

let nth_concl t i =
   let rec aux i = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = _ }; { bvars = [_]; bterm = term }]
      } when opname == hyp_opname ->
         aux i term
    | { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }; { bvars = []; bterm = term }]
      } when opname == concl_opname ->
         if i = 0 then
            t
         else
            aux (i - 1) term
    | { term_op = { op_name = opname } } when opname == concl_opname ->
         raise Not_found
    | t -> raise (TermMatch ("nth_concl", t, ""))
   in
      aux i (goal_of_sequent t)

(*
 * Count the hyps.
 *)
let num_hyps t =
   let rec aux i = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }; { bvars = [x]; bterm = term }]
      } when opname == hyp_opname ->
         aux (i + 1) term
    | _ ->
         i
   in
      aux 0 (goal_of_sequent t)

(*
 * Collect the vars.
 *)
let declared_vars t =
   let rec aux vars = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }; { bvars = [x]; bterm = term }]
      } when opname == hyp_opname ->
         aux (x::vars) term
    | _ ->
         vars
   in
      aux [] (goal_of_sequent t)

(*
 * Collect the vars.
 *)
let declarations t =
   let rec aux vars = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }; { bvars = [x]; bterm = term }]
      } when opname == hyp_opname ->
         aux ((x, t)::vars) term
    | _ ->
         vars
   in
      aux [] (goal_of_sequent t)

(*
 * Get the number of the hyp with the given var.
 *)
let get_decl_number t v =
   let rec aux i = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }; { bvars = [x]; bterm = term }]
      } when opname == hyp_opname ->
         if x = v then
            i
         else
            aux (i + 1) term
    | _ ->
         raise Not_found
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
         match t with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [{ bvars = []; bterm = _ }; { bvars = [_]; bterm = term }]
            } when opname == hyp_opname ->
               aux (i - 1) term
          | _ -> raise (Invalid_argument "is_free_seq_var")
   in
      aux i (goal_of_sequent t)

(*
 * Generate a list of sequents with replaced goals.
 *)
let rec replace_concl seq goal =
   match seq with
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = v1; bterm = t2 }]
      } when opname == hyp_opname ->
         { term_op = { op_name = hyp_opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 }; { bvars = v1; bterm = replace_concl t2 goal }]
         }
    | _ ->
         goal

let replace_goal seq goal =
   replace_concl (mk_concl_term goal null_concl) seq

