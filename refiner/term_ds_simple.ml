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
 * Native terms are injected into the "perv" module.
 *)
let xperv = make_opname ["Perv"]

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

