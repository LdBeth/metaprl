(*
 * This is the simple term module, where the
 * implementation of the term mirrors the interface.
 * Destructors are identiy functions.
 *
 *)

open Debug
open Opname

(************************************************************************
 * Type definitions                                                     *
 ************************************************************************)

(*
 * Level expression have offsets from level expression
 * vars, plus a constant offset.
 *)
type level_exp_var' = { le_var : string; le_offset : int }

and level_exp' = { le_const : int; le_vars : level_exp_var list }

(*
 * Parameters have a number of simple types.
 *)
and param' =
   Number of int
 | String of string
 | Token of string
 | Level of level_exp
 | Var of string
 | MNumber of string
 | MString of string
 | MToken of string
 | MLevel of string
 | MVar of string
 | ObId of object_id
 | ParmList of param list
   
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

(*
 * An operator combines a name with a list of parameters.
 * The order of params is significant.
 *)
and object_id = param list

and operator' = { op_name : opname; op_params : param list }

(*
 * A term has an operator, and a finite number of subterms
 * that may be bound.
 *)
and term' = { term_op : operator; term_terms : bound_term list }
and bound_term' = { bvars : string list; bterm : term }

(*
 * Level expression have offsets from level expression
 * vars, plus a constant offset.
 *)
and level_exp_var = level_exp_var'
and level_exp = level_exp'
and param = param'
and operator = operator'

and term = term'
and bound_term = bound_term'

type term_subst = (string * term) list

(*
 * General exception for term destruction.
 *)
exception TermMatch of string * term * string

(*
 * An address of a subterm is an int list describing the path.
 * In NthPath, the flag is true if the car should be taken.
 *)
type address =
   Path of int list
 | NthPath of int * bool

exception IncorrectAddress of address * term
exception BadAddressPrefix of address * address
exception BadParamMatch of param * param
exception BadMatch of term * term

(************************************************************************
 * Term de/constructors                                                 *
 ************************************************************************)

(*
 * These are basically identity functions for this implementation.
 *)
let mk_term op bterms = { term_op = op; term_terms = bterms }

let make_term term = term

let dest_term term = term

let mk_op name params = { op_name = name; op_params = params }

let make_op op = op

let dest_op op = op

let mk_bterm bvars term = { bvars = bvars; bterm = term }

let make_bterm bterm = bterm

let dest_bterm bterm = bterm

let make_param param = param

let dest_param param = param

let mk_level_var v i =
   { le_var = v; le_offset = i }

let make_level_var v = v

let dest_level_var v = v

let mk_level i l =
   { le_const = i; le_vars = l }

let make_level l = l

let dest_level l = l



let make_object_id object_id  = object_id 
let dest_object_id object_id  = object_id

 
(*
 * Operator names.
 *)
let opname_of_term = function
   { term_op = { op_name = name } } -> name

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

let mk_simple_term name terms =
   let aux t = { bvars = []; bterm = t }
   in
      { term_op = { op_name = name; op_params = [] };
        term_terms = List.map aux terms
      }

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

(************************************************************************
 * Nonsimple but useful forms                                           *
 ************************************************************************)

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

let mk_dep0_dep1_term opname = fun
   v t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                term_terms = [{ bvars = []; bterm = t1 };
                              { bvars = [v]; bterm = t2 }]
              }

let dest_dep0_dep1_term opname = function
   { term_op = { op_name = opname'; op_params = [] };
     term_terms = [{ bvars = []; bterm = t1 };
                   { bvars = [v]; bterm = t2 }]
   } when opname' = opname -> v, t1, t2
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
                                      term_terms = [{ bvars = []; bterm = t1 };
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
 * ZIP/UNZIP                                                            *
 ************************************************************************)        

(*
 * Unzipping means that we want to recursively destruct a
 * right associative type.
 *)
let unzip_rassoc opname =
   let rec aux = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = a }; { bvars = []; bterm = b }]
      } when opname' = opname ->
         a::(aux b)
    | t -> [t]
   in
      aux

let unzip_lassoc opname =
   let rec aux = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = a }; { bvars = []; bterm = b }]
      } when opname' = opname ->
         (aux a) @ [b]
    | t -> [t]
   in
      aux

let unzip_dep_rassoc opname =
   let rec aux = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = a }; { bvars = [v]; bterm = b }]
      } when opname' = opname ->
         (v, a)::(aux b)
    | t -> ["", t]
   in
      aux

let unzip_dep_lassoc opname =
   let rec aux = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = a }; { bvars = [v]; bterm = b }]
      } when opname' = opname ->
         (aux a) @ [v, b]
    | t -> ["", t]
   in
      aux

let zip_rassoc opname =
   let rec aux = function
      [] -> raise (Invalid_argument "zip_rassoc")
    | [h] -> h
    | h::t ->
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = h }; { bvars = []; bterm = aux t }]
         }
   in
      aux

let zip_lassoc opname t =
   let rec aux t = function
      [] -> t
    | h::tl ->
         let t' = { term_op = { op_name = opname; op_params = [] };
                    term_terms = [{ bvars = []; bterm = t }; { bvars = []; bterm = h }]
                  }
         in
            aux t' tl
   in
      match t with
         [] -> raise (Invalid_argument "zip_lassoc")
       | hd::tl -> aux hd tl
            
let zip_dep_rassoc opname =
   let rec aux = function
      [] -> raise (Invalid_argument "zip_rassoc")
    | [v, h] -> h
    | (v, h)::t ->
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = h }; { bvars = [v]; bterm = aux t }]
         }
   in
      aux

let zip_dep_lassoc opname t =
   let rec aux t = function
      [] -> t
    | (v, h)::tl ->
         let t' = { term_op = { op_name = opname; op_params = [] };
                    term_terms = [{ bvars = []; bterm = t }; { bvars = [v]; bterm = h }]
                  }
         in
            aux t' tl
   in
      match t with
         [] -> raise (Invalid_argument "zip_lassoc")
       | (v, h)::tl -> aux h tl

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
 * Variables                                                            *
 ************************************************************************)

let var_opname = make_opname ["var"]

(*
 * See if a term is a variable.
 *)
let is_var_term = function
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == var_opname -> true
 | _ -> false

(*
 * Destructor for a variable.
 *)
let dest_var = function
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == var_opname -> v
  | t -> raise (TermMatch ("dest_var", t, ""))

(*
 * Make a variable.
 *)
let mk_var_term v =
   { term_op = { op_name = var_opname; op_params = [Var v] };
     term_terms = []
   }

let mk_var_op v = { op_name = var_opname; op_params = [Var v] }

(*
 * Second order variables have subterms.
 *)
let is_so_var_term = function
   ({ term_op = { op_name = opname; op_params = [Var(_)] }; term_terms = bterms } : term)
   when opname == var_opname ->
      List.for_all (function { bvars = [] } -> true | _ -> false) bterms
 | _ -> false

let dest_so_var = function
   ({ term_op = { op_name = opname; op_params = [Var(v)] };
      term_terms = bterms
    } : term) as term when opname == var_opname ->
      v, List.map (function { bvars = []; bterm = t } -> t | _ ->
            raise (TermMatch ("dest_so_var", term, "bvars exist")))
      bterms
 | term -> raise (TermMatch ("dest_so_var", term, "not a so_var"))

(*
 * Second order variable.
 *)
let mk_so_var_term v terms =
   let mk_bterm term =
      { bvars = []; bterm = term }
   in
      { term_op = { op_name = var_opname; op_params = [Var(v)] };
        term_terms = List.map mk_bterm terms
      }

(*
 * Second order context, contains a context term, plus
 * binding variables like so vars.
 *)
let context_opname = make_opname ["context"]

let is_context_term = function
   ({ term_op = { op_name = opname; op_params = [Var _] }; term_terms = bterms } : term)
   when opname == context_opname ->
      List.for_all (function { bvars = [] } -> true | _ -> false) bterms
 | term -> false

let dest_context = function
   ({ term_op = { op_name = opname; op_params = [Var v] };
      term_terms = { bvars = []; bterm = term' }::bterms
    } : term) as term when opname == context_opname ->
      v, term', List.map (function { bvars = []; bterm = t } -> t
       | _ ->
            raise (TermMatch ("dest_context", term, "bvars exist")))
      bterms
 | term -> raise (TermMatch ("dest_context", term, "not a context"))

let mk_context_term v term terms =
   let mk_bterm term =
      { bvars = []; bterm = term }
   in
      { term_op = { op_name = context_opname; op_params = [Var v] };
        term_terms = (mk_bterm term)::(List.map mk_bterm terms)
      }

(************************************************************************
 * PRIMITIVE FORMS                                                      *
 ************************************************************************)

(*
 * Lists.
 *)
let xnil_opname = make_opname ["nil"]
let xcons_opname = make_opname ["cons"]

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
      { term_op = { op_name = xnil_opname; op_params = [] };
        term_terms = []
      }

(*
 * Strings.
 *)
let string_opname = make_opname ["string"]

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

let xlambda_opname = make_opname ["lambda"]

let mk_xlambda_term = mk_dep1_term xlambda_opname

(************************************************************************
 * Subterm addressing                                                   *
 ************************************************************************)

(*
 * Constructor.
 *)
let make_address l = Path l

let make_seq_address i = NthPath (i + 1, true)

let nth_cdr_addr i = NthPath (i, false)

(*
 * Compute arities of subterms.
 *)
let subterm_arities { term_terms = terms } =
   let aux { bvars = vars } = List.length vars in
      List.map aux terms

(*
 * Get a subterm.
 *)
let term_subterm term = function
   (Path addr) as a ->
      begin
         let rec aux t = function
            [] -> t
          | i::tl -> aux (List.nth t.term_terms i).bterm tl
         in
            try aux term addr with
               Not_found -> raise (IncorrectAddress (a, term))
      end
 | (NthPath (addr, flag)) as a ->
      begin
         let rec aux t = function
            0 ->
               if flag then
                  match t with
                     { term_terms = { bterm = h }::_ } -> h
                   | _ -> raise (IncorrectAddress (a, term))
               else
                  t
          | i ->
               begin
                  match t.term_terms with
                     [{ bterm = bterm }] ->
                        aux bterm (i - 1)
                   | _::{ bterm = bterm }::_ ->
                        aux bterm (i - 1)
                   | _ ->
                        raise (IncorrectAddress (a, term))
               end
         in
            aux term addr
      end

let apply_fun_at_addr f a term =
   match a with
      Path addr ->
         begin
            let rec aux t = function
               [] -> f t
             | i::tl ->
                  match t with
                     { term_op = op; term_terms = bterms } ->
                        { term_op = op;
                          term_terms =
                             List_util.replacef_nth bterms i
                             (function { bvars = vars; bterm = term } ->
                                   { bvars = vars; bterm = aux term tl })
                        }
            in
               try aux term addr with
                  Not_found -> raise (IncorrectAddress (a, term))
         end
    | NthPath (addr, flag) ->
         begin
            let rec aux t = function
               0 ->
                  if flag then
                     match t with
                        { term_op = op;
                          term_terms = { bvars = vars; bterm = term }::bterms
                        } ->
                           { term_op = op;
                             term_terms = { bvars = vars; bterm = f term }::bterms
                           }
                      | _ ->
                           raise (IncorrectAddress (a, term))
                  else
                     f t
             | i ->
                  begin
                     match t with
                        { term_op = op; term_terms = [{ bvars = vars; bterm = term }] } ->
                           { term_op = op;
                             term_terms = [{ bvars = vars; bterm = aux term (i - 1) }]
                           }
                      | { term_op = op; term_terms = h::{ bvars = vars; bterm = term }::bterms } ->
                           { term_op = op;
                             term_terms = h::{ bvars = vars; bterm = aux term (i - 1) }::bterms
                           }
                      | _ ->
                           raise (IncorrectAddress (a, term))
                  end
            in
               aux term addr
         end

let replace_subterm term a subterm =
   let aux _ = subterm in
      apply_fun_at_addr aux a term

(*
 * Subtract two addresses.
 * addr1 must be a prefix of addr2, and it is removed from addr2.
 *)
let remove_addr_prefix addr1 addr2 =
   match addr1 with
      NthPath (i, flag1) ->
         begin
            match addr2 with
               NthPath (j, flag2) ->
                  if flag1 then
                     if flag2 & i = j then
                        NthPath (0, false)
                     else
                        raise (BadAddressPrefix (addr1, addr2))
                  else if j >= i then
                     NthPath (j - i - 1, flag2)
                  else
                     raise (BadAddressPrefix (addr1, addr2))

             | Path path ->
                  (*
                   * Check prefix of addr2 is a cdr path
                   * and remove a head for each component of the path.
                   *)
                  let rec aux i' path' =
                     if i' = 0 then
                        if flag1 then
                           match path' with
                              0::path'' -> Path path''
                            | _ -> raise (BadAddressPrefix (addr1, addr2))
                        else
                           Path path'
                     else
                        match path' with
                           1::path'' -> aux (i' - 1) path''
                         | _ -> raise (BadAddressPrefix (addr1, addr2))
                  in
                     aux i path
         end
         
    | Path path1 ->
         begin
            match addr2 with
               NthPath (j, flag) ->
                  (* addr1 must be a cdr path *)
                  let rec aux path' j' =
                     match path' with
                        1::path'' -> aux path'' (j' - 1)
                      | [] -> NthPath (j', flag)
                      | _ -> raise (BadAddressPrefix (addr1, addr2))
                  in
                     aux path1 j
             | Path path2 ->
                  let rec aux path1' path2' =
                     match path1' with
                        x::path1'' ->
                           begin
                              match path2' with
                                 y::path2'' when x = y -> aux path1'' path2''
                               | _ -> raise (BadAddressPrefix (addr1, addr2))
                           end
                      | [] ->
                           Path path2'
                  in
                     aux path1 path2
         end

(*
 * Print a string.
 *)
let string_of_address = function
   Path addr ->
      let rec aux = function
         [] -> ""
       | [h] -> string_of_int h
       | h::t -> (string_of_int h) ^ "; " ^ (aux t)
      in
         "[" ^ (aux addr) ^ "]"
 | NthPath (addr, flag) ->
      let rec aux = function
         0 -> if flag then "1" else "0"
       | i -> "2; " ^ (aux i)
      in
         "[" ^ (aux addr) ^ "]"

(************************************************************************
 * Alpha renaming                                                       *
 ************************************************************************)

(*
 * Standardize apart a term by renaming all the
 * binding variables.
 *)
let rec standardize_apart_term i bvars gvars = function
   { term_op = { op_name = opname; op_params = [Var v] }; term_terms = bterms } when opname == var_opname ->
      (* Variable *)
      let i', v', gvars' =
         if List.mem_assoc v bvars then
            i, List.assoc v bvars, gvars
         else if List.mem_assoc v gvars then
            i, List.assoc v gvars, gvars
         else
            i + 1, i + 1, (v, i + 1)::gvars
      in
         let i2, gvars2, bterms2 = standardize_apart_bterms i' bvars gvars' bterms
         in
            i2,
            gvars2,
            { term_op = { op_name = var_opname; op_params = [Var (new_bvar v')] };
              term_terms = bterms2
            }
 | { term_op = { op_name = name; op_params = params }; term_terms = bterms } ->
      let i2, gvars2, params2 = standardize_apart_params i gvars params in
      let i3, gvars3, bterms2 = standardize_apart_bterms i2 bvars gvars2 bterms
      in
         i3,
         gvars3,
         { term_op = { op_name = name; op_params = params2 };
           term_terms = bterms2
         }

and standardize_apart_param i gvars = function
   (*
    * Parameters are standardized if they are meta.
    *)
   MNumber(v) ->
      if List.mem_assoc v gvars then
         i, gvars, MNumber(new_bvar (List.assoc v gvars))
      else
         i + 1, (v, i + 1)::gvars, MNumber(new_bvar (i + 1))
 | MString(v) ->
      if List.mem_assoc v gvars then
         i, gvars, MString(new_bvar (List.assoc v gvars))
      else
         i + 1, (v, i + 1)::gvars, MString(new_bvar (i + 1))
 | MToken(v) ->
      if List.mem_assoc v gvars then
         i, gvars, MToken(new_bvar (List.assoc v gvars))
      else
         i + 1, (v, i + 1)::gvars, MToken(new_bvar (i + 1))
 | MLevel(v) ->
      if List.mem_assoc v gvars then
         i, gvars, MLevel(new_bvar (List.assoc v gvars))
      else
         i + 1, (v, i + 1)::gvars, MLevel(new_bvar (i + 1))
 | MVar(v) ->
      if List.mem_assoc v gvars then
         i, gvars, MVar(new_bvar (List.assoc v gvars))
      else
         i + 1, (v, i + 1)::gvars, MVar(new_bvar (i + 1))
 | p ->
      i, gvars, p

and standardize_apart_bterm i bvars gvars = function
   (*
    * When a bterm is standardized, the binding variables are added
    * to the list of bvars.
    *)
   { bvars = vars; bterm = term } ->
      (* Add bvars to binders *)
      let rec blist j = function
         [] -> bvars
       | v::l -> (v, j)::(blist (j + 1) l)
      and newvlist j = function
         [] -> []
       | v::l -> (new_bvar j)::(newvlist (j + 1) l)
      in
      let bvars' = blist i vars in
      let vars' = newvlist i vars in
      let i', gvars', t' =
         standardize_apart_term (i + (List.length vars)) bvars' gvars term
      in
         i', gvars', { bvars = vars'; bterm = t' }

and standardize_apart_params i gvars = function
   (* Accumulat on a list *)
   [] -> i, gvars, []
 | p::l ->
      let i', gvars', p' = standardize_apart_param i gvars p in
      let i'', gvars'', p'' = standardize_apart_params i' gvars' l in
         i'', gvars'', p'::p''

and standardize_apart_bterms i bvars gvars = function
   (* Accumulate on a list *)
   [] -> i, gvars, []
 | bterm::l ->
      let i', gvars', bterm' = standardize_apart_bterm i bvars gvars bterm in
      let i'', gvars'', bterms'' = standardize_apart_bterms i' bvars gvars' l in
         i'', gvars'', bterm'::bterms''

and new_bvar i = "v" ^ (string_of_int i)

(*
 * This is the final call for a term.
 *)
let standardize_apart t =
   let _, _, t' = standardize_apart_term 0 [] [] t
   in
      t'

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
 * Comparisons                                                          *
 ************************************************************************)

(*
 * Arbitrary comparison on level expressions.
 *)
let compare_level_exps = fun
   { le_const = c1; le_vars = vars1 }
   { le_const = c2; le_vars = vars2 } ->
      if c1 = c2 then
         let compare_var = fun
            { le_var = v1; le_offset = o1 }
            { le_var = v2; le_offset = o2 } ->
               if v1 = v2 then
                  o1 - o2
               else if v1 > v2 then
                  1
               else
                  -1
         in
            List_util.compare_lists compare_var vars1 vars2
      else
         c1 - c2
         
(*
 * This is an arbitrary comparison on parameters.
 *)
let param_index = function
   Number _ -> 0
 | String _ -> 1
 | Token _ -> 2
 | Var _ -> 3
 | Level _ -> 4
 | MNumber _ -> 5
 | MString _ -> 6
 | MToken _ -> 7
 | MVar _ -> 8
 | MLevel _ -> 9
 | MSum _ -> 10
 | MDiff _ -> 11
 | MProduct _ -> 12
 | MQuotient _ -> 13
 | MRem _ -> 14
 | MLessThan _ -> 15
 | MEqual _ -> 16
 | MNotEqual _ -> 17

let rec compare_param_values = function
   Number i, Number j -> i - j
 | String s, String t -> compare s t
 | Token s, Token t -> compare s t
 | Var s, Var t -> compare s t
 | Level i, Level j -> compare_level_exps i j
 | MNumber s, MNumber t -> compare s t
 | MString s, MString t -> compare s t
 | MToken s, MToken t -> compare s t
 | MVar s, MVar t -> compare s t
 | MLevel s, MLevel t -> compare s t
 | MSum (a1, b1), MSum (a2, b2) ->
      let i = compare_params a1 a2 in
         if i = 0 then
            compare_params b1 b2
         else
            i
 | MDiff (a1, b1), MDiff (a2, b2) ->
      let i = compare_params a1 a2 in
         if i = 0 then
            compare_params b1 b2
         else
            i
 | MProduct (a1, b1), MProduct (a2, b2) ->
      let i = compare_params a1 a2 in
         if i = 0 then
            compare_params b1 b2
         else
            i
 | MQuotient (a1, b1), MQuotient (a2, b2) ->
      let i = compare_params a1 a2 in
         if i = 0 then
            compare_params b1 b2
         else
            i
 | MRem (a1, b1), MRem (a2, b2) ->
      let i = compare_params a1 a2 in
         if i = 0 then
            compare_params b1 b2
         else
            i
 | p, q -> raise (BadParamMatch (p, q))

and compare_params p1 p2 =
   let i = param_index p1
   and j = param_index p2
   in
      if i = j then
         compare_param_values (p1, p2)
      else
         i - j

(*
 * In some cases, we use the params for matching and we don't really
 * care what the variable names are.  Here is a coarser comparison.
 *)
let param_pattern_index = function
   Number _ -> 0
 | String _ -> 1
 | Token _ -> 2
 | Var _ -> 3
 | Level _ -> 4
 | MNumber _ -> 0
 | MString _ -> 1
 | MToken _ -> 2
 | MVar _ -> 3
 | MLevel _ -> 4
 | MSum _ -> 0
 | MDiff _ -> 0
 | MProduct _ -> 0
 | MQuotient _ -> 0
 | MRem _ -> 0
 | MLessThan _ -> 0
 | MEqual _ -> 0
 | MNotEqual _ -> 0

let rec compare_pattern_param_values = function
   Number i, Number j ->     i - j
 | String s, String t ->     compare s t
 | Token s, Token t ->       compare s t
 | Var s, Var t ->           compare s t
 | Level i, Level j ->       compare_level_exps i j
 | _ -> 0

and compare_pattern_params p1 p2 =
   let i = param_pattern_index p1
   and j = param_pattern_index p2
   in
      if i = j then
         compare_pattern_param_values (p1, p2)
      else
         i - j

(*
 * Add name comparisons.
 *)
let compare_operators = fun
   { op_name = name1; op_params = params1 }
   { op_name = name2; op_params = params2 } ->
      if name1 = name2 then
         List_util.compare_lists compare_params params1 params2
      else if name1 < name2 then
         -1
      else
         1

let compare_pattern_operators = fun
   { op_name = name1; op_params = params1 }
   { op_name = name2; op_params = params2 } ->
      if name1 = name2 then
         List_util.compare_lists compare_pattern_params params1 params2
      else if name1 < name2 then
         -1
      else
         1

(************************************************************************
 * ALPHA EQUALITY                                                       *
 ************************************************************************)

(*
 * Recursive computation of alpha equality.
 *)
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
         name1 = name2 & params1 = params2 & equal_bterms vars bterms1 bterms2

and equal_bterms vars bterms1 bterms2 =
   let equal_bterm = fun
      { bvars = bvars1; bterm = term1 }
      { bvars = bvars2; bterm = term2 } ->
         equal_term (List_util.zip_list vars bvars1 bvars2) term1 term2
   in
      List.for_all2 equal_bterm bterms1 bterms2

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
         List.for_all2 equal_comp_bterm bterms1 bterms2
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
      { term_op = { op_name = opname; op_params = [Var(v)] }; term_terms = [] } as t when opname == var_opname->
         (* Var case *)
         begin
             try let i = List_util.find_index vars v in List.nth terms i with
                Not_found -> t
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
            let vars' = List_util.remove_elements vars flags in
            let fv' = List_util.remove_elements fv flags in
            let terms' = List_util.remove_elements terms flags in

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
         try (let i = List_util.find_index renames v in List.nth renames' i) with
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
            v
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

(*************************
 * Sequents              *                                              *
 *************************)

(* Sequents operator name *)
let hyp_opname = make_opname ["hyp"]
let concl_opname = make_opname ["concl"]
let sequent_opname = make_opname ["sequent"]

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
let mk_seq_subgoals seq subgoals =
   let replace_concl goal =
      let rec aux = function
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 }; { bvars = v1; bterm = t2 }]
         } when opname == hyp_opname ->
            { term_op = { op_name = hyp_opname; op_params = [] };
              term_terms = [{ bvars = []; bterm = t1 }; { bvars = v1; bterm = aux t2 }]
            }
       | _ -> goal
      in
         aux
   in
   let replace_goal goal =
      replace_concl (mk_concl_term goal null_concl) seq
   in
      List.map replace_goal subgoals

(************************************************************************
 * NORMALIZATION                                                        *
 ************************************************************************)

(*
 * "Normalization" means producing a canonical version of the term,
 * not reduction.  Right now, this just means rehashing the opname.
 *)
let rec normalize_term = function
   { term_op = { op_name = name; op_params = params }; term_terms = bterms } ->
      { term_op = { op_name = normalize_opname name;
                    op_params = params
                  };
        term_terms = List.map normalize_bterm bterms
      }
      
and normalize_bterm = function
   { bvars = vars; bterm = t } ->
      { bvars = vars; bterm = normalize_term t }

(************************************************************************
 * EFFICIENCY                                                           *
 ************************************************************************)

(*
 * Compute the "shape" of the term that can be used for reductions.
 * Terms are reduced to these templates for indexing
 * purposes.  Each template just contains information
 * about the opname, the order and types of params,
 * and the arties of the subterms.
 *)
type shape =
   { shape_opname : opname;
     shape_params : shape_param list;
     shape_arities : (int * opname) list
   }

and shape_param =
   ShapeNumber
 | ShapeString
 | ShapeToken
 | ShapeLevel
 | ShapeVar

(*
 * When computing the shape, we don't allow meta-parameters.
 * Raises Invalid_argument if this happens.
 *)
let shape_of_term { term_op = { op_name = name; op_params = params }; term_terms = bterms } =
   let param_type = function
      Number _ -> ShapeNumber
    | String _ -> ShapeString
    | Token _ -> ShapeToken
    | Level _ -> ShapeLevel
    | Var _ -> ShapeVar
    | _ ->
         raise (Invalid_argument "Term.shape_of_term")
   in
   let bterm_type { bvars = vars; bterm = { term_op = { op_name = op } } } =
      List.length vars, op
   in
      { shape_opname = name;
        shape_params = List.map param_type params;
        shape_arities = List.map bterm_type bterms
      }

(*
 * $Log$
 * Revision 1.3  1997/08/07 19:08:18  lolorigo
 * added ObId and ParmList parameter types
 *
; Revision 1.2  1997/08/06  16:18:14  jyh
; This is an ocaml version with subtyping, type inference,
; d and eqcd tactics.  It is a basic system, but not debugged.
;
 * Revision 1.1  1997/04/28 15:51:42  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.25  1996/11/13 22:58:10  jyh
 * Initial version of forward/backward chaining cache.
 *
 * Revision 1.24  1996/10/23 15:17:58  jyh
 * First working version of dT tactic.
 *
 * Revision 1.23  1996/09/25 22:52:03  jyh
 * Initial "tactical" commit.
 *
 * Revision 1.22  1996/09/02 19:43:26  jyh
 * Semi working package management.
 *
 * Revision 1.21  1996/05/21 02:14:21  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.20  1996/04/11 13:29:46  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.19  1996/04/07 18:24:55  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.18  1996/03/30 01:37:56  jyh
 * Initial version of ITT.
 *
 * Revision 1.17  1996/03/28 02:58:26  jyh
 * Prelim checkin for an partial version of the refiner document in the
 * first version of README.tex.
 *
 * Revision 1.16  1996/03/25 20:51:02  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.15  1996/03/11 18:34:44  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 * Revision 1.14  1996/03/08 15:41:01  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.13  1996/03/05 19:48:45  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.12  1996/02/25 15:16:24  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.11  1996/02/19 18:47:13  jyh
 * Updating format.prl
 *
 * Revision 1.10  1996/02/18 23:32:38  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.9  1996/02/14 03:51:57  jyh
 * This is a version common to Caml-Light and Caml-Special-Light.
 *
 * Revision 1.8  1996/02/13 21:33:13  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.7  1996/02/10 20:20:03  jyh
 * Initial checkin of filter (prlcomp).
 *
 * Revision 1.6  1996/02/07 23:41:52  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.5  1996/02/07 17:34:14  jyh
 * This is Version 0 of the refiner in Caml-Light.  At this point,
 * Caml-Light becomes a branch, and main development will be
 * in Caml-Special-Light.
 *
 * Revision 1.4  1996/02/05 18:15:14  jyh
 * Merge context rewrites onto the main branch.
 *
 * Revision 1.3.4.1  1996/02/05 06:09:58  jyh
 * This version has the rewriter with contexts, and Rule application
 * in Sequent.ml, but it is not fully debugged.
 *
 * Revision 1.3  1996/01/31 20:02:59  jyh
 * Generalizing rewriter to work on Sequents.
 *
 * Revision 1.2  1996/01/26 20:15:17  jyh
 * This version has a complete rewriter using the simple term structure.
 * Next implement sequents and refinement.
 *
 * Revision 1.1  1995/12/06 16:43:23  jyh
 * This is an ML version of a term rewriting system.
 * This checkin is partial, and provides a rewriter on
 * regular terms.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
