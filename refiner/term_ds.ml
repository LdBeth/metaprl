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
      eprintf "Loading Term_ds%t" eflush

(************************************************************************
 * Sets of strings                                                      *
 ************************************************************************)

module OrderedString =
    struct
      type t = string
      let compare = Pervasives.compare
    end

module StringSet = Set.Make (OrderedString)

(************************************************************************
 * Type definitions                                                     *
 ************************************************************************)

(*
 * Level expression have offsets from level expression
 * vars, plus a constant offset.
 *)
type level_exp_var = { le_var : string; le_offset : int }

type level_exp = { le_const : int; le_vars : level_exp_var list }

(*
 * Parameters have a number of simple types.
 *)
type param =
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

and object_id = param list

(*
 * An operator combines a name with a list of parameters.
 * The order of params is significant.
 *)
type operator = { op_name : opname; op_params : param list }

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
   Term of term_nods |
   Subst of term * term_subst
and term = { free_vars : StringSet.t; mutable core : term_core }
and bound_term_core =
   BTerm of bound_term_nods |
   BSubst of bound_term * term_subst
and bound_term = { bfree_vars : StringSet.t; mutable bcore: bound_term_core }
and term_nods = { term_op : operator; term_terms : bound_term list }
and bound_term_nods = { bvars : string list; bterm : term }

(*
 * Address of a subterm.
 *)
type address =
   Path of int list
 | NthPath of int * bool

exception IncorrectAddress of address * term
exception BadAddressPrefix of address * address
exception BadMatch of term * term

(************************************************************************
 * Term de/constructors                                                 *
 ************************************************************************)

let rec filter f = function 
   [] -> [] |
   h::tl -> 
      if (f h) 
         then h::(filter f tl)
         else filter f tl

let bterms_free_vars = 
   List.fold_left 
      (fun s bt -> StringSet.union s bt.bfree_vars) 
      StringSet.empty

let subst_free_vars = 
   List.fold_left 
      (fun s (v,t) -> StringSet.union s t.free_vars) 
      StringSet.empty

let do_term_subst sub t =
   match filter (fun (v,_) -> StringSet.mem v t.free_vars) sub with
      [] -> t|
      sub' ->
         {free_vars = 
            StringSet.union
               (List.fold_right
                  StringSet.remove
                  (fst (List.split sub'))
                  t.free_vars)
               (subst_free_vars sub');
          core = Subst (t,sub')}

let subst t tl vl = do_term_subst (List.combine vl tl) t

let do_bterm_subst sub bt =
   match filter (fun (v,_) -> StringSet.mem v bt.bfree_vars) sub with
      [] -> bt|
      sub' ->
         {bfree_vars =
            StringSet.union
               (List.fold_right
                  StringSet.remove
                  (fst (List.split sub'))
                  bt.bfree_vars)
               (subst_free_vars sub');
          bcore = BSubst (bt,sub')}

let rec dest_ds_term t = 
   match t.core with
      Term tt -> tt |
      Subst (tt,sub) -> 
         let rec 
            ttt = dest_ds_term tt and
            t4 = 
               {term_op = ttt.term_op; 
                term_terms = List.map (do_bterm_subst sub) ttt.term_terms}
         in
            t.core <- Term t4;
            t4

let mk_term op bterms = { term_op = op; term_terms = bterms }

let mk_op name params = { op_name = name; op_params = params }

let mk_bterm bvars term = { bvars = bvars; bterm = term }

let mk_level_var v i =
   { le_var = v; le_offset = i }

let mk_level i l =
   { le_const = i; le_vars = l }

(*
 * Operator names.
 *)
let opname_of_term = function
   { term_op = { op_name = name } } -> name

(*
 * Get the subterms.
 * None of the subterms should be bound.
 *)
let subterms_of_term t =
   List.map (fun { bterm = t } -> t) (dest_ds_term t).term_terms

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

(*
 * Rewrite
 *)
let xrewrite_op = mk_opname "rewrite" xperv

let is_xrewrite_term = is_dep0_dep0_term xrewrite_op
let mk_xrewrite_term = mk_dep0_dep0_term xrewrite_op
let dest_xrewrite = dest_dep0_dep0_term xrewrite_op

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
                             let f { bvars = vars; bterm = term } =
                                { bvars = vars; bterm = aux term tl }
                             in
                                List_util.replacef_nth i f bterms
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
      { term_op = { op_name = opname; op_params = [Var(v)] }; term_terms = [] } as t when opname == var_opname->
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
     shape_arities : int list
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
    | MNumber _ -> ShapeNumber
    | MString _ -> ShapeString
    | MToken _ -> ShapeToken
    | MLevel _ -> ShapeLevel
    | MVar _ -> ShapeVar
    | _ ->
         raise (Invalid_argument "Term.shape_of_term")
   in
   let bterm_type { bvars = vars } =
      List.length vars
   in
      { shape_opname = name;
        shape_params = List.map param_type params;
        shape_arities = List.map bterm_type bterms
      }

let print_shape out { shape_opname = name; shape_params = params; shape_arities = arities } =
   let print_param param =
      let s =
         match param with
            ShapeNumber ->
               "N"
          | ShapeString ->
               "S"
          | ShapeToken  ->
               "T"
          | ShapeLevel  ->
               "L"
          | ShapeVar    ->
               "V"
      in
         output_string out s
   in
   let rec print_arity out = function
      [i] ->
         fprintf out "%d" i
    | i::t ->
         fprintf out "%d;%a" i print_arity t
    | [] ->
         ()
   in
      output_string out (flat_opname name);
      output_string out "[";
      List.iter print_param params;
      output_string out "]{";
      print_arity out arities;
      output_string out "}"
 
(*
 * ``Special'' terms to be used in reduction rules
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
 
let canon_var_opname = mk_opname "canon_var" xperv

(*
 * See if a term is a "canon_var".
 *)
let is_canon_var_term = function
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == canon_var_opname -> true
 | _ -> false

(*
 * Destructor for a "canon_var".
 *)
let dest_canon_var = function
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == canon_var_opname -> v
  | t -> raise (TermMatch ("dest_canon_var", t, ""))

(*
 * Make a "canon_var".
 *)
let mk_canon_var_term v =
   { term_op = { op_name = canon_var_opname; op_params = [Var v] };
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
   { term_op = { op_name = opname; op_params = [] };
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
   { term_op = { op_name = opname; op_params = [] };
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
   { term_op = { op_name = subst_opname; op_params = [] };
     term_terms = {bvars=vars; bterm=main_term}::terms
   }

let make_subst_term main_term vars trms =
   if List.length vars != List.length trms then raise (Invalid_argument "make_subst_term") else
   let terms = List.map (function term -> {bvars=[]; bterm = term}) trms in
   { term_op = { op_name = subst_opname; op_params = [] };
     term_terms = {bvars=vars; bterm=main_term}::terms
   }

let make_1subst_term main_term v t =
   { term_op = { op_name = subst_opname; op_params = [] };
     term_terms = [ {bvars=[v]; bterm=main_term};{bvars=[]; bterm=t} ]
   }

let make_2subst_term main_term v1 v2 t1 t2 =
   { term_op = { op_name = subst_opname; op_params = [] };
     term_terms = [ {bvars=[v1;v2]; bterm=main_term};{bvars=[]; bterm=t1};{bvars=[]; bterm=t2} ]
   }

