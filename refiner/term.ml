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
type level_exp_var' = { le_var : string; le_offset : int }
type level_exp_var = level_exp_var'

type level_exp'= { le_const : int; le_vars : level_exp_var list }
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
and param = param'

(*
 * An operator combines a name with a list of parameters.
 * The order of params is significant.
 *)
type operator' = { op_name : opname; op_params : param list }
type operator = operator'

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
   Term of term' |
   Subst of term * term_subst
and term = { free_vars : StringSet.t; mutable core : term_core }
and bound_term_core =
   BTerm of bound_term' |
   BSubst of bound_term * term_subst
and bound_term = { bfree_vars : StringSet.t; mutable bcore: bound_term_core }
and term' = { term_op : operator; term_terms : bound_term list }
and bound_term' = { bvars : string list; bterm : term }

(*
 * Address of a subterm.
 *)
type address =
   Path of int list
 | NthPath of int * bool

exception IncorrectAddress of address * term
exception BadAddressPrefix of address * address
exception BadMatch of term * term
exception TermMatch of string * term * string

(************************************************************************
 * Free variables, substitution                                         *
 ************************************************************************)

let bterms_free_vars = 
   List.fold_left 
      (fun s bt -> StringSet.union s bt.bfree_vars) 
      StringSet.empty

let subst_free_vars = 
   List.fold_left 
      (fun s (v,t) -> StringSet.union s t.free_vars) 
      StringSet.empty

let do_term_subst sub t =
   match List_util.filter (fun (v,_) -> StringSet.mem v t.free_vars) sub with
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
   match List_util.filter (fun (v,_) -> StringSet.mem v bt.bfree_vars) sub with
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

let is_free_var v t = StringSet.mem v t.free_vars

let free_vars t = StringSet.elements t.free_vars

let free_vars_terms = function
   [] -> [] |
   hd::tl ->
      StringSet.elements 
         (List.fold_left 
            (function vars -> function trm -> StringSet.union vars trm.free_vars)
            hd.free_vars tl)

(************************************************************************
 * Variables in pure terms                                              *
 ************************************************************************)

let var_opname = make_opname ["var"]

(*
 * See if a term is a variable.
 *)
let is_var_term_nods = function
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == var_opname -> true
 | _ -> false

(*
 * Destructor for a variable.
 *)
let dest_var_nods = function
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == var_opname -> v
  | t -> raise (TermMatch ("dest_var", {free_vars = bterms_free_vars t.term_terms; core = Term t}, ""))

(************************************************************************
 * Term de/constructors                                                 *
 ************************************************************************)

let rec dest_term t = 
   match t.core with
      Term tt -> tt |
      Subst (tt,sub) -> 
         let ttt = dest_term tt in
         let t4 = 
            try dest_term (List.assoc (dest_var_nods ttt) sub)
            with _ -> 
               {term_op = ttt.term_op; 
                term_terms = List.map (do_bterm_subst sub) ttt.term_terms}
         in
            t.core <- Term t4;
            t4

let var_subst t t2 v = 
   if StringSet.mem v t.free_vars 
      then 
         { free_vars = StringSet.union t2.free_vars (StringSet.remove v t.free_vars);
           core = Subst (t,[(v,t2)])}
      else t

(*
 * Make a variable.
 *)
let mk_var_term v =
   { free_vars = StringSet.add v StringSet.empty;
     core = Term
      { term_op = { op_name = var_opname; op_params = [Var v] };
        term_terms = [] }}

let make_term t = 
   try mk_var_term (dest_var_nods t)
   with _ ->
      {free_vars = bterms_free_vars t.term_terms;
       core = Term t}

let mk_term op bterms = 
   {free_vars = bterms_free_vars bterms;
    core = Term { term_op = op; term_terms = bterms }}

let mk_op name params = { op_name = name; op_params = params }


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
   [] -> ([],[]) |
   v::vt ->
      match (new_vars av vt) with
         (vs,ts) -> 
            let v' = new_var av v 0 in
            ((v,v')::vs, (v,mk_var_term v')::ts)

let rec dest_bterm bt = 
   match bt.bcore with
      BTerm tt -> tt |
      BSubst (tt,sub) -> 
         let ttt = dest_bterm tt in
         let t4 =
            match ttt.bvars with
               [] -> { bvars = []; bterm = do_term_subst sub ttt.bterm } |
               bvrs -> 
                  let sub_fvars = subst_free_vars sub in
                  let capt_vars = List_util.filter (function v -> StringSet.mem v sub_fvars) bvrs in
                  match capt_vars with
            (* 
             * Unefficiency : in [] case do_term_subst will go collecting
             * free variables of sub to get the
             * list of free variables of bterm, but it may be more efficient to take 
             * union of bt.free_vars and (bvrs intersect ttt.bterm.free_vars)
             * 
             * Similar inefficiency in the "captured" case.
             *)
                     [] -> { bvars = bvrs; bterm = do_term_subst sub ttt.bterm } |
                     captured -> 
                        let avoidvars = StringSet.union sub_fvars ttt.bterm.free_vars in
                        let (vs,ts) = new_vars avoidvars captured in
                        { bvars = 
                           List.map 
                              (function v ->
                                 try List.assoc v vs
                                 with Not_found -> v)
                              bvrs;
                          bterm = 
                           do_term_subst sub (do_term_subst ts ttt.bterm)}
         in
            bt.bcore <- BTerm t4;
            t4

let no_bvars =
   List.for_all
      (function bt ->
        match dest_bterm bt with
           { bvars = [] } -> true |
           _ -> false)

let dest_simple_bterm term bt =
   match dest_bterm bt with
      { bvars = []; bterm = tt } -> tt |
      _ -> raise (TermMatch ("dest_simple_bterm", term, "bvars exist"))

let dest_simple_bterms term = 
   List.map (function bt -> dest_simple_bterm term bt)

let mk_simple_bterm term =
   { bfree_vars = term.free_vars; 
     bcore = BTerm { bvars = []; bterm = term }}

let mk_level_var v i =
   { le_var = v; le_offset = i }

let mk_level i l =
   { le_const = i; le_vars = l }

let subterms_of_term t =
   List.map (fun bt -> (dest_bterm bt).bterm) (dest_term t).term_terms

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

(*
 * Operator names.
 *)
let opname_of_term t = (dest_term t).term_op.op_name

(*
 * Trivial ones
 *)

let dest_object_id o = o
let make_object_id o = o
let dest_level_var v = v
let make_level_var v = v
let dest_level l = l
let make_level l = l
let dest_param p = p
let make_param p = p
let dest_op o = o
let make_op o = o

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

(*
 * See if a term is a variable.
 *)
let is_var_term t = is_var_term_nods (dest_term t)

(*
 * Destructor for a variable.
 *)
let dest_var t = dest_var_nods (dest_term t)

let mk_var_op v = { op_name = var_opname; op_params = [Var v] }

(*
 * Second order variables have subterms.
 *)
let is_so_var_term t = match dest_term t with
   { term_op = { op_name = opname; op_params = [Var(_)] }; term_terms = bterms }
   when opname == var_opname -> no_bvars bterms
 | _ -> false

let dest_so_var t = match dest_term t with
   { term_op = { op_name = opname; op_params = [Var(v)] };
      term_terms = bterms
   } when opname == var_opname ->
      v, dest_simple_bterms t bterms
 | term -> raise (TermMatch ("dest_so_var", t, "not a so_var"))

(*
 * Second order variable.
 *)
let mk_so_var_term v terms =
   make_term 
      { term_op = { op_name = var_opname; op_params = [Var(v)] };
        term_terms = List.map mk_simple_bterm terms }

(*
 * Second order context, contains a context term, plus
 * binding variables like so vars.
 *)
let context_opname = make_opname ["context"]

let is_context_term t = match dest_term t with
   { term_op = { op_name = opname; op_params = [Var _] }; term_terms = bterms }
   when opname == context_opname -> no_bvars bterms
 | term -> false

let dest_context term = match dest_term term with
   { term_op = { op_name = opname; op_params = [Var v] };
      term_terms = bterm :: bterms
    } when opname == context_opname ->
      v, dest_simple_bterm term bterm, 
         dest_simple_bterms term bterms
 | _ -> raise (TermMatch ("dest_context", term, "not a context"))

let mk_context_term v term terms =
   make_term 
      { term_op = { op_name = context_opname; op_params = [Var v] };
        term_terms = (mk_simple_bterm term)::(List.map mk_simple_bterm terms) }

let rec context_vars_term cvars t = match dest_term t with
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
   bt::l ->
      context_vars_bterms (context_vars_term cvars (dest_bterm bt).bterm) l
 | [] -> cvars

let context_vars = context_vars_term []

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
let subterm_arities term =
   let aux bterm = List.length (dest_bterm bterm).bvars in
      List.map aux (dest_term term).term_terms

(*
 * Get a subterm.
 *)
let term_subterm term = 
   function
      (Path addr) as a ->
         begin
            let rec aux t = function
               [] -> t
             | i::tl -> aux (dest_bterm (List.nth (dest_term t).term_terms i)).bterm tl
            in
               try aux term addr with
                  Not_found -> raise (IncorrectAddress (a, term))
         end
    | (NthPath (addr, flag)) as a ->
         begin
            let rec aux t = function
               0 ->
                  if flag then
                     match dest_term t with
                        { term_terms = bterm::_ } -> (dest_bterm bterm).bterm
                      | _ -> raise (IncorrectAddress (a, term))
                  else
                     t
             | i ->
                  begin
                     match (dest_term t).term_terms with
                        [bterm] ->
                           aux (dest_bterm bterm).bterm (i - 1)
                      | _::bterm::_ ->
                           aux (dest_bterm bterm).bterm (i - 1)
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
                  match dest_term t with
                     { term_op = op; term_terms = bterms } ->
                        let f bt =
                           let bterm = dest_bterm bt in
                           mk_bterm bterm.bvars (aux bterm.bterm tl)
                        in
                        mk_term op (List_util.replacef_nth i f bterms)
            in
               try aux term addr with
                  Not_found -> raise (IncorrectAddress (a, term))
         end
    | NthPath (addr, flag) ->
         begin
            let rec aux t = function
               0 ->
                  if flag then
                     match dest_term t with
                        { term_op = op;
                          term_terms = btrm::bterms
                        } ->
                           let bt = dest_bterm btrm in
                           mk_term op ((mk_bterm bt.bvars (f bt.bterm))::bterms)
                      | _ ->
                           raise (IncorrectAddress (a, term))
                  else
                     f t
             | i ->
                  begin
                     match dest_term t with
                        { term_op = op; term_terms = [btrm] } ->
                           let bt = dest_bterm btrm in
                           mk_term op [mk_bterm bt.bvars (aux bt.bterm (pred i))]
                      | { term_op = op; term_terms = h::btrm::bterms } ->
                           let bt = dest_bterm btrm in
                           mk_term op (h::(mk_bterm bt.bvars (aux bt.bterm (pred i)))::bterms)
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

let rec join_vars vars = function
   ([],[]) -> vars |
   (v1::vt1,v2::vt2) -> 
      if (v1=v2) 
         then join_vars vars (vt1,vt2)
         else (v1,v2)::(join_vars vars (vt1,vt2)) |
   _ -> raise (Invalid_argument ("join_vars"))

let rec equal_term_main vars t t' =
   match (dest_term t, dest_term t') with
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
         name1 == name2
                 & List_util.for_all2 equal_params params1 params2
                 & equal_bterms vars bterms1 bterms2

and equal_term = function
   [] ->
      (function t1  ->
         function t2 ->
            (t1 == t2) || equal_term_main [] t1 t2) |
   vars -> equal_term_main vars

and equal_bterm_main vars btrm1 btrm2 =
   let bt1 = dest_bterm btrm1 in
   let bt2 = dest_bterm btrm2 in
   equal_term (join_vars vars (bt1.bvars,bt2.bvars)) bt1.bterm bt2.bterm

and equal_bterm = function
   [] -> 
      (function bt1 ->
         function bt2 ->
            (bt1 == bt2) || equal_bterm_main [] bt1 bt2) |
   vars -> equal_bterm_main vars 

and equal_bterms vars = List_util.for_all2 (equal_bterm vars)

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
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == var_opname ->
      (try equal_term vars' t' (List.assoc v vars) with
         Not_found ->
            begin
               match dest_term t' with
                  { term_op = { op_name = opname; op_params = [Var v'] };
                    term_terms = []
                  } when opname == var_opname -> v = v'
                | _ -> false
            end)
 | { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 } ->
      (function
         { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
         name1 = name2 & params1 = params2 & equal_comp_bterms vars' vars bterms1 bterms2)
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
   v1::t1, v2::t2 -> 
      if (v1=v2) 
         then zip_cons l (t1,t2)
         else zip_cons ((v1, v2)::l) (t1, t2)
 | [], [] -> l
 | _ -> raise (Invalid_argument "zip_cons")

(*
 * Unify two terms.
 *)
let rec unify_terms subst bvars tm1 tm2 = match (dest_term tm1, dest_term tm2) with
   ({ term_op = { op_name = opname; op_params = [Var v] };
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
          | TermMatch _ ->
               raise (BadMatch (tm1, tm2))
      end
         
 | t1, ({ term_op = { op_name = opname; op_params = [Var v] };
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
          | TermMatch _ ->
               raise (BadMatch (tm1, tm2))
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
   mk_term 
      { op_name = normalize_opname op.op_name; op_params = op.op_params }
      (List.map normalize_bterm t.term_terms)
      
and normalize_bterm btrm =
   let bt = dest_bterm btrm in
   mk_bterm bt.bvars (normalize_term bt.bterm)

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
let shape_of_term trm =
   let t = dest_term trm in
   let op = t.term_op in
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
   let bterm_type bt =
      List.length (dest_bterm bt).bvars
   in
      { shape_opname = op.op_name;
        shape_params = List.map param_type op.op_params;
        shape_arities = List.map bterm_type t.term_terms
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
 * This is the module that implements simple functions 
 * for manipuating terms with simple shapes
 *)

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
let is_canon_var_term t = match dest_term t with
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == canon_var_opname -> true
 | _ -> false

(*
 * Destructor for a "canon_var".
 *)
let dest_canon_var t = match dest_term t with
   { term_op = { op_name = opname; op_params = [Var v] };
     term_terms = []
   } when opname == canon_var_opname -> v
  | _ -> raise (TermMatch ("dest_canon_var", t, ""))

(*
 * Make a "canon_var".
 *)
let mk_canon_var_term v =
   { free_vars = StringSet.add v StringSet.empty;
     core = Term
      { term_op = { op_name = canon_var_opname; op_params = [Var v] };
        term_terms = []}}
   
let subst_opname = mk_opname "subst" xperv

(*
 * See if a term is a "subst" term.
 *)

let rec is_subst_term_args = function
   [],[] -> true
 | var::vars,bt::bterms -> 
      (dest_bterm bt).bvars = [] && is_subst_term_args (vars,bterms) 
 | _ -> false

let is_subst_term t = match dest_term t with
   { term_op = { op_name = opname; op_params = [] };
     term_terms = bt::bterms
   } when opname == subst_opname -> 
      is_subst_term_args ((dest_bterm bt).bvars,bterms)
 | _ -> false

(*
 * Destructor for a "subst" term.
 *)

let rec dest_subst_args t = function
   [],[] -> [],[]
 | var::vars,bt::bterms -> 
      let tail = dest_subst_args t (vars,bterms) in
      (var::fst tail),((dest_simple_bterm t bt)::snd tail)
 | _ -> raise (TermMatch ("dest_subst", t, ""))

let dest_subst t = match dest_term t with
   { term_op = { op_name = opname; op_params = [] };
     term_terms = btrm::bterms
   } when opname == subst_opname -> 
      let bt = dest_bterm btrm in
      (bt.bterm, dest_subst_args t (bt.bvars,bterms))
  | _ -> raise (TermMatch ("dest_subst", t, ""))

(*
 * Make a "subst" term.
 *)

let mk_subst_term main_term subst =
   let sub = List.split subst in
   let vars = fst sub in
   let terms = List.map mk_simple_bterm (snd sub) in
   mk_term
      { op_name = subst_opname; op_params = [] }
      (mk_bterm vars main_term::terms)

let make_subst_term main_term vars trms =
   if List.length vars != List.length trms then raise (Invalid_argument "make_subst_term") else
   let terms = List.map mk_simple_bterm trms in
   mk_term
      { op_name = subst_opname; op_params = [] }
      (mk_bterm vars main_term::terms)

let make_1subst_term main_term v t =
   let fvm = StringSet.remove v main_term.free_vars in
   { free_vars = StringSet.union fvm t.free_vars; 
     core = Term
      { term_op = { op_name = subst_opname; op_params = [] };
        term_terms = 
         [ { bfree_vars = fvm;
             bcore = BTerm {bvars=[v]; bterm=main_term}};
           { bfree_vars = t.free_vars;
             bcore = BTerm {bvars=[]; bterm=t}}]}}

let make_2subst_term main_term v1 v2 t1 t2 =
   let fvm = StringSet.remove v1 (StringSet.remove v2 main_term.free_vars) in
   { free_vars = StringSet.union fvm (StringSet.union t1.free_vars t2.free_vars); 
     core = Term
      { term_op = { op_name = subst_opname; op_params = [] };
        term_terms = 
         [ { bfree_vars = fvm;
             bcore = BTerm {bvars=[v1;v2]; bterm=main_term}};
           { bfree_vars = t1.free_vars;
             bcore = BTerm {bvars=[]; bterm=t1}};
           { bfree_vars = t2.free_vars;
             bcore = BTerm {bvars=[]; bterm=t2}}]}}
