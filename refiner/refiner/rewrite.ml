(*
 * This module specifies rewrite rules, which require second
 * order variables.  Each rule has a "redex" and a "contractum",
 * although rewrites can be performed in either direction.
 *
 *)

open Printf
open Debug
open Opname
open Term_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Rewrite%t" eflush

(*
 * Rewrite module.
 *)
module Rewrite (Term : TermSig) (**)
   (TermMan : TermManSig
    with type term = Term.term)
   (TermAddr : TermAddrSig
    with type term = Term.term)
   (TermSubst : TermSubstSig
    with type term = Term.term) =
struct
   open Term
   open TermMan
   open TermAddr
   open TermSubst

   type term = Term.term
   type level_exp = Term.level_exp
   type param = Term.param
   type operator = Term.operator
   type bound_term = Term.bound_term
   type address = TermAddr.address

   (************************************************************************
    * Rewrites
    ************************************************************************)

   (*
    * We need to define a term for matching rewrite rules.
    * This is similar to a DeBruijn term, but it includes second
    * order variables.
    *)
   type rwparam =
      RWNumber of Num.num
    | RWString of string
    | RWToken of string
    | RWLevel of level_exp
    | RWVar of string
    | RWMNumber of int
    | RWMString of int
    | RWMToken of int
    | RWMLevel of int
    | RWMVar of int
    | RWSum of rwparam * rwparam
    | RWDiff of rwparam * rwparam
    | RWProduct of rwparam * rwparam
    | RWQuotient of rwparam * rwparam
    | RWRem of rwparam * rwparam
    | RWLessThan of rwparam * rwparam
    | RWEqual of rwparam * rwparam
    | RWNotEqual of rwparam * rwparam
    | RWObId of object_id
    | RWParamList of rwparam list
   and rwoperator = { rw_name : opname; rw_params : rwparam list }

   (*
    * These are the types of terms.
    * In a redex:
    *    RWComposite matches a term with a given pattern
    *    RWSOVar matches any term
    *    RWSOContext matches a second order context with an addressed subterm
    *    RWCheckVar matches the specific bound variable
    * In a contractum:
    *    RWComposite construct a term with the given pattern
    *    RWSOMatch matches an instantiated second order variable
    *    RWSOSubst instantiates a second order variable
    *    RWSOContextSubst instantiates a second order context
    *    RWCheckVar instantiates a bound variable
    *)
   and rwterm =
      RWComposite of rwcterm
    | RWSOVar of int * int list
    | RWSOMatch of int * (int list * string list * term list)
    | RWSOSubst of int * rwterm list
    | RWSOContext of int * int * rwterm * int list
    | RWSOContextSubst of int * rwterm * rwterm list
    | RWCheckVar of int
    | RWStackVar of int
    | RWError

   (* Match a specific term *)
   and rwcterm = { rw_op : rwoperator; rw_bterms : rwboundTerm list }

   (*
    * In the bound term, rw_bnames is used in the contractum
    * for suggesting names for the bound variables.
    *)
   and rwboundTerm = { rw_bvars : int; rw_bnames : varname list; rw_bterm : rwterm }

   and varname =
      StackName of int
    | ArgName of int
    | SaveName of string

   (*
    * During redex compilation, we keep track of
    * second order variables and binding variables.
    *)
   type rstack =
      FOVarPattern of string
    | SOVarPattern of string
    | SOVarInstance of string
    | FOVar of string
    | CVar of string
    | PIVar of string
    | PSVar of string
    | PLVar of string

   (*
    * During reduction, we keep a stack of objects of all the
    * possible types.
    *)
   type stack =
      StackVoid
    | StackNumber of Num.num
    | StackString of string
    | StackLevel of level_exp
    | StackBTerm of term * string list
    | StackITerm of (term * string list * string list * term list) list
    | StackContext of string list * term * address

   type rewrite_stack = stack array

   (*
    * A contractum can be a term to be instantiated,
    * or it can be a function to be called.
    *)
   type rwcontractum =
      RWCTerm of rwterm list
    | RWCFunction of (term -> term)

   (*
    * The rewrite rule contains an rwterm for matching a redex,
    * and another for constructing the contractum.
    *)
   type rewrite_rule =
      {  (* Redex, and matching stack *)
         rr_redex : rwterm list;
         rr_gstacksize : int;

         (* The contractum is a term or a function *)
         rr_contractum : rwcontractum;

         (* After a reduction, the namer function extracts variable names *)
         rr_namer : stack array -> string array -> string array
      }

   (*
    * Separated formas.
    *)
   type rewrite_redex =
      { redex_stack : rstack array;
        redex_redex : rwterm list
      }

   type rewrite_contractum =
      { con_contractum : rwterm }

   (*
    * Types for redex matching.
    *)
   type rewrite_type =
      RewriteTermType of string
    | RewriteFunType of string
    | RewriteContextType of string
    | RewriteStringType of string
    | RewriteIntType of string
    | RewriteLevelType of string

   type rewrite_item =
      RewriteTerm of term
    | RewriteFun of (term list -> term)
    | RewriteContext of (term -> term list -> term)
    | RewriteString of string
    | RewriteInt of int
    | RewriteLevel of level_exp

   (*
    * Matchings
    *)
   type match_type =
      ParamMatch of param
    | VarMatch of string
    | TermMatch of term
    | BTermMatch of bound_term

   (* Exceptions *)
   type rewrite_error =
      BoundSOVar of string
    | FreeSOVar of string
    | BoundParamVar of string
    | FreeParamVar of string
    | BadRedexParam of param
    | NoRuleOperator
    | BadMatch of match_type
    | AllSOInstances of string
    | MissingContextArg of string
    | StackError of stack
    | StringError of string

   exception RewriteError of rewrite_error

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   (*
    * Upgrade a second-order instance to a pattern
    *)
   let rstack_upgrade v =
      let rec aux = function
         (SOVarInstance v')::t when v' = v ->
            (SOVarPattern v)::t
       | ((SOVarPattern v')::t as stack) when v' = v ->
            stack
       | (FOVarPattern v')::t when v' = v ->
            (SOVarPattern v)::t
       | h::t ->
            h::(aux t)
       | [] ->
            raise (Invalid_argument "rstack_upgrade")
      in
         aux

   (*
    * Membership functions.
    *)
   let rstack_mem_prop v = function
      FOVarPattern v' -> v = v'
    | SOVarPattern v' -> v = v'
    | SOVarInstance v' -> v = v'
    | FOVar v' -> v = v'
    | CVar v' -> v = v'
    | PIVar v' -> v = v'
    | PSVar v' -> v = v'
    | PLVar v' -> v = v'

   let rstack_so_mem_prop v = function
      FOVarPattern v' -> v = v'
    | SOVarPattern v' -> v = v'
    | SOVarInstance v' -> v = v'
    | _ -> false

   let rstack_fo_mem_prop v = function
      FOVar v' -> v = v'
    | _ -> false

   let rstack_p_mem_prop v = function
      PIVar v' -> v = v'
    | PSVar v' -> v = v'
    | PLVar v' -> v = v'
    | _ -> false

   let rstack_c_mem_prop v = function
      CVar v' -> v = v'
    | _ -> false

   let rstack_mem v = List.exists (rstack_mem_prop v)
   let rstack_so_mem v = List.exists (rstack_so_mem_prop v)
   let rstack_fo_mem v = List.exists (rstack_fo_mem_prop v)
   let rstack_p_mem v = List.exists (rstack_p_mem_prop v)
   let rstack_c_mem v = List.exists (rstack_c_mem_prop v)

   let array_rstack_mem v = Array_util.exists (rstack_mem_prop v)
   let array_rstack_so_mem v = Array_util.exists (rstack_so_mem_prop v)
   let array_rstack_fo_mem v = Array_util.exists (rstack_fo_mem_prop v)
   let array_rstack_c_mem v = Array_util.exists (rstack_c_mem_prop v)
   let array_rstack_p_mem v = Array_util.exists (rstack_p_mem_prop v)

   (*
    * Indexing.
    *)
   let rstack_index v l = List_util.find_item (rstack_mem_prop v) l
   let rstack_so_index v l = List_util.find_item (rstack_so_mem_prop v) l
   let rstack_fo_index v l = List_util.find_item (rstack_fo_mem_prop v) l
   let rstack_p_index v l = List_util.find_item (rstack_p_mem_prop v) l
   let rstack_c_index v l = List_util.find_item (rstack_c_mem_prop v) l

   let array_rstack_index v l = Array_util.find_index (rstack_mem_prop v) l
   let array_rstack_so_index v l = Array_util.find_index (rstack_so_mem_prop v) l
   let array_rstack_fo_index v l = Array_util.find_index (rstack_fo_mem_prop v) l
   let array_rstack_p_index v l = Array_util.find_index (rstack_p_mem_prop v) l
   let array_rstack_c_index v l = Array_util.find_index (rstack_c_mem_prop v) l

   (*
    * Find the index of a binding var into the stack
    * given an association list of indices.
    *)
   let var_index bvars t =
      let s = dest_var t in
         try List.assoc s bvars with
            Not_found -> raise (RewriteError (FreeSOVar s))

   let svar_index bvars s =
      try List.assoc s bvars with
         Not_found -> raise (RewriteError (FreeSOVar s))

   (************************************************************************
    * REWRITE RULE COMPILATION
    ************************************************************************)

   (*
    * Compile a term with second order variables into
    * a rewrite term.
    *
    * The stack contains a list of variable names.
    * All second order variables and first order
    * binding variables go on the stack.
    *
    * The bvars is an association list of the binding
    * variables at the current point being compiled, mapping
    * binding names to stack locations.
    *
    * Return the stack and the compiled term.
    *)
   let compile_so_redex addrs =
      let rec compile_so_redex_term stack bvars term =
         (* Check for variables and contexts *)
         if is_so_var_term term then
            let is_bound_var v =
               (* Determine if a term is a bound variable *)
               if is_var_term v then
                  List.mem_assoc (dest_var v) bvars
               else
                  false
            in
            let gen_subterms subterms =
               (* Find the free variables and their indices *)
               let fv = List_util.intersect (free_vars_terms subterms) (List.map fst bvars) in
                  List.map (svar_index bvars) fv, fv, subterms
            in
            let v, subterms = dest_so_var term in
               (* This is a first or second order variable *)
               if List.mem_assoc v bvars then
                  (* This is a first order variable instance *)
                  if subterms <> [] then
                     raise (RewriteError (BoundSOVar v))
                  else
                     stack, RWCheckVar(svar_index bvars v)

               else if List.for_all is_bound_var subterms then
                  (* This is a second order variable, and all subterms are vars *)
                  if rstack_so_mem v stack then
                     (* Treat this as an instance, but record that a pattern was found *)
                     rstack_upgrade v stack, RWSOMatch(rstack_so_index v stack, gen_subterms subterms)
                  else if subterms = [] then
                     stack @ [FOVarPattern v], RWSOVar(List.length stack, List.map (var_index bvars) subterms)
                  else
                     stack @ [SOVarPattern v], RWSOVar(List.length stack, List.map (var_index bvars) subterms)

               else
                  (* This is a second order variable instance *)
                  if rstack_so_mem v stack then
                     stack, RWSOMatch(rstack_so_index v stack, gen_subterms subterms)
                  else
                     stack @ [SOVarInstance v], RWSOMatch(List.length stack, gen_subterms subterms)

         else if is_context_term term then
            let v, term, vars = dest_context term in
               if rstack_mem v stack then
                  (* The context should have a unique name *)
                  raise (RewriteError (BoundSOVar v))

               else if Array_util.mem v addrs then
                  (* All the vars should be free variables *)
                  let stack' = stack @ [CVar v] in
                  let stack'', term' = compile_so_redex_term stack' bvars term in
                     stack'', RWSOContext(Array_util.index v addrs,
                                          List.length stack,
                                          term',
                                          List.map (var_index bvars) vars)
               else
                  (* No argument for this context *)
                  raise (RewriteError (MissingContextArg v))

         else
            (* This is normal term--not a var *)
            let { term_op = op; term_terms = bterms } = dest_term term in
            let { op_name = name; op_params = params } = dest_op op in
               let stack2, params2 = compile_so_redex_params stack params in
               let stack3, bterms3 = compile_so_redex_bterms stack2 bvars bterms in
                  stack3, RWComposite { rw_op = { rw_name = name; rw_params = params2 };
                                        rw_bterms = bterms3
                                      }

      (*
       * We also compile parameters, and bind meta-variables.
       *)
      and compile_so_redex_params stack = function
         [] -> stack, []
       | param::params ->
            (* Do this param *)
            let stack', param' = compile_so_redex_param stack param in
            let stack'', params' = compile_so_redex_params stack' params in
               stack'', param'::params'

      and compile_so_redex_param stack param =
         let meta_param const pvar v =
            if rstack_p_mem v stack then
               (* This param is not free, do a match *)
               stack, const (rstack_p_index v stack)
            else
               (* Add it *)
               stack @ [pvar v], const (List.length stack)
         in
            match dest_param param with
               MNumber v -> meta_param (fun i -> RWMNumber i) (fun v -> PIVar v) v
             | MString(v) -> meta_param (fun s -> RWMString s) (fun s -> PSVar s) v
             | MToken(v) -> meta_param (fun t -> RWMToken t) (fun s -> PSVar s) v
             | MLevel(v) -> meta_param (fun l -> RWMLevel l) (fun l -> PLVar l) v
             | MVar(v) -> meta_param (fun v -> RWMVar v) (fun v -> PSVar v) v
             | Number(i) -> stack, RWNumber(i)
             | String(s) -> stack, RWString(s)
             | Token(t) -> stack, RWToken(t)
             | Level(i) -> stack, RWLevel(i)
             | Var(v) -> stack, RWVar(v)
             | _ -> raise (RewriteError (BadMatch (ParamMatch param)))

      (*
       * In bterms, have to add these vars to the binding stack.
       *)
      and compile_so_redex_bterms stack bvars = function
         [] -> stack, []
       | bterm::bterms ->
            let stack', bterm' = compile_so_redex_bterm stack bvars bterm in
            let stack'', bterms' = compile_so_redex_bterms stack' bvars bterms in
               stack'', bterm'::bterms'

      and compile_so_redex_bterm stack bvars bterm =
         match dest_bterm bterm with
            { bvars = vars; bterm = term } ->
               (* Add vars to the stack *)
               let stack' = stack @ (List.map (fun v -> FOVar v) vars) in
               let rec bnames i = function
                  h::t -> (StackName i)::(bnames (i + 1) t)
                | [] -> []
               in

               (* Add new bvars *)
               let rec new_bvar_items i = function
                  h::t -> (h, i)::(new_bvar_items (i + 1) t)
                | [] -> []
               in
               let l = List.length stack in
               let bvars' = bvars @ (new_bvar_items l vars) in

               (* Compile the term *)
               let stack'', term' = compile_so_redex_term stack' bvars' term in
                  stack'', { rw_bvars = List.length vars; rw_bnames = bnames l vars; rw_bterm = term' }
      in

      (*
       * This is the final version.
       *)
      let compile t =
         let rec aux stack = function
            [] -> stack, []
          | term::t ->
               let stack', term' = compile_so_redex_term stack [] term in
               let stack'', terms' = aux stack' t in
                  stack'', term'::terms'
         in
         let stack, terms = aux [] t in
         let check_stack = function
            SOVarInstance n ->
               raise (RewriteError (AllSOInstances n))
          | _ -> ()
         in
            List.iter check_stack stack;
            Array.of_list stack, terms
      in
         compile

   (*
    * When the contractum is compiled, the redex has already been
    * compiled, and the stack contains a list of the special variables.
    *)
   let compile_so_contractum names stack =
      let bname n =
         if Array_util.mem n names then
            ArgName (Array_util.index n names)
         else if array_rstack_fo_mem n stack then
            StackName (array_rstack_fo_index n stack)
         else
            SaveName n
      in
      let rec compile_so_contractum_term bvars term =
         if is_so_var_term term then
            let v, subterms = dest_so_var term in
               (* This is a first or second order variable *)
               if List.mem v bvars then
                  (* This is a first order variable instance *)
                  if subterms <> [] then
                     raise (RewriteError (BoundSOVar v))
                  else
                     RWCheckVar(List_util.find_index v bvars)

               else if array_rstack_so_mem v stack then
                  (*
                   * This is a second order variable.
                   * The variable v should be bound, and we generate a
                   * a substitution instance.
                   *)
                  RWSOSubst(array_rstack_so_index v stack,
                            List.map (compile_so_contractum_term bvars) subterms)

               else if array_rstack_fo_mem v stack & subterms = [] then
                  (* This variable represents a binding occurrence *)
                  RWStackVar (array_rstack_fo_index v stack)

               else
                  (* This is a second order variable that is free *)
                  raise (RewriteError (FreeSOVar v))

         else if is_context_term term then
            (* This is a second order context *)
            let v, term', subterms = dest_context term in
               if array_rstack_c_mem v stack then
                  (*
                   * Second order context and the v is bound.
                   * We generate a substitution instance.
                   *)
                  RWSOContextSubst(
                     array_rstack_c_index v stack,
                     compile_so_contractum_term bvars term',
                     List.map (compile_so_contractum_term bvars) subterms)

               else
                  (* Free second order context *)
                  raise (RewriteError (FreeSOVar v))

         else
            (* This is a normal term--not a var *)
            let { term_op = op; term_terms = bterms } = dest_term term in
            let { op_name = name; op_params = params } = dest_op op in
               (* This is a normal term *)
            let params' = List.map compile_so_contractum_param params in
            let bterms' = List.map (compile_so_contractum_bterm bvars) bterms in
               RWComposite { rw_op = { rw_name = name; rw_params = params' };
                             rw_bterms = bterms'
               }

      (*
       * We also compile parameters, and bind meta-variables.
       *)
      and compile_so_contractum_param param =
         match dest_param param with
            MNumber v ->
               if array_rstack_p_mem v stack then
                  (* New param *)
                  RWMNumber (array_rstack_p_index v stack)
               else
                  (* Free param *)
                  raise (RewriteError (FreeParamVar v))

          | MString v ->
               if array_rstack_p_mem v stack then
                  (* New param *)
                  RWMString (array_rstack_p_index v stack)
               else
                  (* Free param *)
                  raise (RewriteError (FreeParamVar v))

          | MToken v ->
               if array_rstack_p_mem v stack then
                  (* New param *)
                  RWMToken (array_rstack_p_index v stack)
               else
                  (* Free param *)
                  raise (RewriteError (FreeParamVar v))

          | MLevel v ->
               if array_rstack_p_mem v stack then
                  (* New param *)
                  RWMLevel (array_rstack_p_index v stack)
               else
                  (* Free param *)
                  raise (RewriteError (FreeParamVar v))

          | MVar v ->
               if array_rstack_p_mem v stack then
                  (* New param *)
                  RWMVar(array_rstack_p_index v stack)
               else
                  (* Free param *)
                  raise (RewriteError (FreeParamVar v))

          | Number i -> RWNumber i
          | String s -> RWString s
          | Token t -> RWToken t
          | Level i -> RWLevel i
          | Var v -> RWVar v

          | MSum (a, b) -> RWSum (compile_so_contractum_param a,
                                  compile_so_contractum_param b)
          | MDiff (a, b) -> RWDiff (compile_so_contractum_param a,
                                    compile_so_contractum_param b)
          | MProduct (a, b) -> RWProduct (compile_so_contractum_param a,
                                          compile_so_contractum_param b)
          | MQuotient (a, b) -> RWQuotient (compile_so_contractum_param a,
                                            compile_so_contractum_param b)
          | MRem (a, b) -> RWRem (compile_so_contractum_param a,
                                  compile_so_contractum_param b)
          | MLessThan (a, b) -> RWLessThan (compile_so_contractum_param a,
                                            compile_so_contractum_param b)
          | MEqual (a, b) -> RWEqual (compile_so_contractum_param a,
                                      compile_so_contractum_param b)
          | MNotEqual (a, b) -> RWNotEqual (compile_so_contractum_param a,
                                            compile_so_contractum_param b)

          | ObId id ->
               RWObId id

          | ParamList l ->
               RWParamList (List.map compile_so_contractum_param l)

      (*
       * In bterms, have to add these vars to the binding stack.
       *)
      and compile_so_contractum_bterm bvars bterm =
         match dest_bterm bterm with
            { bvars = vars; bterm = term } ->
               let term' = compile_so_contractum_term (bvars @ vars) term in
               let vars' = List.map bname vars in
                  { rw_bvars = List.length vars; rw_bnames = vars'; rw_bterm = term' }
      in
         compile_so_contractum_term []

   (*
    * Naming function.
    *)
   let compute_namer stack names =
      (* Compute an array of names to change *)
      let compute_index v =
         if array_rstack_fo_mem v stack then
            Some (array_rstack_fo_index v stack)
         else
            None
      in
      let indices = Array.map compute_index names in
      let length = Array.length names in
      let namer stack' names' =
         let names'' = Array.copy names' in
            for i = 0 to length - 1 do
               match indices.(i) with
                  Some j ->
                     begin
                        match stack'.(j) with
                           StackString s ->
                              names''.(j) <- s
                         | x ->
                              raise (RewriteError (StackError x))
                     end
                | None ->
                     ()
            done;
            names''
      in
         namer

   (*
    * Compile redex and contractum, and form a rewrite rule.
    *)
   let term_rewrite (addrs, names) redex contracta =
      let stack, redex' = compile_so_redex addrs redex in
      let namer = compute_namer stack names in
      let contracta' = List.map (compile_so_contractum names stack) contracta in
         { rr_redex = redex';
           rr_namer = namer;
           rr_contractum = RWCTerm contracta';
           rr_gstacksize = Array.length stack
         }

   (*
    * Make a ML function rewrite.
    *)
   let fun_rewrite redex f =
      let stack, redex' = compile_so_redex [||] [redex] in
         { rr_redex = redex';
           rr_namer = (fun stack names -> names);
           rr_contractum = RWCFunction(f);
           rr_gstacksize = Array.length stack
         }

   (*
    * Compile just the redex.
    *)
   let compile_redices addrs redices =
      let stack, redices = compile_so_redex addrs redices in
         { redex_stack = stack; redex_redex = redices }

   let compile_redex addrs redex =
      let redex = compile_redices addrs [redex] in
         match redex.redex_redex with
            [_] ->
               redex
          | _ ->
               failwith "compile_redex: too many redices"

   (*
    * Compile a contractum, given the previous redex.
    *)
   let compile_contractum { redex_stack = stack } contractum =
      let contractum = compile_so_contractum [||] stack contractum in
         { con_contractum = contractum }

   (************************************************************************
    * REWRITE RULE APPLICATION
    ************************************************************************)

   (*
    * Get the vars from their indices.
    *)
   let extract_bvars stack l =
      let extract_bvar v =
         match stack.(v) with
            StackString s -> s
          | x -> raise (RewriteError (StackError x))
      in
         List.map extract_bvar l

   (*
    * Add the assignment to the list.
    *)
   let insert_bvars bvars l =
      Array.append bvars (Array.of_list l)

   (*
    * Assign the bvars.
    *)
   let set_bvars stack names vars =
      let aux v = function
         StackName i -> stack.(i) <- StackString v
       | _ -> ()
      in
         List.iter2 aux vars names

   (*
    * Check that two terms are equal under the given var equivalence
    *)
   let check_simple_match ((t, v) as tv) tv' =
      if not (alpha_equal_vars tv tv') then
         raise (RewriteError (BadMatch (TermMatch t)))

   (*
    * Check that the terms are all equivalent under the given instantiations
    *)
   let check_match ((t, vars) as tv) =
      let rec aux = function
         h::tl ->
            if alpha_equal_match tv h then
               aux tl
            else
               raise (RewriteError (BadMatch (TermMatch t)))
       | [] -> ()
      in
         aux

   (*
    * Match a term against the redex.
    *)
   let match_redex addrs stack =
       let rec match_redex_term t' t =
           match t' with
              RWComposite { rw_op = { rw_name = name'; rw_params = params' }; rw_bterms = bterms' } ->
                 let { term_op = op; term_terms = bterms } = dest_term t in
                 let { op_name = name; op_params = params } = dest_op op in
                    if !debug_rewrite then
                       eprintf "Rewrite.match_redex.RWComposite: %s/%s%t" (**)
                          (flat_opname name')
                          (flat_opname name)
                          eflush;
                    if name == name' then
                       begin
                          List.iter2 match_redex_params params' params;
                          List.iter2 match_redex_bterms bterms' bterms
                       end
                    else
                       raise (RewriteError (BadMatch (TermMatch t)))

            | RWCheckVar i ->
                 begin
                    let v = dest_var t in
                       if !debug_rewrite then
                          eprintf "Rewrite.match_redex.RWCheckVar: %d/%s%t" i v eflush;
                        match stack.(i) with
                           StackString v' ->
                             if !debug_rewrite then
                                eprintf "Rewrite.match_redex.RWCheckVar: %s/%s%t" v' v eflush;
                              if v' <> v then
                                 raise (RewriteError (BadMatch (VarMatch v)))
                         | x ->
                            raise (RewriteError (StackError x))
                 end

            | RWSOVar (i, l) ->
                 (* Save the term at i *)
                 begin
                    let vars = extract_bvars stack l in
                       if !debug_rewrite then
                          eprintf "Rewrite.match_redex.RWSOVar%t" eflush;
                       match stack.(i) with
                          StackVoid ->
                             stack.(i) <- StackBTerm (t, vars)
                        | StackBTerm (t', vars') ->
                             check_simple_match (t, vars) (t', vars)
                        | StackITerm l ->
                             check_match (t, vars) l;
                             stack.(i) <- StackBTerm (t, vars)
                        | _ ->
                             raise (RewriteError (StackError stack.(i)))
                 end

            | RWSOMatch (i, (ivars, vars, subterms)) ->
                 (* See if the term matches *)
                 begin
                    let vars' = extract_bvars stack ivars in
                       if !debug_rewrite then
                          eprintf "Rewrite.match_redex.RWSOMatch%t" eflush;
                        match stack.(i) with
                           StackVoid ->
                              stack.(i) <- StackITerm [t, vars', vars, subterms]
                         | StackBTerm (t'', vars'') ->
                              check_match (t'', vars'') [t, vars', vars, subterms]
                         | StackITerm l ->
                              stack.(i) <- StackITerm ((t, vars', vars, subterms)::l)
                         | _ ->
                              raise (RewriteError (StackError stack.(i)))
                 end

            | RWSOContext (addr, i, term', l) ->
                 (* Pull an address out of the addr argument *)
                 let addr' = addrs.(addr) in
                 let term = term_subterm t addr' in
                    if !debug_rewrite then
                       eprintf "Rewrite.match_redex.RWSOContext%t" eflush;
                    stack.(i) <- StackContext (extract_bvars stack l, t, addr');
                    match_redex_term term' term

            | _ ->
                 raise (RewriteError (BadMatch (TermMatch t)))

       and match_redex_params p' p =
           match p', dest_param p with
              (* Literal matches *)
              RWNumber i, Number j ->
                 if not (i = j) then
                    raise (RewriteError (BadMatch (ParamMatch p)))
            | RWString s', String s ->
                 if not (s' = s) then
                    raise (RewriteError (BadMatch (ParamMatch p)))
            | RWToken t', Token t ->
                 if not (t' = t) then
                    raise (RewriteError (BadMatch (ParamMatch p)))
            | RWLevel i', Level i ->
                 if not (i' = i) then
                    raise (RewriteError (BadMatch (ParamMatch p)))
            | RWVar v', Var v ->
                 if not (v' = v) then
                    raise (RewriteError (BadMatch (ParamMatch p)))

              (* Variable matches *)
            | RWMNumber i, Number j ->
                 stack.(i) <- StackNumber j
            | RWMString i, String s ->
                 stack.(i) <- StackString s
            | RWMToken i, Token t ->
                 stack.(i) <- StackString t
            | RWMVar i, Var v ->
                 stack.(i) <- StackString v
            | RWMLevel i, Level l ->
                 stack.(i) <- StackLevel l

            | _ -> raise (RewriteError (BadMatch (ParamMatch p)))

       and match_redex_bterms bt' bt =
           match bt', dest_bterm bt with
               { rw_bvars = vars'; rw_bnames = names; rw_bterm = bterm' },
               { bvars = vars; bterm = bterm } ->
                  set_bvars stack names vars;
                  if vars' = List.length vars then
                     match_redex_term bterm' bterm
                  else
                     raise (RewriteError (BadMatch (BTermMatch bt)))
       in
           List.iter2 match_redex_term

   (*
    * The contractum is built as a second order substitution.
    * For variable renaming, we keep track of the varibale
    * and the name it has been renamed to.  Whenever a second
    * order term is instantiated, we do a calculation of
    * the variables to be renamed, and send it back up.
    *)
   let build_contractum names stack =
      (* Map name specifications to names *)
      let bname = function
         ArgName i -> names.(i)
       | StackName i ->
            begin
               match stack.(i) with
                  StackString s -> s
                | x -> raise (RewriteError (StackError x))
            end
       | SaveName n -> n
      in

      (* Build the terms *)
      let rec build_contractum_term bvars = function
         RWComposite { rw_op = { rw_name = name; rw_params = params }; rw_bterms = bterms } ->
            (* Build a regular term from the parts *)
            mk_term (mk_op name (build_contractum_params params))
                (build_contractum_bterms bvars bterms)

       | RWSOSubst(i, terms) ->
            begin
                (*
                 * Instantiate a second order term.
                 * Find its free variables, and rename the binding stack
                 * if necessary.
                 *)
                match stack.(i) with
                   StackBTerm(term, vars) -> subst term (List.map (build_contractum_term bvars) terms) vars
                 | _ -> raise (RewriteError (StackError stack.(i)))
            end

       | RWSOContextSubst(i, t, terms) ->
            begin
                (*
                 * Instantiate a context.
                 *)
                match stack.(i) with
                   StackContext(vars, term, addr) ->
                      subst (replace_subterm term addr (build_contractum_term bvars t))
                            (List.map (build_contractum_term bvars) terms)
                            vars
                 | _ -> raise (RewriteError (StackError stack.(i)))
            end

       | RWCheckVar i ->
            (*
             * This is a bound occurrence.
             *)
            mk_var_term bvars.(i)

       | RWStackVar i ->
            (*
             * This is a bound occurrence.
             *)
            begin
               match stack.(i) with
                  StackString s ->
                     mk_var_term s
                | x -> raise (RewriteError (StackError x))
            end

       | t -> raise (RewriteError (StringError "bad contractum"))

      and build_contractum_params params =
         let rec build_contractum_param = function
            RWNumber i -> Number i
          | RWString s -> String s
          | RWToken s -> Token s
          | RWLevel l -> Level l
          | RWVar v -> Var v
          | RWMNumber i ->
               begin
                   match stack.(i) with
                      StackNumber j -> Number j
                    | StackString s -> Number (Num.num_of_string s)
                    | t -> raise (RewriteError (StackError t))
               end
          | RWMString i ->
               begin
                   match stack.(i) with
                      StackString s -> String s
                    | StackNumber j -> String (Num.string_of_num j)
                    | t -> raise (RewriteError (StackError t))
               end
          | RWMToken i ->
               begin
                   match stack.(i) with
                      StackString s -> Token s
                    | StackNumber j -> String (Num.string_of_num j)
                    | t -> raise (RewriteError (StackError t))
               end
          | RWMLevel i ->
               begin
                   match stack.(i) with
                      StackLevel l -> Level l
                    | t -> raise (RewriteError (StackError t))
               end
          | RWMVar i ->
               begin
                   match stack.(i) with
                      StackString v -> Var v
                    | StackNumber j -> Var (Num.string_of_num j)
                    | t -> raise (RewriteError (StackError t))
               end
          | RWSum (p1, p2) ->
               begin
                   match (build_contractum_param p1, build_contractum_param p2) with
                      (Number i, Number j) -> Number (Num.add_num i j)
                    | (Number i, p) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
                    | (p, _) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
               end
          | RWDiff (p1, p2) ->
               begin
                   match (build_contractum_param p1, build_contractum_param p2) with
                      (Number i, Number j) -> Number (Num.sub_num i j)
                    | (Number i, p) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
                    | (p, _) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
               end
          | RWProduct (p1, p2) ->
               begin
                   match (build_contractum_param p1, build_contractum_param p2) with
                      (Number i, Number j) -> Number (Num.mult_num i j)
                    | (Number i, p) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
                    | (p, _) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
               end
          | RWQuotient (p1, p2) ->
               begin
                   match (build_contractum_param p1, build_contractum_param p2) with
                      (Number i, Number j) -> Number (Num.quo_num i j)
                    | (Number i, p) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
                    | (p, _) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
               end
          | RWRem (p1, p2) ->
               begin
                   match (build_contractum_param p1, build_contractum_param p2) with
                      (Number i, Number j) -> Number (Num.mod_num i j)
                    | (Number i, p) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
                    | (p, _) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
               end
          | RWLessThan (p1, p2) ->
               begin
                   match (build_contractum_param p1, build_contractum_param p2) with
                      (Number i, Number j) -> Token (if i < j then "true" else "false")
                    | (Number i, p) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
                    | (p, _) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
               end
          | RWEqual (p1, p2) ->
               begin
                   match (build_contractum_param p1, build_contractum_param p2) with
                      (Number i, Number j) -> Token (if i = j then "true" else "false")
                    | (Number i, p) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
                    | (p, _) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
               end
          | RWNotEqual (p1, p2) ->
               begin
                   match (build_contractum_param p1, build_contractum_param p2) with
                      (Number i, Number j) -> Token (if i = j then "false" else "true")
                    | (Number i, p) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
                    | (p, _) -> raise (RewriteError (BadMatch (ParamMatch (make_param p))))
               end
          | RWObId id ->
               ObId id
          | RWParamList l ->
               ParamList (build_contractum_params l)
         in
         let build_contractum_param' p =
            make_param (build_contractum_param p)
         in
            List.map build_contractum_param' params

      and build_contractum_bterms bvars bterms =
         let build_contractum_bterm = function
            { rw_bvars = vcount; rw_bnames = vars; rw_bterm = term } ->
               let vars' = List.map bname vars in
                  mk_bterm vars' (build_contractum_term (insert_bvars bvars vars') term)
         in
            List.map build_contractum_bterm bterms
      in
         build_contractum_term [||]

   (*
    * To do the rewrite. match agaist the redex, then
    * instantiate the contractum.
    *)
   let apply_rewrite
       { rr_redex = redex;
         rr_contractum = contractum;
         rr_namer = namer;
         rr_gstacksize = gstacksize
       } (addrs, names) terms =
      let gstack = Array.create gstacksize StackVoid in
         match_redex addrs gstack redex terms;
         let names' = namer gstack names in
   	 match contractum with
   	    RWCTerm c ->
                  List.map (build_contractum names gstack) c, names'
   	  | RWCFunction f ->
                  match terms with
                     [t] -> [f t], names'
                   | _ -> raise (RewriteError (BadMatch (TermMatch xnil_term)))

   (*
    * Compute the redex types.
    *)
   let extract_redex_types { redex_stack = stack } =
      let extract_redex_type = function
         FOVarPattern s -> RewriteTermType s
       | SOVarPattern s -> RewriteFunType s
       | SOVarInstance s -> RewriteFunType s
       | FOVar s -> RewriteStringType s
       | CVar s -> RewriteContextType s
       | PIVar s -> RewriteIntType s
       | PSVar s -> RewriteStringType s
       | PLVar s -> RewriteLevelType s
      in
      let l = Array.length stack in
      let rec aux j =
         if j < l then
            (extract_redex_type stack.(j))::(aux (j + 1))
         else
            []
      in
         aux 0

   (*
    * Given the two stack, extract values that can be used in a program.
    * For each object:
    *    1. A second order variable becomes a function
    *       that takes a list of subexpressions and performs the substitution.
    *    2. A first order variable becomes the string with the name.
    *    3. A context variable is converted to a function
    *       that takes the hole and the subexpressions and
    *       performs the substitution
    *    4. A param variable becaome the param that was matched.
    *)
   let extract_redex_values gstack stack=
      let aux gstack = function
         FOVarPattern _ ->
            begin
               match gstack with
                  StackBTerm (t, []) -> RewriteTerm t
                | _ -> raise (RewriteError (StackError gstack))
            end
       | SOVarPattern _ ->
            begin
               match gstack with
                  StackBTerm (t, l) ->
                     RewriteFun (fun l' -> subst t l' l)
                | _ -> raise (RewriteError (StackError gstack))
            end
       | SOVarInstance _ ->
            failwith "extract_redex_values: SOVarInstance"
       | FOVar _ ->
            begin
               match gstack with
                  StackString s -> RewriteString s
                | _ -> raise (RewriteError (StackError gstack))
            end
       | CVar _ ->
            begin
               match gstack with
                  StackContext (l, t, addr) ->
                     RewriteContext (fun c l' -> subst (replace_subterm t addr c) l' l)
                | _ -> raise (RewriteError (StackError gstack))
            end
       | PIVar _ ->
            begin
               match gstack with
                  StackNumber i -> RewriteInt (Num.int_of_num i)
                | _ -> raise (RewriteError (StackError gstack))
            end
       | PSVar _ ->
            begin
               match gstack with
                  StackString s -> RewriteString s
                | _ -> raise (RewriteError (StackError gstack))
            end
       | PLVar _ ->
            begin
               match gstack with
                  StackLevel l -> RewriteLevel l
                | _ -> raise (RewriteError (StackError gstack))
            end
      in
      let l = Array.length gstack in
      let rec aux' i =
         if i < l then
            (aux gstack.(i) stack.(i))::(aux' (i + 1))
         else
            []
      in
         aux' 0

   (*
    * Match with a redex, and extract the forms to be bound.
    *)
   let apply_redex { redex_stack = stack; redex_redex = redex } addrs terms =
      let gstack = Array.create (Array.length stack) StackVoid in
         match_redex addrs gstack redex terms;
         gstack

   let apply_redex' { redex_stack = stack; redex_redex = redex } addrs terms =
      let gstack = Array.create (Array.length stack) StackVoid in
         match_redex addrs gstack redex terms;
         gstack, extract_redex_values gstack stack

   (*
    * Build a contractum from the spec and a stack.
    *)
   let make_contractum { con_contractum = con } gstack =
      build_contractum [||] gstack con

   (************************************************************************
    * REDEX RELEVANCY                                                      *
    ************************************************************************)

   (*
    * See if an operator generalizes another.
    *)
   let relevant_operator op rw =
      match dest_op op, rw with
         { op_name = name1; op_params = params1 },
         { rw_name = name2; rw_params = params2 } ->
               let compare_params p rwp =
                  match dest_param p, rwp with
                     Number a, RWNumber b -> a = b
                   | String a, RWString b -> a = b
                   | Token a, RWToken b -> a = b
                   | Var a, RWVar b -> a = b
                   | Level a, RWLevel b -> a = b
                   | MNumber a, RWNumber b -> true
                   | MNumber a, RWMNumber b -> true
                   | MString a, RWString b -> true
                   | MString a, RWMString b -> true
                   | MToken a, RWToken b -> true
                   | MToken a, RWMToken b -> true
                   | MLevel a, RWLevel b -> true
                   | MLevel a, RWMLevel b -> true
                   | MVar a, RWVar b -> true
                   | MVar a, RWMVar b -> true
                   | _ -> false
               in
               let rec compare_param_lists = function
                  p1::t1, p2::t2 ->
                     (* Short circuit and *)
                     if compare_params p1 p2 then
                        compare_param_lists (t1, t2)
                     else
                        false
                | [], [] -> true
                | _ -> false
               in
                  if name1 = name2 then
                     compare_param_lists (params1, params2)
                  else
                     false

   (*
    * See if the bterms can be described by these arities.
    *)
   let rec relevant_bterms = function
      arity::arities, { rw_bvars = vars }::t ->
         if vars = arity then
            relevant_bterms (arities, t)
         else
            false
    | [], [] -> true
    | _ -> false

   (*
    * See if a rule is relevant to a term description.
    *)
   let relevant_rule op1 arities = function
      { rr_redex = (RWComposite { rw_op = op2; rw_bterms = bterms })::_ } ->
         if relevant_operator op1 op2 then
            relevant_bterms (arities, bterms)
         else
            false
     | _ -> false

   (*
    * Get the operator of a rewrite rule.
    *)
   let rec convert_param' = function
      RWNumber i -> Number(i)
    | RWString s -> String s
    | RWToken t -> Token t
    | RWLevel i -> Level i
    | RWVar v -> Var v
    | RWMNumber i -> MNumber ("v" ^ (string_of_int i))
    | RWMString i -> MString ("v" ^ (string_of_int i))
    | RWMToken i -> MToken ("v" ^ (string_of_int i))
    | RWMLevel i -> MLevel ("v" ^ (string_of_int i))
    | RWMVar i -> MVar ("v" ^ (string_of_int i))
    | RWSum (i, j) -> MSum (convert_param i, convert_param j)
    | RWDiff (i, j) -> MDiff (convert_param i, convert_param j)
    | RWProduct (i, j) -> MProduct (convert_param i, convert_param j)
    | RWQuotient (i, j) -> MQuotient (convert_param i, convert_param j)
    | RWRem (i, j) -> MRem (convert_param i, convert_param j)
    | RWLessThan (i, j) -> MLessThan (convert_param i, convert_param j)
    | RWEqual (i, j) -> MEqual (convert_param i, convert_param j)
    | RWNotEqual (i, j) -> MNotEqual (convert_param i, convert_param j)
    | RWObId id -> ObId id
    | RWParamList l -> ParamList (List.map convert_param l)
   and convert_param p =
      make_param (convert_param' p)

   let rewrite_operator = function
      { rr_redex = (RWComposite { rw_op = { rw_name = name; rw_params = params } })::_ } ->
           mk_op name (List.map convert_param params)
    | _ -> raise (RewriteError NoRuleOperator)

   (*
    * Get the arities of the subterms.
    *)
   let bterm_eval_flags = function
      { rw_bvars = bvars; rw_bterm = bterm } ->
         bvars, (if bvars = 0 then
                     match bterm with
                        RWSOVar _ -> true
                      | _ -> false
                  else
                     false)

   let rewrite_eval_flags = function
      { rr_redex = (RWComposite { rw_bterms = bterms })::_ } ->
         List.map bterm_eval_flags bterms
    | _ -> raise (RewriteError NoRuleOperator)
end

(*
 * $Log$
 * Revision 1.2  1998/06/01 13:54:46  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:00:36  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.8  1998/05/27 15:14:07  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.7  1998/04/29 14:48:19  jyh
 * Added ocaml_sos.
 *
 * Revision 1.6  1998/04/24 02:42:50  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.5  1998/04/21 19:54:08  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.4  1998/03/20 22:16:19  eli
 * Eli: Changed integer parameters to Num.num's.
 *
 * Revision 1.3  1997/09/12 17:21:44  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.2  1997/08/07 19:43:47  jyh
 * Updated and added Lori's term modifications.
 * Need to update all pattern matchings.
 *
 * Revision 1.1  1997/04/28 15:51:35  jyh
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
 * Revision 1.21  1996/05/21 02:14:08  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.20  1996/04/11 13:29:41  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.19  1996/04/07 18:24:50  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.18  1996/03/30 01:37:54  jyh
 * Initial version of ITT.
 *
 * Revision 1.17  1996/03/28 02:58:24  jyh
 * Prelim checkin for an partial version of the refiner document in the
 * first version of README.tex.
 *
 * Revision 1.16  1996/03/25 20:50:48  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.15  1996/03/11 18:34:28  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 * Revision 1.14  1996/03/08 22:03:43  jyh
 * This rewriter contains the pattern comparisons.
 *
 * Revision 1.13  1996/03/08 15:40:50  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.12  1996/03/05 19:48:37  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.11  1996/02/19 18:47:00  jyh
 * Updating format.prl
 *
 * Revision 1.10  1996/02/18 23:32:32  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.9  1996/02/14 03:51:52  jyh
 * This is a version common to Caml-Light and Caml-Special-Light.
 *
 * Revision 1.8  1996/02/13 21:32:33  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.7  1996/02/10 20:19:56  jyh
 * Initial checkin of filter (prlcomp).
 *
 * Revision 1.6  1996/02/07 23:41:20  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.5  1996/02/07 20:24:59  jyh
 * Partial checkin while I change filenames to lowercase.
 *
 * Revision 1.4  1996/02/05 18:15:00  jyh
 * Merge context rewrites onto the main branch.
 *
 * Revision 1.3.4.1  1996/02/05 06:09:54  jyh
 * This version has the rewriter with contexts, and Rule application
 * in Sequent.ml, but it is not fully debugged.
 *
 * Revision 1.3  1996/01/31 20:02:43  jyh
 * Generalizing rewriter to work on Sequents.
 *
 * Revision 1.2  1996/01/26 20:15:02  jyh
 * This version has a complete rewriter using the simple term structure.
 * Next implement sequents and refinement.
 *
 * Revision 1.1  1995/12/06 16:42:55  jyh
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
