(*
 * This is the simple term module, where the
 * implementation of the term mirrors the interface.
 * Destructors are identity functions.
 *)

open Printf

open Debug
open Opname

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term%t" eflush

(*
 * Simple term module.
 *)
module Term =
struct
   (************************************************************************
    * Type definitions                                                     *
    ************************************************************************)

   (*
    * The type are just the naive types.
    *)
   type level_exp_var = level_exp_var'
   and level_exp = level_exp'
   and param = param'
   and operator = { mutable imp_op_name : opname; imp_op_params : param list }

   and term = term'
   and bound_term = bound_term'

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   and level_exp_var' = { le_var : string; le_offset : int }

   and level_exp' = { le_const : int; le_vars : level_exp_var list }

   (*
    * Parameters have a number of simple types.
    *)
   and param' =
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
    | ObId of object_id
    | ParamList of param list

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

   type term_subst = (string * term) list

   (*
    * General exception for term destruction.
    *)
   exception TermMatch of string * term * string
   exception BadMatch of term * term

   (************************************************************************
    * DEBUGGING                                                            *
    ************************************************************************)

   (*
    * Printer is installed by client.
    *)
   let print_term = ref (fun _ _ -> raise (Failure "Term_ds.print_term: printer not installed"))

   let debug_print out t =
      !print_term out t

   let install_debug_printer f =
      print_term := f

   (************************************************************************
    * Term de/constructors                                                 *
    ************************************************************************)

   (*
    * These are basically identity functions for this implementation.
    *)
   let mk_term op bterms = { term_op = op; term_terms = bterms }

   let make_term term = term

   let dest_term term = term

   let mk_op name params =
      { imp_op_name = name; imp_op_params = params }

   let make_op { op_name = name; op_params = params } =
      { imp_op_name = name; imp_op_params = params }

   let dest_op { imp_op_name = name; imp_op_params = params } =
      { op_name = name; op_params = params }

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
      { term_op = { imp_op_name = name } } ->
         name

   (*
    * Get the subterms.
    * None of the subterms should be bound.
    *)
   let subterms_of_term t =
      List.map (fun { bterm = t } -> t) t.term_terms

   let subterm_count { term_terms = terms } =
      List.length terms

   let subterm_arities { term_terms = terms } =
      List.map (fun { bvars = vars } -> List.length vars) terms

   (************************************************************************
    * Variables                                                            *
    ************************************************************************)

   let var_opname = make_opname ["var"]

   (*
    * See if a term is a variable.
    *)
   let is_var_term = function
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
        term_terms = []
      } when opname == var_opname -> true
    | _ -> false

   (*
    * Destructor for a variable.
    *)
   let dest_var = function
      { term_op = { imp_op_name = opname; imp_op_params = [Var v] };
        term_terms = []
      } when opname == var_opname -> v
    | t -> raise (TermMatch ("dest_var", t, ""))

   (*
    * Make a variable.
    *)
   let mk_var_term v =
      { term_op = { imp_op_name = var_opname; imp_op_params = [Var v] };
        term_terms = []
      }

   let mk_var_op v = { imp_op_name = var_opname; imp_op_params = [Var v] }

   (*
    * Second order variables have subterms.
    *)
   let is_so_var_term = function
      ({ term_op = { imp_op_name = opname; imp_op_params = [Var(_)] }; term_terms = bterms } : term)
      when opname == var_opname ->
         List.for_all (function { bvars = [] } -> true | _ -> false) bterms
    | _ -> false

   let dest_so_var = function
      ({ term_op = { imp_op_name = opname; imp_op_params = [Var(v)] };
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
         { term_op = { imp_op_name = var_opname; imp_op_params = [Var(v)] };
           term_terms = List.map mk_bterm terms
         }

   (*
    * Second order context, contains a context term, plus
    * binding variables like so vars.
    *)
   let context_opname = make_opname ["context"]

   let is_context_term = function
      ({ term_op = { imp_op_name = opname; imp_op_params = [Var _] }; term_terms = bterms } : term)
      when opname == context_opname ->
         List.for_all (function { bvars = [] } -> true | _ -> false) bterms
    | term ->
         false

   let dest_context = function
      ({ term_op = { imp_op_name = opname; imp_op_params = [Var v] };
         term_terms = bterms
       } : term) as term when opname == context_opname ->
         let rec collect = function
            [{ bvars = []; bterm = t }] ->
               [], t
          | { bvars = []; bterm = t } :: tl ->
               let args, term = collect tl in
                  t :: args, term
          | _ ->
               raise (TermMatch ("dest_context", term, "bvars exist"))
         in
         let args, term = collect bterms in
            v, term, args
    | term ->
         raise (TermMatch ("dest_context", term, "not a context"))

   let mk_context_term v term terms =
      let rec collect term = function
         [] ->
            [{ bvars = []; bterm = term }]
       | h::t ->
            { bvars = []; bterm = h } :: collect term t
      in
         { term_op = { imp_op_name = context_opname; imp_op_params = [Var v] };
           term_terms = collect term terms
         }

   (************************************************************************
    * Simple terms                                                         *
    ************************************************************************)

   (*
    * "Simple" terms have no parameters and no binding variables.
    *)
   let is_simple_term_opname name = function
      { term_op = { imp_op_name = name'; imp_op_params = [] };
        term_terms = bterms
      } when name' == name ->
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
      mk_any_term { imp_op_name = name; imp_op_params = [] } terms

   let dest_simple_term = function
      ({ term_op = { imp_op_name = name; imp_op_params = [] };
         term_terms = bterms
       } : term) as t ->
         let aux = function
            { bvars = []; bterm = t } -> t
          | _ -> raise (TermMatch ("dest_simple_term", t, "binding vars exist"))
         in
            name, List.map aux bterms
    | t ->
         raise (TermMatch ("dest_simple_term", t, "params exist"))

   let dest_simple_term_opname name = function
      ({ term_op = { imp_op_name = name'; imp_op_params = [] };
         term_terms = bterms
       } : term) as t ->
         if name == name' then
            let aux = function
               { bvars = []; bterm = t } -> t
             | _ -> raise (TermMatch ("dest_simple_term_opname", t, "binding vars exist"))
            in
               List.map aux bterms
         else
            raise (TermMatch ("dest_simple_term_opname", t, "opname mismatch"))
    | t ->
         raise (TermMatch ("dest_simple_term_opname", t, "params exist"))

   (*
    * Bound terms.
    *)
   let mk_simple_bterm bterm =
      { bvars = []; bterm = bterm }

   let dest_simple_bterm t = function
      { bvars = []; bterm = bterm } ->
         bterm
    | _ ->
         raise (TermMatch ("dest_simple_bterm", t, "bterm is not simple"))

   (*
    * "Normalization" means producing a canonical version of the term,
    * not reduction.  Right now, this just means rehashing the opname.
    *)
   let rec normalize_term = function
      { term_op = op; term_terms = bterms } ->
         op.imp_op_name <- normalize_opname op.imp_op_name;
         List.iter normalize_bterm bterms

   and normalize_bterm { bterm = t } =
      normalize_term t
end

(*
 * $Log$
 * Revision 1.11  1998/06/22 19:45:57  jyh
 * Rewriting in contexts.  This required a change in addressing,
 * and the body of the context is the _last_ subterm, not the first.
 *
 * Revision 1.10  1998/06/15 22:53:56  nogin
 * Use == for comparing opnames
 *
 * Revision 1.9  1998/06/15 21:57:21  jyh
 * Added a few new functions.
 *
 * Revision 1.8  1998/06/12 13:47:10  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.7  1998/06/03 22:19:37  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.6  1998/06/03 15:23:58  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.5  1998/06/02 21:52:00  nogin
 * Use == for comparing opnames
 *
 * Revision 1.4  1998/06/01 19:53:51  jyh
 * Working addition proof.  Removing polymorphism from refiner(?)
 *
 * Revision 1.3  1998/06/01 13:55:32  jyh
 * Proving twice one is two.
 *
 * Revision 1.2  1998/05/30 19:18:48  nogin
 * Eliminated white space in empty lines.
 *
 * Revision 1.1  1998/05/28 15:02:41  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:54  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
