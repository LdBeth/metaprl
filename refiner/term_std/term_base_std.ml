(*
 * This is the simple term module, where the
 * implementation of the term mirrors the interface.
 * Destructors are identity functions.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

INCLUDE "refine_error.mlh"

open Printf

open Mp_debug
open Opname
open Refine_error_sig
open Term_std

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_base_std%t"

(*
 * Simple term module.
 *)
module Term (**)
   (RefineError : RefineErrorSig
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type term = TermType.term
    with type bound_term = TermType.bound_term)
=
struct
   (************************************************************************
    * Type definitions                                                     *
    ************************************************************************)

   open TermType
   open RefineError

   type level_exp_var  = TermType.level_exp_var
   type level_exp      = TermType.level_exp
   type param          = TermType.param
   type operator       = TermType.operator
   type term           = TermType.term
   type bound_term     = TermType.bound_term
   type seq_hyps       = TermType.seq_hyps
   type seq_goals      = TermType.seq_goals

   type hypothesis     = TermType.hypothesis
   type level_exp_var' = TermType.level_exp_var'
   type level_exp'     = TermType.level_exp'
   type object_id      = TermType.object_id
   type param'         = TermType.param'
   type match_param    = TermType.match_param
   type operator'      = TermType.operator'
   type term'          = TermType.term'
   type bound_term'    = TermType.bound_term'

   (*
    * Simple substitution.
    *)
   type term_subst = (string * term) list

   module SeqHypType =
   struct
      type t = hypothesis
   end

   module SeqGoalType =
   struct
      type t = term
   end

   module SeqHyp = SEQ_SET.Make (SeqHypType)
   module SeqGoal = SEQ_SET.Make (SeqGoalType)

   (************************************************************************
    * DEBUGGING                                                            *
    ************************************************************************)

   (*
    * Printer is installed by client.
    *)
   let print_term_ref = ref (fun _ _ -> raise (Failure "Term_ds.print_term: printer not installed"))

   let debug_print out t =
      !print_term_ref out t

   let print_term = debug_print

   let rec print_term_list out = function
      [t] ->
         debug_print out t
    | h::t ->
         debug_print out h;
         output_string out ", ";
         print_term_list out t
    | [] ->
         ()

   let install_debug_printer f =
      print_term_ref := f

   (************************************************************************
    * Term de/constructors                                                 *
    ************************************************************************)

   (*
    * No descriptors here.
    *)
   let dest_descriptor _ =
      None

   let mk_descriptor_term d =
      Weak_memo.TheWeakMemo.retrieve_hack d

   (*
    * These are basically identity functions for this implementation.
    *)
   let mk_term op bterms = { term_op = op; term_terms = bterms }

   let make_term x = x (* external make_term : term' -> term = "%identity" *)
   let dest_term x = x (* external dest_term : term -> term' = "%identity" *)

   let mk_op name params =
      { op_name = name; op_params = params }

   let make_op x = x (* external make_op : operator' -> operator = "%identity" *)
   let dest_op x = x (* external dest_op : operator -> operator' = "%identity" *)

   let mk_bterm bvars term = { bvars = bvars; bterm = term }

   let make_bterm x = x (* external make_bterm : bound_term' -> bound_term = "%identity" *)
   let dest_bterm x = x (* external dest_bterm : bound_term -> bound_term' = "%identity" *)
   let make_param x = x (* external make_param : param' -> param = "%identity" *)
   let dest_param x = x (* external dest_param : param -> param' = "%identity" *)
   let dest_params x = x (* external dest_params : param list -> param' list = "%identity" *)

   let dest_match_param param =
      match param with
         Number n ->
            if Mp_num.is_integer_num n then
               MatchNumber (n, Some (Mp_num.int_of_num n))
            else
               MatchNumber (n, None)
       | String s ->
            MatchString s
       | Token s ->
            MatchToken s
       | Var v ->
            MatchVar v
       | MLevel l ->
            MatchLevel l
       | MNumber _
       | MString _
       | MToken _
       | MVar _
       | ObId _
       | ParamList _ ->
            raise (Invalid_argument "Term_base_std.dest_match_param")

   let explode_term t =
      let op = dest_opname t.term_op.op_name in
      let params = List.map dest_match_param t.term_op.op_params in
         op, params, t.term_terms

   let mk_level_var v i =
      { le_var = v; le_offset = i }

   let make_level_var x = x (* external make_level_var : level_exp_var' -> level_exp_var = "%identity" *)
   let dest_level_var x = x (* external dest_level_var : level_exp_var -> level_exp_var' = "%identity" *)

   let mk_level i l =
      { le_const = i; le_vars = l }

   let make_level x = x (* external make_level : level_exp' -> level_exp = "%identity" *)
   let dest_level x = x (* external dest_level : level_exp -> level_exp' = "%identity" *)
   let make_object_id x = x (* external make_object_id : param list -> object_id = "%identity" *)
   let dest_object_id x = x (* external dest_object_id : object_id -> param list = "%identity" *)

   (*
    * Operator names.
    *)
   let opname_of_term = function
      { term_op = { op_name = name } } ->
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

   (*
    * Manifest terms are injected into the "perv" module.
    *)
   let xperv = make_opname ["Perv"]
   let sequent_opname = mk_opname "sequent" xperv

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
      } when Opname.eq opname var_opname -> true
    | _ ->
         false

   (*
    * Destructor for a variable.
    *)
   let dest_var = function
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = []
      } when Opname.eq opname var_opname -> v
    | t ->
         REF_RAISE(RefineError ("dest_var", TermMatchError (t, "not a variable")))

   (*
    * Make a variable.
    *)
   let mk_var_term v =
      { term_op = { op_name = var_opname; op_params = [Var v] };
        term_terms = []
      }

   (*
    * Second order variables have subterms.
    *)
   let is_so_var_term = function
      ({ term_op = { op_name = opname; op_params = [Var(_)] }; term_terms = bterms } : term)
      when Opname.eq opname var_opname ->
         List.for_all (function { bvars = [] } -> true | _ -> false) bterms
    | _ -> false

   let dest_so_var = function
      ({ term_op = { op_name = opname; op_params = [Var(v)] };
         term_terms = bterms
       } : term) as term when Opname.eq opname var_opname ->
         v, List.map (function
            { bvars = []; bterm = t } ->
               t
          | _ ->
               REF_RAISE(RefineError ("dest_so_var", TermMatchError (term, "bvars exist"))))
         bterms
    | term ->
         REF_RAISE(RefineError ("dest_so_var", TermMatchError (term, "not a so_var")))

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
      when Opname.eq opname context_opname ->
         List.for_all (function { bvars = [] } -> true | _ -> false) bterms
    | term ->
         false

   let dest_context = function
      { term_op = { op_name = opname; op_params = [Var v] };
         term_terms = bterms
       } as term when Opname.eq opname context_opname ->
         let rec collect = function
            [{ bvars = []; bterm = t }] ->
               [], t
          | { bvars = []; bterm = t } :: tl ->
               let args, term = collect tl in
                  t :: args, term
          | _ ->
               REF_RAISE(RefineError ("dest_context", TermMatchError (term, "bvars exist")))
         in
         let args, term = collect bterms in
            v, term, args
    | term ->
         REF_RAISE(RefineError ("dest_context", TermMatchError (term, "not a context")))

   let mk_context_term v term terms =
      let rec collect term = function
         [] ->
            [{ bvars = []; bterm = term }]
       | h::t ->
            { bvars = []; bterm = h } :: collect term t
      in
         { term_op = { op_name = context_opname; op_params = [Var v] };
           term_terms = collect term terms
         }

   (************************************************************************
    * Simple terms                                                         *
    ************************************************************************)

   (*
    * "Simple" terms have no parameters and no binding variables.
    *)
   let is_simple_term_opname name = function
      { term_op = { op_name = name'; op_params = [] };
        term_terms = bterms
      } when Opname.eq name' name ->
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
            { bvars = []; bterm = t } ->
               t
          | _ ->
               REF_RAISE(RefineError ("dest_simple_term", TermMatchError (t, "binding vars exist")))
         in
            name, List.map aux bterms
    | t ->
         REF_RAISE(RefineError ("dest_simple_term", TermMatchError (t, "params exist")))

   let dest_simple_term_opname name = function
      ({ term_op = { op_name = name'; op_params = [] };
         term_terms = bterms
       } : term) as t ->
         if Opname.eq name name' then
            let aux = function
               { bvars = []; bterm = t } -> t
             | _ -> REF_RAISE(RefineError ("dest_simple_term_opname", TermMatchError (t, "binding vars exist")))
            in
               List.map aux bterms
         else
            REF_RAISE(RefineError ("dest_simple_term_opname", TermMatchError (t, "opname mismatch")))
    | t ->
         REF_RAISE(RefineError ("dest_simple_term_opname", TermMatchError (t, "params exist")))

   (*
    * Bound terms.
    *)
   let mk_simple_bterm bterm =
      { bvars = []; bterm = bterm }

   let dest_simple_bterm = function
      { bvars = []; bterm = bterm } ->
         bterm
    | _ ->
         REF_RAISE(RefineError ("dest_simple_bterm", StringError ("bterm is not simple")))
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
