(*
 * The rewriter compiles redices into a stack of terms
 * that is used to collect values when the redex is matched.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_symbol

open Opname
open Term_shape_sig

open Term_sig
open Term_addr_sig
open Rewrite_sig

module MakeRewriteTypes (TermType : TermSig)
   (TermAddr : TermAddrSig with module AddrTypes = TermType) =
struct
   type level_exp = TermType.level_exp
   type object_id = TermType.object_id
   type term = TermType.term
   type operator = TermType.operator
   type address = TermAddr.address
   type seq_hyps = TermType.seq_hyps

   (*
    * For matching level expressions.
    *)
   (* %%MAGICBEGIN%% *)
   type rw_level_exp_var = { rw_le_var : int; rw_le_offset : int }
   type rw_level_exp = { rw_le_const : int; rw_le_vars : rw_level_exp_var list }

   (*
    * We need to define a term for matching rewrite rules.
    * This is similar to a DeBruijn term, but it includes second
    * order variables.
    *)
   type rwparam =
      RWNumber of Lm_num.num
    | RWString of string
    | RWToken of opname
    | RWShape of shape
    | RWQuote
    | RWMNumber of int
    | RWMString of int
    | RWMToken of int
    | RWMShape of int
    | RWMVar of int
    | RWMLevel1 of int
    | RWMLevel2 of rw_level_exp
    | RWObId of object_id
    | RWParamList of rwparam list
   and rwoperator = { rw_name : opname; rw_params : rwparam list }

   (*
    * These are the types of terms.
    * In a redex:
    *    RWComposite matches a term with a given pattern
    *    RWSOVar matches any term
    *    RWSOInstance matches an instantiated second order variable
    *    RWSOContext matches a second order context with an addressed subterm
    *    RWCheckVar matches the specific bound variable
    *    RWFreeVars enforces restrictions on free instances
    *       of some variables. This is generated only in "strict" mode.
    *    RWMatchFreeFOVar is an instance of a free variables (with the list of
    *       context and the list of bvars enforcing that the variable is actually
    *       free - in the strict mode only)
    * In a contractum:
    *    RWComposite construct a term with the given pattern
    *    RWSOINstance instantiates a second order variable
    *    RWSOContextSubst instantiates a second order context
    *    RWCheckVar instantiates a bound variable
    *)
   and rwterm =
      RWComposite of rwcterm
    | RWCompositeSimple of rwcterm_simple
    | RWSequent of rwterm * rw_seq_term list * rwterm
    | RWSOVar of int * int list
    | RWSOInstance of int * rwterm list
    | RWSOContext of int * int * rwterm * int list
    | RWSOFreeVarsContext of int list * int list * int * int * rwterm * int list
    | RWSOContextSubst of int * rwterm * rwterm list
    | RWFreeVars of rwterm * int list * int list
    | RWCheckVar of int
    | RWStackVar of int
    | RWMatchFreeFOVar of int * int list * int list

   (* Match a specific term *)
   and rwcterm = { rw_op : rwoperator; rw_bterms : rw_bound_term list }
   and rwcterm_simple = { rws_op : operator; rws_bterms : rw_bound_term list }

   (*
    * In the bound term, rw_bnames is used in the contractum
    * for suggesting names for the bound variables.
    *)
   and rw_bound_term = { rw_bvars : int; rw_bnames : varname list; rw_bterm : rwterm }

   (*
    * Special forms for sequents.
    *
    * In RWSeqContext, the first int is an index in the addr array, if non-negative,
    * or the number of hyps to leave at the end (-1 - 0 hyps, -2 - 1 hyps, etc).
    *)
   and rw_seq_term =
      RWSeqHyp of varname * rwterm
    | RWSeqContext of int * int * int list
    | RWSeqContextInstance of int * rwterm list
    | RWSeqFreeVarsContext of int list * int list * int * int * int list

   and varname =
      StackName of int
    | SaveName of int

   (*
    * We keep arrays of hyps.
    *)
   type hyp_array = int * int * seq_hyps

   (*
    * During redex compilation, we keep track of
    * second order variables and binding variables.
    * We keep the so conts and arg length for checking.
    *)
   type rstack =
      FreeFOVarPattern of var
    | FreeFOVarInstance of var
    | SOVarPattern of var * var list * int
    | SOVarInstance of var * var list * int
    | FOVar of var
    | CVar of var * var list * int
    | PVar of var * shape_param

   (*
    * A contractum can be a term to be instantiated,
    * or it can be a function to be called.
    *)
   type rwcontractum =
      RWCTerm of rwterm list * var array
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

         rr_strict : strict;
      }

   (*
    * Separated formas.
    *)
   type rewrite_redex =
      { redex_stack : rstack array;
        redex_redex : rwterm list
      }

   type rewrite_contractum =
      { con_contractum : rwterm;
        con_new_vars : var array
      }
   (* %%MAGICEND%% *)

   (*
    * During reduction, we keep a stack of objects of all the
    * possible types.
    *)
   type stack =
      StackVoid
    | StackNumber of Lm_num.num
    | StackString of string
    | StackOpname of opname
    | StackVar of var
    | StackShape of shape
    | StackLevel of level_exp
    | StackBTerm of term * var list
    | StackITerm of (term * rwterm list) list
    | StackContext of var list * term * address
    | StackSeqContext of var list * hyp_array
    | StackContextRestrict of SymbolSet.t

   type rewrite_stack = stack array

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
