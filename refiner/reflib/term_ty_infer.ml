(*
 * The term typeenv define a very simple type system.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 2005-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_printf
open Lm_symbol
open Lm_debug

open Opname
open Term_sig
open Term_ty_sig
open Rewrite_sig

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermTy
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError

open Simple_print.SimplePrint

let debug_infer =
   create_debug (**)
      { debug_name = "ty_infer";
        debug_description = "debug type inference for term typeenv";
        debug_value = false
      }

let is_fo_var_term = is_var_term

(************************************************************************
 * A table indexed on pairs of shapes.
 *)

module Shape2Compare =
struct
   type t = shape * shape

   let compare (shape11, shape12) (shape21, shape22) =
      let code = shape_compare shape11 shape21 in
         if code = 0 then
            shape_compare shape12 shape22
         else
            code
end

module Shape2Table = Lm_map.LmMake (Shape2Compare);;

(************************************************************************
 * Type checking.
 *)

(*
 * A type environment has 3 parts.
 *   tenv_typeclasses : the set of typeclasses
 *   tenv_typeenv     : the typeclass of each type
 *   tenv_termenv     : the type of each term
 *)
type typeclasses      = OpnameSet.t OpnameTable.t
type typeenv          = opname ShapeTable.t
type typereduce       = rewrite_rule Term_match_table.term_table
type typereductions   = (term * term) Shape2Table.t
type termenv          = ty_term ShapeTable.t

type tenv =
   { tenv_typeclasses    : typeclasses;
     tenv_typereduce     : typereduce;
     tenv_typereductions : typereductions;
     tenv_typeenv        : typeenv;
     tenv_termenv        : termenv
   }

type ty =
   TypeVar      of var
 | TypeTerm     of term
 | TypeSoVar    of ty list * ty list * ty         (* sovar: context vars, arguments, result *)
 | TypeCVar     of ty list * ty list * ty * ty    (* context: context vars, arguments, hole, result *)
 | TypeSCVar    of ty list * ty list * ty         (* sequent context: context vars, arguments, ty_var * ty_hyp *)
 | TypeHyp      of ty * ty                        (* ty_var, ty_hyp *)
 | TypeExists   of var * ty * ty                  (* Universal quantifier *)
 | TypeHypCases of (ty * ty) list                 (* Hypothesis cases *)
 | TypeSequent  of ty * ty * ty                   (* hyps, concl, result *)

(*
 * Types of error info.
 *)
type unify_info =
   UnifyVar of var
 | UnifyType of ty
 | UnifyTerm of term
 | UnifyTermType2 of term * ty * ty
 | UnifyContextVarType2 of var * ty * ty
 | UnifyCompose of unify_info * unify_info

type venv = ty SymbolTable.t

(*
 * Constraints that should be solved once the main inference
 * is done.
 *)
type unify_constraint =
   ConstraintSubtype  of unify_info * term * term
 | ConstraintEqual    of unify_info * term * term
 | ConstraintMember   of unify_info * term * ty                 (* type constraint *)
 | ConstraintCase     of unify_info * ty * ty * (ty * ty) list  (* hypothesis case *)

type senv =
   { senv_subst       : ty SymbolTable.t;
     senv_constraints : unify_constraint list;
     senv_termenv     : ty SymbolTable.t;
     senv_mode        : strict
   }

(************************************************
 * Utilities.
 *)

(*
 * Opnames.
 *)
let perv_opname = mk_opname "Perv" nil_opname
let mk_perv_opname s =
   mk_opname s perv_opname

(*
 * Types.
 *)
let mk_typeclass name =
   let opname = mk_perv_opname name in
   let t = mk_term (mk_op opname []) [] in
      opname, t

let _, top_term = mk_typeclass "Top"

let term_opname,         term_type           = mk_typeclass "Term"
let type_opname,         type_type           = mk_typeclass "Ty"
let token_opname,        token_type          = mk_typeclass "Token"
let judgment_opname,     judgment_type       = mk_typeclass "Judgment"
let quote_opname,        quote_type          = mk_typeclass "Quote"
let dform_opname,        dform_type          = mk_typeclass "Dform"
let nonterminal_opname,  nonterminal_type    = mk_typeclass "Nonterminal"
let ignore_opname,       ignore_type         = mk_typeclass "Ignore"

let ty_type        = TypeTerm type_type
let ty_term        = TypeTerm term_type
let ty_judgment    = TypeTerm judgment_type
let ty_dform       = TypeTerm dform_type
let ty_nonterminal = TypeTerm nonterminal_type

(*
 * Create a term from the type.
 * This is just for error messages.
 *)
let so_cvars_opname = mk_perv_opname "ty_so_cvars"
let so_args_opname  = mk_perv_opname "ty_so_args"
let so_var_opname   = mk_perv_opname "ty_so_var"

let context_cvars_opname = mk_perv_opname "ty_context_cvars"
let context_args_opname  = mk_perv_opname "ty_context_args"
let context_var_opname   = mk_perv_opname "ty_context_var"

let sequent_context_cvars_opname = mk_perv_opname "ty_sequent_context_cvars"
let sequent_context_args_opname  = mk_perv_opname "ty_sequent_context_args"
let sequent_context_var_opname   = mk_perv_opname "ty_sequent_context_var"

let sequent_opname   = mk_perv_opname "ty_sequent"
let hyp_opname       = mk_perv_opname "ty_hyp"
let hyp_cases_opname = mk_perv_opname "ty_hyp_cases"
let hyp_case_opname  = mk_perv_opname "ty_hyp_case"
let exists_opname    = mk_perv_opname "ty_exists"

let ty_constant_opname = mk_perv_opname "type"

(*
 * Sequent types.
 *)
let mk_ty_hyp_term =
   mk_dep0_dep0_term hyp_opname

let dest_ty_hyp_term =
   dest_dep0_dep0_term hyp_opname

let is_ty_hyp_term =
   is_dep0_dep0_term hyp_opname

let mk_ty_hyp_case_term =
   mk_dep0_dep0_term hyp_case_opname

let dest_ty_hyp_case_term =
   dest_dep0_dep0_term hyp_case_opname

let is_ty_hyp_case_term =
   is_dep0_dep0_term hyp_case_opname

let mk_ty_hyp_cases_term cases =
   mk_dep0_term hyp_cases_opname (mk_xlist_term (List.map (fun (ty_var, ty_hyp) -> mk_ty_hyp_case_term ty_var ty_hyp) cases))

let dest_ty_hyp_cases_term t =
   let cases = dest_dep0_term hyp_cases_opname t in
   let cases = dest_xlist cases in
      List.map dest_ty_hyp_case_term cases

let is_ty_hyp_cases_term t =
   if is_dep0_term hyp_cases_opname t then
      let cases = one_subterm t in
         is_xlist_term cases && List.for_all is_ty_hyp_case_term (dest_xlist cases)
   else
      false

let rec mk_ty_exists_term =
   mk_dep0_dep1_term exists_opname

let dest_ty_exists_term =
   dest_dep0_dep1_term exists_opname

let is_ty_exists_term =
   is_dep0_dep1_term exists_opname

let mk_ty_sequent_term =
   mk_dep0_dep0_dep0_term sequent_opname

let dest_ty_sequent_term =
   dest_dep0_dep0_dep0_term sequent_opname

let is_ty_sequent_term =
   is_dep0_dep0_dep0_term sequent_opname

let dest_ty_sequent_cases t =
   let ty_hyps, ty_concl, ty_seq = dest_ty_sequent_term t in
      if is_ty_hyp_term ty_hyps then
         let ty_var, ty_hyp = dest_ty_hyp_term ty_hyps in
            [ty_var, ty_hyp], ty_concl, ty_seq
      else
         let cases = dest_ty_hyp_cases_term ty_hyps in
            cases, ty_concl, ty_seq

(*
 * Type constraints.
 *)
let constrain_opname = mk_perv_opname "ty_constrain"

let is_ty_constrain_term =
   is_dep0_dep0_term constrain_opname

let mk_ty_constrain_term =
   mk_dep0_dep0_term constrain_opname

let dest_ty_constrain_term =
   dest_dep0_dep0_term constrain_opname

(*
 * Constant terms.
 *)
let is_ty_constant_term =
   is_var_param_term ty_constant_opname

let mk_ty_constant_term =
   mk_var_param_term ty_constant_opname

let dest_ty_constant_term =
   dest_var_param_term ty_constant_opname

(*
 * Constructing terms from types.
 *)
let rec term_of_ty ty =
   match ty with
      TypeVar v ->
         mk_var_term v
    | TypeTerm t ->
         t
    | TypeSoVar (ty_cvars, ty_args, ty_res) ->
         let cvars = mk_dep0_term so_cvars_opname (mk_xlist_term (List.map term_of_ty ty_cvars)) in
         let args = mk_dep0_term so_args_opname (mk_xlist_term (List.map term_of_ty ty_args)) in
            mk_simple_term so_var_opname [cvars; args; term_of_ty ty_res]
    | TypeCVar (ty_cvars, ty_args, ty_exp, ty_res) ->
         let cvars = mk_dep0_term context_cvars_opname (mk_xlist_term (List.map term_of_ty ty_cvars)) in
         let args = mk_dep0_term context_args_opname (mk_xlist_term (List.map term_of_ty ty_args)) in
            mk_simple_term context_var_opname [cvars; args; term_of_ty ty_exp; term_of_ty ty_res]
    | TypeSCVar (ty_cvars, ty_args, ty_hyp) ->
         let cvars = mk_dep0_term sequent_context_cvars_opname (mk_xlist_term (List.map term_of_ty ty_cvars)) in
         let args = mk_dep0_term sequent_context_args_opname (mk_xlist_term (List.map term_of_ty ty_args)) in
         let hyp = term_of_ty ty_hyp in
            mk_simple_term sequent_context_var_opname [cvars; args; hyp]
    | TypeHyp (ty_var, ty_hyp) ->
         mk_ty_hyp_term (term_of_ty ty_var) (term_of_ty ty_hyp)
    | TypeHypCases cases ->
         mk_ty_hyp_cases_term (List.map (fun (ty_var, ty_hyp) -> term_of_ty ty_var, term_of_ty ty_hyp) cases)
    | TypeExists (v, ty_bound, ty) ->
         mk_ty_exists_term v (term_of_ty ty_bound) (term_of_ty ty)
    | TypeSequent (ty_hyp, ty_concl, ty_seq) ->
         mk_ty_sequent_term (term_of_ty ty_hyp) (term_of_ty ty_concl) (term_of_ty ty_seq)

(*
 * Shapes of the noncanonical terms.
 *)
let v_sym = Lm_symbol.add "v"
let type_var = TypeVar v_sym

let ty_var_shape       = shape_of_term (term_of_ty type_var)
let ty_term_shape      = shape_of_term (term_of_ty (TypeTerm (mk_var_term v_sym)))
let ty_so_var_shape    = shape_of_term (term_of_ty (TypeSoVar ([], [], type_var)))
let ty_cvar_shape      = shape_of_term (term_of_ty (TypeCVar ([], [], type_var, type_var)))
let ty_scvar_shape     = shape_of_term (term_of_ty (TypeSCVar ([], [], type_var)))
let ty_hyp_shape       = shape_of_term (term_of_ty (TypeHyp (type_var, type_var)))
let ty_hyp_cases_shape = shape_of_term (term_of_ty (TypeHypCases []))
let ty_exists_shape    = shape_of_term (term_of_ty (TypeExists (v_sym, type_var, type_var)))
let ty_sequent_shape   = shape_of_term (term_of_ty (TypeSequent (type_var, type_var, type_var)))

let shape_of_type = function
   TypeVar _      -> ty_var_shape
 | TypeTerm _     -> ty_term_shape
 | TypeSoVar _    -> ty_so_var_shape
 | TypeCVar _     -> ty_cvar_shape
 | TypeSCVar _    -> ty_scvar_shape
 | TypeHyp _      -> ty_hyp_shape
 | TypeHypCases _ -> ty_hyp_cases_shape
 | TypeExists _   -> ty_exists_shape
 | TypeSequent _  -> ty_sequent_shape

(************************************************
 * Type environment.
 *)

(*
 * Construct the quoted type from the normal type.
 *
 * XXX: BUG? Nogin: Currently we change the type to be Perv!Quote (for all the
 * bindings, for all the subterms for the resulting type). However, this is too coarse
 * and may be a better approach would be to do "ty -> Quote{ty}" instead of just "ty -> Quote".
 *)
let quote_bterm_type bterm =
   let { ty_bvars = bvars; ty_bterm = term } = bterm in
      { ty_bvars = List.map (fun _ -> quote_type) bvars;
        ty_bterm = quote_type
      }

let quote_type ty =
   let { ty_term   = t;
         ty_opname = opname;
         ty_params = params;
         ty_bterms = bterms;
         ty_type   = ty
       } = ty
   in
      { ty_term   = t;
        ty_opname = opname;
        ty_params = TyQuote :: params;
        ty_bterms = List.map quote_bterm_type bterms;
        ty_type   = quote_type
      }

(*
 * See if a typeclass name is bound.
 *)
let tenv_typeclasses_mem tenv opname =
   OpnameTable.mem tenv.tenv_typeclasses opname

(*
 * Type environment.
 *)
let tenv_find_typeclasses tenv shape =
   let opname =
      try ShapeTable.find tenv.tenv_typeenv shape with
         Not_found ->
            term_opname
   in
      try OpnameTable.find tenv.tenv_typeclasses opname with
         Not_found ->
            raise (RefineError ("Term_ty_infer.tenv_find_typeclasses: unknown typeclass", OpnameError opname))

let tenv_find_type_aux tenv t =
   let shape = shape_of_term t in
      try ShapeTable.find tenv.tenv_termenv shape with
         Not_found ->
            raise (RefineError ("Term_ty_infer.tenv_find_type: unknown term", ShapeError shape))

let tenv_find_type tenv t =
   if is_quoted_term t then
      quote_type (tenv_find_type_aux tenv (unquote_term t))
   else
      tenv_find_type_aux tenv t

let tenv_find_class tenv t =
   let shape = shape_of_term t in
      eprintf "tenv_find_class: shape = %s@." (string_of_shape shape);
      try
         let opname = ShapeTable.find tenv.tenv_typeenv shape in
            eprintf "tenv_find_class: opname class: %s@." (string_of_opname opname);
            OpnameTable.find tenv.tenv_typeclasses opname
      with
         Not_found ->
            raise (RefineError ("Term_ty_infer.tenv_find_class: unbound class", TermError t))

(*
 * Look for a possible reduction pair.
 *)
let tenv_find_reduction tenv shape1 shape2 =
   try Some (Shape2Table.find tenv.tenv_typereductions (shape1, shape2)) with
      Not_found ->
         try
            let term2, term1 = Shape2Table.find tenv.tenv_typereductions (shape2, shape1) in
               Some (term1, term2)
         with
            Not_found ->
               None

(*
 * To be a typeclass, the term needs to be a simple opname
 * that represents a typeclass.
 *)
let is_typeclass_term tenv term =
   let { term_op = op; term_terms = bterms } = dest_term term in
   let { op_name = opname; op_params = params } = dest_op op in
      match bterms, params with
         [], [] ->
            OpnameTable.mem tenv.tenv_typeclasses opname
       | _ ->
            false

(************************************************
 * Variable environment.
 *)
let venv_empty = SymbolTable.empty

let venv_add_var = SymbolTable.add

let venv_mem_var = SymbolTable.mem

let venv_add_vars venv vars tyl =
   List.fold_left2 (fun venv v ty ->
         SymbolTable.add venv v (TypeTerm ty)) venv vars tyl

let venv_find_var venv v =
   match
      try SymbolTable.find venv v with
         Not_found ->
            raise (RefineError ("Term_ty_infer.venv_find_var: unbound variable", RewriteFreeSOVar v))
   with
      TypeTerm ty when alpha_equal ty ignore_type ->
         raise (RefineError ("Term_ty_infer.venv_find_var", StringVarError("free occurrence of an \"Ignore\" variable", v)))
    | ty ->
         ty

let venv_find_var_opt venv v =
   match
      try Some (SymbolTable.find venv v) with
         Not_found ->
            None
   with
      Some (TypeTerm ty) when alpha_equal ty ignore_type ->
         raise (RefineError ("Term_ty_infer.venv_find_var_opt", StringVarError("free occurrence of an \"Ignore\" variable", v)))
    | ty ->
         ty

(************************************************
 * Substitution environment.
 *)

(*
 * Check that the variable is not free in the type.
 *)
let new_subst mode =
   { senv_subst       = SymbolTable.empty;
     senv_constraints = [];
     senv_termenv     = SymbolTable.empty;
     senv_mode        = mode
   }

let subst_add_var subst v ty =
   { subst with senv_subst = SymbolTable.add subst.senv_subst v ty }

let subst_add_constraint subst con =
   { subst with senv_constraints = con :: subst.senv_constraints }

let subst_find_opt subst v =
   try Some (SymbolTable.find subst.senv_subst v) with
      Not_found ->
         None

let subst_add_type subst v ty =
   { subst with senv_termenv = SymbolTable.add subst.senv_termenv v ty }

let subst_find_type subst v =
   try SymbolTable.find subst.senv_termenv v with
      Not_found ->
         raise (Invalid_argument ("Term_ty_infer.subst_find_type: bad type: " ^ string_of_symbol v))

(*
 * Apply the substitution to a term.
 *)
let rec subst_var subst vars v =
   if SymbolSet.mem vars v then
      raise (RefineError ("Term_ty_infer.subst_term", StringVarError ("occurs check", v)));
   let vars = SymbolSet.add vars v in
      match subst_find_opt subst v with
         Some ty ->
            subst_type subst vars ty
       | _ ->
            TypeVar v

and subst_var_term subst vars v =
   term_of_ty (subst_var subst vars v)

and subst_term subst vars t =
   if is_fso_var_term t then
      subst_var_term subst vars (dest_fso_var t)
   else if is_so_var_term t then
      let v, cvars, args = dest_so_var t in
      let args = List.map (subst_term subst vars) args in
         mk_so_var_term v cvars args
   else if is_context_term t then
      let v, arg, cvars, args = dest_context t in
      let args = List.map (subst_term subst vars) args in
      let arg = subst_term subst vars arg in
         mk_context_term v arg cvars args
   else if is_sequent_term t then
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_concl = concl
          } = explode_sequent t
      in
      let arg = subst_term subst vars arg in
      let hyps =
         SeqHyp.map (fun hyp ->
               match hyp with
                  Hypothesis (v, t) ->
                     Hypothesis (v, subst_term subst vars t)
                | Context (v, cvars, args) ->
                     Context (v, cvars, List.map (subst_term subst vars) args)) hyps
      in
      let concl = subst_term subst vars concl in
      let seq =
         { sequent_args = arg;
           sequent_hyps = hyps;
           sequent_concl = concl
         }
      in
         mk_sequent_term seq
   else
      let { term_op = op; term_terms = bterms } = dest_term t in
      let bterms = List.map (subst_bterm subst vars t) bterms in
         mk_term op bterms

and subst_bterm subst vars ty bterm =
   let { bvars = bvars; bterm = t } = dest_bterm bterm in
      mk_bterm bvars (subst_term subst vars t)

and subst_type subst vars ty =
   match ty with
      TypeVar v ->
         subst_var subst vars v
    | TypeTerm t ->
         TypeTerm (subst_term subst vars t)
    | TypeSoVar (cvars, args, ty) ->
         TypeSoVar (subst_type_list subst vars cvars,
                    subst_type_list subst vars args,
                    subst_type subst vars ty)
    | TypeCVar (cvars, args, ty_exp, ty) ->
         TypeCVar (subst_type_list subst vars cvars,
                   subst_type_list subst vars args,
                   subst_type subst vars ty_exp,
                   subst_type subst vars ty)
    | TypeSCVar (cvars, args, ty_hyp) ->
         TypeSCVar (subst_type_list subst vars cvars,
                    subst_type_list subst vars args,
                    subst_type subst vars ty_hyp)
    | TypeHyp (ty_var, ty_hyp) ->
         TypeHyp (subst_type subst vars ty_var, subst_type subst vars ty_hyp)
    | TypeHypCases cases ->
         TypeHypCases (subst_type_cases subst vars cases)
    | TypeExists (v, ty1, ty2) ->
         TypeExists (v, subst_type subst vars ty1, subst_type subst vars ty2)
    | TypeSequent (ty_hyp, ty_concl, ty_seq) ->
         TypeSequent (subst_type subst vars ty_hyp,
                      subst_type subst vars ty_concl,
                      subst_type subst vars ty_seq)

and subst_type_list subst vars tyl =
   List.map (subst_type subst vars) tyl

and subst_type_cases subst vars cases =
   List.map (fun (ty_var, ty_hyp) ->
         subst_type subst vars ty_var, subst_type subst vars ty_hyp) cases

(* Toplevel versions *)
let subst_var_term subst v =
   subst_var_term subst SymbolSet.empty v

let subst_term subst term =
   subst_term subst SymbolSet.empty term

let subst_type subst ty =
   subst_type subst SymbolSet.empty ty

let subst_type_term subst ty =
   term_of_ty (subst_type subst ty)

(************************************************
 * Errors.
 *)

(*
 * Wrap an error message with the info.
 *)
let rec wrap_unify_error subst info err =
   match info with
      UnifyVar v ->
         VarErrorError (v, err)
    | UnifyType ty ->
         TermErrorError (subst_type_term subst ty, err)
    | UnifyTerm t ->
         StringErrorError ("while typechecking",
         TermErrorError (subst_term subst t, err))
    | UnifyTermType2 (t, ty1, ty2) ->
         TermErrorError (t,
         StringErrorError ("has type",
         TermErrorError (subst_type_term subst ty1,
         StringErrorError ("but is used with type",
         TermErrorError (subst_type_term subst ty2,
         err)))))
    | UnifyContextVarType2 (v, ty1, ty2) ->
         StringErrorError ("context variable <" ^ string_of_symbol v ^ ">",
         StringErrorError ("has type",
         TermErrorError (subst_type_term subst ty1,
         StringErrorError ("but is used with type",
         TermErrorError (subst_type_term subst ty2,
         err)))))
    | UnifyCompose (info1, info2) ->
         wrap_unify_error subst info1 (wrap_unify_error subst info2 err)

let raise_err subst info err =
   raise (RefineError ("type error", wrap_unify_error subst info err))

(*
 * Error messages.
 *)
let raise_type2_error subst info ty1 ty2 =
   let err =
      StringErrorError ("type",
      TermErrorError (subst_type_term subst ty1,
      StringErrorError ("is not compatible with",
      TermError (subst_type_term subst ty2))))
   in
      raise_err subst info err

let raise_case_error subst info ty1 ty2 =
   let err =
      StringErrorError ("hypothesis var",
      TermErrorError (subst_type_term subst ty1,
      StringErrorError ("is not compatible with hypothesis type",
      TermError (subst_type_term subst ty2))))
   in
      raise_err subst info err

let raise_term2_error debug subst info t1 t2 =
   let err =
      StringErrorError (debug,
      StringErrorError ("term type",
      TermErrorError (subst_term subst t1,
      StringErrorError ("is not compatible with",
      TermError (subst_term subst t2)))))
   in
      raise_err subst info err

let raise_term_type_error subst info t ty =
   let err =
      StringErrorError ("has term type",
      TermErrorError (subst_term subst t,
      StringErrorError ("but is used with type",
      TermError (subst_type_term subst ty))))
   in
      raise_err subst info err

let raise_param2_error subst info p1 p2 =
   raise_err subst info (StringErrorError ("parameter mismatch", Param2Error (p1, p2)))

let raise_illegal_term_error subst info t =
   raise_err subst info (StringErrorError ("illegal type", TermError t))

let raise_expand_type_error subst info s ty =
   raise_err subst info (StringErrorError (s, TermError (term_of_ty ty)))

(************************************************
 * Type expansion, eliminating variables.
 *)

(*
 * Normalize the type representations.
 *)
let rec normalize_term info subst t =
   if is_fso_var_term t then
      subst, TypeVar (dest_fso_var t)
   else if is_ty_constrain_term t then
      let t, ty = dest_ty_constrain_term t in
      let subst = subst_add_constraint subst (ConstraintMember (info, t, TypeTerm ty)) in
         normalize_term info subst t
   else if is_ty_sequent_term t then
      let ty_hyp, ty_concl, ty_seq = dest_ty_sequent_term t in
      let subst, ty_hyp = normalize_term info subst ty_hyp in
      let subst, ty_concl = normalize_term info subst ty_concl in
      let subst, ty_seq = normalize_term info subst ty_seq in
         subst, TypeSequent (ty_hyp, ty_concl, ty_seq)
   else if is_ty_exists_term t then
      let v, t1, t2 = dest_ty_exists_term t in
      let subst, t1 = normalize_term info subst t1 in
      let subst, t2 = normalize_term info subst t2 in
         subst, TypeExists (v, t1, t2)
   else if is_ty_hyp_term t then
      let ty_var, ty_hyp = dest_ty_hyp_term t in
      let subst, ty_var = normalize_term info subst ty_var in
      let subst, ty_hyp = normalize_term info subst ty_hyp in
         subst, TypeHyp (ty_var, ty_hyp)
   else if is_ty_hyp_cases_term t then
      let cases = dest_ty_hyp_cases_term t in
      let subst, cases =
         List.fold_left (fun (subst, cases) (ty_var, ty_hyp) ->
               let subst, ty_var = normalize_term info subst ty_var in
               let subst, ty_hyp = normalize_term info subst ty_hyp in
               let cases = (ty_var, ty_hyp) :: cases in
                  subst, cases) (subst, []) cases
      in
         subst, TypeHypCases (List.rev cases)
   else
      subst, TypeTerm t

let normalize_type info subst ty =
   match ty with
      TypeTerm t ->
         normalize_term info subst t
    | _ ->
         subst, ty

(*
 * The type should be a hyp type.
 *)
let rec expand_hyp_type_aux info subst ty =
   let subst, ty = normalize_type info subst ty in
      match ty with
         TypeVar v ->
            (match subst_find_opt subst v with
                Some ty ->
                   expand_hyp_type_aux info subst ty
              | None ->
                   let ty_var = TypeVar (new_symbol_string "sequent-var") in
                   let ty_hyp = TypeVar (new_symbol_string "sequent-hyp") in
                   let ty = TypeHyp (ty_var, ty_hyp) in
                      subst_add_var subst v ty, ty)
       | ty ->
            subst, ty

let expand_hyp_type info subst ty hyp =
   let rec expand subst vars ty =
      let subst, ty = expand_hyp_type_aux info subst ty in
         match ty with
            TypeExists (v, ty_var, ty) ->
               expand subst ((v, ty_var) :: vars) ty
          | TypeHyp (ty_var, ty_hyp) ->
               subst, List.rev vars, ty_var, ty_hyp
          | ty ->
               raise_expand_type_error subst info "not a legal hypothesis type" ty
   in
   let subst, ty = expand_hyp_type_aux info subst ty in
      match ty with
         TypeExists (v, ty_var, ty) ->
            expand subst [v, ty_var] ty
       | TypeHyp (ty_var, ty_hyp) ->
            subst, [], ty_var, ty_hyp
       | TypeHypCases cases ->
            let ty_var = TypeVar (new_symbol_string "hyp-var") in
            let ty_hyp = TypeVar (new_symbol_string "hyp-type") in
            let subst = subst_add_constraint subst (ConstraintCase (info, ty_var, ty_hyp, cases)) in
               subst, [], ty_var, ty_hyp
       | ty ->
            raise_expand_type_error subst info "not a legal hypothesis type" ty

(************************************************
 * Free vars and substitution.
 *)

(*
 * Free vars of a type.
 *)
let rec free_vars_type fv ty =
   match ty with
      TypeVar v ->
         SymbolSet.add fv v
    | TypeTerm t ->
         SymbolSet.union fv (free_vars_set t)
    | TypeSoVar (cvars, args, ty)
    | TypeSCVar (cvars, args, ty) ->
         free_vars_type_list (free_vars_type_list (free_vars_type fv ty) args) cvars
    | TypeCVar (cvars, args, ty_exp, ty_res) ->
         free_vars_type_list (free_vars_type_list (free_vars_type (free_vars_type fv ty_res) ty_exp) args) cvars
    | TypeHyp (ty_var, ty_hyp) ->
         free_vars_type (free_vars_type fv ty_hyp) ty_var
    | TypeHypCases cases ->
         List.fold_left (fun fv (ty_var, ty_hyp) ->
               free_vars_type (free_vars_type fv ty_var) ty_hyp) fv cases
    | TypeExists (v, ty1, ty2) ->
         let fv2 = free_vars_type SymbolSet.empty ty2 in
         let fv2 = SymbolSet.remove fv2 v in
         let fv = SymbolSet.union fv fv2 in
            free_vars_type fv ty1
    | TypeSequent (ty_hyp, ty_concl, ty_seq) ->
         free_vars_type (free_vars_type (free_vars_type fv ty_hyp) ty_concl) ty_seq

and free_vars_type_list fv tyl =
   List.fold_left free_vars_type fv tyl

let free_vars_subst venv senv =
   let subst = senv.senv_subst in
   let fv = SymbolSet.empty in
   let fv =
      SymbolTable.fold (fun fv _ ty ->
            free_vars_type fv ty) fv venv
   in
   let fv =
      SymbolTable.fold (fun fv _ ty ->
            free_vars_type fv ty) fv subst
   in
   let fv =
      SymbolTable.fold (fun fv v _ ->
            SymbolSet.remove fv v) fv subst
   in
      fv

(*
 * Substitute for a variable in the subst.
 *)
let type_subst v t ty =
   let rec subst ty =
      match ty with
         TypeVar v' ->
            if Lm_symbol.eq v' v then
               TypeTerm t
            else
               ty
       | TypeTerm t' ->
            TypeTerm (subst1 t' v t)
       | TypeSoVar (cvars, args, ty) ->
            TypeSoVar (List.map subst cvars, List.map subst args, subst ty)
       | TypeCVar (cvars, args, ty_exp, ty) ->
            TypeCVar (List.map subst cvars, List.map subst args, subst ty_exp, subst ty)
       | TypeSCVar (cvars, args, ty_hyp) ->
            TypeSCVar (List.map subst cvars, List.map subst args, subst ty_hyp)
       | TypeHyp (ty_var, ty_hyp) ->
            TypeHyp (subst ty_var, subst ty_hyp)
       | TypeHypCases cases ->
            TypeHypCases (List.map (fun (ty_var, ty_hyp) -> subst ty_var, subst ty_hyp) cases)
       | TypeExists (v', ty_var, ty) ->
            let ty = if Lm_symbol.eq v' v then ty else subst ty in
               TypeExists (v, subst ty_var, subst ty)
       | TypeSequent (ty_hyp, ty_concl, ty_seq) ->
            TypeSequent (subst ty_hyp, subst ty_concl, subst ty_seq)
   in
      subst ty

(*
 * Replace the variable v1 with a constant of type ty_var.
 *)
let instantiate_ty_var subst v1 ty_var ty =
   let v2 = new_symbol v1 in
   let t2 = mk_ty_constant_term v2 in
   let subst = subst_add_type subst v2 ty_var in
   let ty = type_subst v1 t2 ty in
      subst, ty

(*
 * Instantiate all free type variables in the substitution.
 *)
let instantiate_subst venv subst =
   SymbolSet.fold (fun subst v1 ->
         let v2 = new_symbol v1 in
         let t2 = mk_ty_constant_term v2 in
         let ty_kind =
            match venv_find_var_opt venv v1 with
               Some ty ->
                  ty
             | None ->
                  ty_type
         in
         let subst = subst_add_var subst v1 (TypeTerm t2) in
         let subst = subst_add_type subst v2 ty_kind in
            subst) subst (free_vars_subst venv subst)

(************************************************
 * Standardize.
 *)

(*
 * Standardize a reduction.
 *)
let standardize_reduction redex contractum =
   let fv = SymbolSet.union (free_vars_set redex) (free_vars_set contractum) in
      if SymbolSet.is_empty fv then
         redex, contractum
      else
         let subst =
            List.map (fun v -> v, mk_var_term (new_symbol v)) (SymbolSet.to_list fv)
         in
            apply_subst subst redex, apply_subst subst contractum

(*
 * Rename all free first-order variables in the ty_term.
 * Ignore the actual term.
 *)
let standardize_ty_term ty_term =
   let { ty_params = params;
         ty_bterms = bterms;
         ty_type   = ty
       } = ty_term
   in

   (* Get all the free first-order vars *)
   let fv = SymbolSet.empty in
   let fv =
      List.fold_left (fun fv param ->
            match param with
               TyToken t ->
                  SymbolSet.union fv (free_vars_set t)
             | TyNumber
             | TyString
             | TyShape
             | TyOperator
             | TyLevel
             | TyVar
             | TyQuote ->
                  fv) fv params
   in
   let fv =
      List.fold_left (fun fv { ty_bvars = bvars; ty_bterm = t } ->
            let fv = SymbolSet.union fv (free_vars_terms bvars) in
               SymbolSet.union fv (free_vars_set t)) fv bterms
   in
   let fv = SymbolSet.union fv (free_vars_set ty) in
      if SymbolSet.is_empty fv then
         ty_term
      else
         (* Construct a substitution *)
         let subst =
            List.map (fun v -> v, mk_var_term (new_symbol v)) (SymbolSet.to_list fv)
         in

         (* New parts *)
         let params =
            List.map (fun param ->
                  match param with
                     TyToken t ->
                        TyToken (apply_subst subst t)
                   | TyNumber
                   | TyShape
                   | TyOperator
                   | TyString
                   | TyLevel
                   | TyVar
                   | TyQuote ->
                        param) params
         in
         let bterms =
            List.map (fun { ty_bvars = bvars; ty_bterm = t } ->
                  { ty_bvars = List.map (apply_subst subst) bvars;
                    ty_bterm = apply_subst subst t
                  }) bterms
         in
            { ty_term with ty_params = params;
                           ty_bterms = bterms;
                           ty_type   = apply_subst subst ty
            }

(*
 * Simple substitution of one variable for another.
 * The variable v2 is new, so capture is not possible.
 *)
let standardize_ty_var info subst v1 ty_bound ty =
   let v2 = new_symbol v1 in
   let t2 = mk_var_term v2 in
   let rec standardize ty =
      match ty with
         TypeVar v ->
            if Lm_symbol.eq v v1 then
               TypeVar v2
            else
               ty
       | TypeTerm t ->
            TypeTerm (subst1 t v1 t2)
       | TypeSoVar (cvars, args, ty) ->
            TypeSoVar (List.map standardize cvars, List.map standardize args, standardize ty)
       | TypeCVar (cvars, args, ty_exp, ty) ->
            TypeCVar (List.map standardize cvars, List.map standardize args, standardize ty_exp, standardize ty)
       | TypeSCVar (cvars, args, ty_hyp) ->
            TypeSCVar (List.map standardize cvars, List.map standardize args, standardize ty_hyp)
       | TypeHyp (ty_var, ty_hyp) ->
            TypeHyp (standardize ty_var, standardize ty_hyp)
       | TypeHypCases cases ->
            TypeHypCases (List.map (fun (ty_var, ty_hyp) -> standardize ty_var, standardize ty_hyp) cases)
       | TypeExists (v, ty_bound, ty) ->
            TypeExists (v, standardize ty_bound, standardize ty)
       | TypeSequent (ty_hyp, ty_concl, ty_seq) ->
            TypeSequent (standardize ty_hyp, standardize ty_concl, standardize ty_seq)
   in
   let info = UnifyCompose (info, UnifyVar v2) in
   let subst = subst_add_constraint subst (ConstraintMember (UnifyCompose (info, UnifyVar v2), t2, ty_bound)) in
      subst, standardize ty

let standardize_ty_hyp info subst vars ty_var ty_hyp =
   if vars = [] then
      subst, ty_var, ty_hyp
   else
      let subst, sub, table =
         List.fold_left (fun (subst, sub, table) (v, ty) ->
               let v' = new_symbol v in
               let t = mk_var_term v' in
               let sub = (v, t) :: sub in
               let table = SymbolTable.add table v v' in
               let subst = subst_add_constraint subst (ConstraintMember (UnifyCompose (info, UnifyVar v'), t, ty)) in
                  subst, sub, table) (subst, [], SymbolTable.empty) vars
      in
      let rec standardize ty =
         match ty with
            TypeVar v ->
               if SymbolTable.mem table v then
                  TypeVar (SymbolTable.find table v)
               else
                  ty
          | TypeTerm t ->
               TypeTerm (apply_subst sub t)
          | TypeSoVar (cvars, args, ty) ->
               TypeSoVar (List.map standardize cvars, List.map standardize args, standardize ty)
          | TypeCVar (cvars, args, ty_exp, ty) ->
               TypeCVar (List.map standardize cvars, List.map standardize args, standardize ty_exp, standardize ty)
          | TypeSCVar (cvars, args, ty_hyp) ->
               TypeSCVar (List.map standardize cvars, List.map standardize args, standardize ty_hyp)
          | TypeHyp (ty_var, ty_hyp) ->
               TypeHyp (standardize ty_var, standardize ty_hyp)
          | TypeHypCases cases ->
               TypeHypCases (List.map (fun (ty_var, ty_hyp) -> standardize ty_var, standardize ty_hyp) cases)
          | TypeExists (v, ty_var, ty) ->
               TypeExists (v, standardize ty_var, standardize ty)
          | TypeSequent (ty_hyp, ty_concl, ty_seq) ->
               TypeSequent (standardize ty_hyp, standardize ty_concl, standardize ty_seq)
      in
         subst, standardize ty_var, standardize ty_hyp

(************************************************
 * Type reductions.
 *)
let reduce_type tenv subst t =
   let t = subst_term subst t in
      try
         let t = subst_term subst t in
            match Term_match_table.lookup tenv.tenv_typereduce Term_match_table.select_all t with
               Some rw ->
                  begin match Rewrite.apply_rewrite rw empty_args t [] with
                     [contractum] ->
                        Some contractum
                   | _ ->
                        raise (Invalid_argument "Term_ty_infer.reduce_type: reduction produced multiple contracta")
                  end
             | None -> None
      with
         Not_found
       | RefineError _ ->
            None

(************************************************
 * Invariant unification.
 *)

(*
 * The types must really be equal.
 *)
let rec unify_equal_types tenv info subst ty1 ty2 =
   if !debug_infer then
      eprintf "unify_equal_types@.";
   let subst, ty1 = normalize_type info subst ty1 in
   let subst, ty2 = normalize_type info subst ty2 in
      match ty1, ty2 with
         TypeVar v1, TypeVar v2 ->
            if Lm_symbol.eq v1 v2 then
               subst
            else
               (match subst_find_opt subst v1 with
                   Some ty1 ->
                      unify_equal_types tenv info subst ty1 ty2
                 | None ->
                      match subst_find_opt subst v2 with
                         Some ty2 ->
                            unify_equal_types tenv info subst ty1 ty2
                       | None ->
                            subst_add_var subst v1 ty2)
       | TypeVar v1, _ ->
            (match subst_find_opt subst v1 with
                Some ty1 ->
                   unify_equal_types tenv info subst ty1 ty2
              | None ->
                   subst_add_var subst v1 ty2)
       | _, TypeVar v2 ->
            (match subst_find_opt subst v2 with
                Some ty2 ->
                   unify_equal_types tenv info subst ty1 ty2
              | None ->
                   subst_add_var subst v2 ty1)

       | TypeTerm t1, TypeTerm t2 ->
            unify_equal_term_types_normalized tenv info subst t1 t2

       | TypeSoVar (ty_cvars1, ty_vars1, ty_res1), TypeSoVar (ty_cvars2, ty_vars2, ty_res2)
         when List.length ty_cvars1 = List.length ty_cvars2 && List.length ty_vars1 = List.length ty_vars2 ->
            let subst = unify_equal_type_lists tenv info subst ty_cvars2 ty_cvars1 in
            let subst = unify_equal_type_lists tenv info subst ty_vars2 ty_vars1 in
               unify_equal_types tenv info subst ty_res1 ty_res2

       | TypeCVar (ty_cvars1, ty_vars1, ty_exp1, ty_res1), TypeCVar (ty_cvars2, ty_vars2, ty_exp2, ty_res2)
         when List.length ty_cvars1 = List.length ty_cvars2 && List.length ty_vars1 = List.length ty_vars2 ->
            let subst = unify_equal_type_lists tenv info subst ty_cvars2 ty_cvars1 in
            let subst = unify_equal_type_lists tenv info subst ty_vars2 ty_vars1 in
            let subst = unify_equal_types tenv info subst ty_exp2 ty_exp1 in
               unify_equal_types tenv info subst ty_res1 ty_res2

       | TypeSCVar (ty_cvars1, ty_vars1, ty_hyp1), TypeSCVar (ty_cvars2, ty_vars2, ty_hyp2)
         when List.length ty_cvars1 = List.length ty_cvars2 && List.length ty_vars1 = List.length ty_vars2 ->
            let subst = unify_equal_type_lists tenv info subst ty_cvars2 ty_cvars1 in
            let subst = unify_equal_type_lists tenv info subst ty_vars2 ty_vars1 in
               unify_equal_types tenv info subst ty_hyp1 ty_hyp2

       | TypeSequent (ty_hyp1, ty_concl1, ty_seq1), TypeSequent (ty_hyp2, ty_concl2, ty_seq2) ->
            let subst = unify_equal_types tenv info subst ty_hyp1 ty_hyp2 in
            let subst = unify_equal_types tenv info subst ty_concl1 ty_concl2 in
               unify_equal_types tenv info subst ty_seq1 ty_seq2

       | TypeHyp (ty_var1, ty_hyp1), TypeHyp (ty_var2, ty_hyp2) ->
            let subst = unify_equal_types tenv info subst ty_hyp1 ty_hyp2 in
               unify_equal_types tenv info subst ty_var1 ty_var2

       | TypeHypCases cases1, TypeHypCases cases2
         when List.length cases1 = List.length cases2 ->
            unify_equal_case_lists tenv info subst cases1 cases2

       | TypeExists (v1, ty_bound1, ty1), TypeExists (v2, ty_bound2, ty2) ->
            let subst = unify_equal_types tenv info subst ty_bound1 ty_bound2 in
            let v3 = new_symbol v1 in
            let ty = mk_ty_constant_term v3 in
            let subst = subst_add_type subst v3 ty_bound1 in
            let ty1 = type_subst v1 ty ty1 in
            let ty2 = type_subst v2 ty ty2 in
               unify_equal_types tenv info subst ty1 ty2

       | _ ->
            raise_type2_error subst info ty1 ty2

and unify_equal_type_lists tenv info subst tl1 tl2 =
   List.fold_left2 (unify_equal_types tenv info) subst tl1 tl2

and unify_equal_case_lists tenv info subst cases1 cases2 =
   List.fold_left2 (fun subst (t11, t12) (t21, t22) ->
         let subst = unify_equal_types tenv info subst t12 t22 in
            unify_equal_types tenv info subst t11 t21) subst cases1 cases2

(*
 * Two term types.
 *)
and unify_equal_term_types tenv info subst t1 t2 =
   if !debug_infer then
      eprintf "unify_equal_term_types@.";
   let subst, t1 = normalize_term info subst t1 in
   let subst, t2 = normalize_term info subst t2 in
      unify_equal_types tenv info subst t1 t2

and unify_equal_term_types_normalized tenv info subst t1 t2 =
   if !debug_infer then
      eprintf "@[<hv 3>unify_equal_term_types_normalized:@ %s@ %s@]@." (**)
         (string_of_term t1)
         (string_of_term t2);
   if is_fso_var_term t1 || not (is_fo_term t1) || is_so_var_term t1 || is_context_term t1 || is_sequent_term t1 then
      raise_illegal_term_error subst info t1
   else if is_fso_var_term t2 || not (is_fo_term t2) || is_so_var_term t2 || is_context_term t2 || is_sequent_term t2 then
      raise_illegal_term_error subst info t2
   else
      unify_equal_normal_term_types tenv info true subst t1 t2

and unify_equal_normal_term_types tenv info delayed subst t1 t2 =
   if !debug_infer then
      eprintf "@[<hv 3>unify_equal_normal_term_types:@ %s@ %s@]@." (string_of_term t1) (string_of_term t2);
   let shape1 = shape_of_term t1 in
   let shape2 = shape_of_term t2 in
      if TermShape.eq shape1 shape2 then
         (* The types look the same *)
         let { term_op = op1; term_terms = bterms1 } = dest_term t1 in
         let { op_name = opname1; op_params = params1 } = dest_op op1 in
         let { term_op = op2; term_terms = bterms2 } = dest_term t2 in
         let { op_name = opname2; op_params = params2 } = dest_op op2 in
         let () =
            if List.length params1 <> List.length params2 || List.length bterms1 <> List.length bterms2 then
               raise_term2_error "unify_equal_normal_term_types1" subst info t1 t2
         in
         let subst = unify_equal_param_lists info subst params1 params2 in
            unify_equal_bterm_lists tenv info subst bterms1 bterms2

      else
         unify_equal_reduction tenv info delayed subst t1 t2 shape1 shape2

(* See if there is a reduction pair that matches the shapes *)
and unify_equal_reduction tenv info delayed subst t1 t2 shape1 shape2 =
   let t1 = subst_term subst t1 in
   let t2 = subst_term subst t2 in
      match tenv_find_reduction tenv shape1 shape2 with
         Some (ty1, ty2) ->
            let ty1, ty2 = standardize_reduction ty1 ty2 in
            let subst = unify_equal_normal_term_types tenv info delayed subst t1 ty1 in
            let subst = unify_equal_normal_term_types tenv info delayed subst t2 ty2 in
               subst
       | None ->
            unify_equal_reduce_term_types tenv info delayed subst t1 t2 shape1 shape2

(* Try reducing one of the types *)
and unify_equal_reduce_term_types tenv info delayed subst t1 t2 shape1 shape2 =
   match reduce_type tenv subst t1 with
      Some t1 ->
         unify_equal_normal_term_types tenv info delayed subst t1 t2
    | None ->
         match reduce_type tenv subst t2 with
            Some t2 ->
               unify_equal_normal_term_types tenv info delayed subst t1 t2
          | None ->
               if delayed then
                  subst_add_constraint subst (ConstraintEqual (info, t1, t2))
               else
                  raise_term2_error "unify_equal_normal_term_types2" subst info t1 t2

and unify_equal_params info subst p1 p2 =
   if !debug_infer then
      eprintf "unify_equal_params@.";
   match dest_param p1, dest_param p2 with
      Number i1, Number i2 ->
         if Lm_num.eq_num i1 i2 then
            subst
         else
            raise_param2_error subst info p1 p2
    | String s1, String s2 ->
         if s1 = s2 then
            subst
         else
            raise_param2_error subst info p1 p2
    | Var v1, Var v2 ->
         if Lm_symbol.eq v1 v2 then
            subst
         else
            raise_param2_error subst info p1 p2
    | Token op1, Token op2 ->
         if Opname.eq op1 op2 then
            subst
         else
            raise_param2_error subst info p1 p2
    | Number _, MNumber _
    | MNumber _, Number _
    | MNumber _, MNumber _
    | String _, MString _
    | MString _, String _
    | MString _, MString _
    | Token _, MToken _
    | MToken _, Token _
    | MToken _, MToken _
    | Quote, Quote
    | MLevel _, MLevel _
    | ObId _, ObId _ ->
         subst
    | ParamList pl1, ParamList pl2
      when List.length pl1 = List.length pl2 ->
         unify_equal_param_lists info subst pl1 pl2
    | _ ->
         raise_param2_error subst info p1 p2

and unify_equal_param_lists info subst pl1 pl2 =
   List.fold_left2 (unify_equal_params info) subst pl1 pl2

and unify_equal_bterms tenv info subst bterm1 bterm2 =
   if !debug_infer then
      eprintf "unify_equal_bterms@.";
   let { bvars = bvars1; bterm = term1 } = dest_bterm bterm1 in
   let { bvars = bvars2; bterm = term2 } = dest_bterm bterm2 in
      assert (bvars1 = [] && bvars2 = []);
      unify_equal_term_types tenv info subst term1 term2

and unify_equal_bterm_lists tenv info subst btl1 btl2 =
   List.fold_left2 (unify_equal_bterms tenv info) subst btl1 btl2

(************************************************
 * Subtyping unification.
 *)

let rec unify_subtype_types tenv info subst ty1 ty2 =
   if !debug_infer then
      eprintf "unify_subtype_types@.";
   let subst, ty1 = normalize_type info subst ty1 in
   let subst, ty2 = normalize_type info subst ty2 in
      match ty1, ty2 with
         TypeVar v1, TypeVar v2 ->
            if Lm_symbol.eq v1 v2 then
               subst
            else
               (match subst_find_opt subst v1 with
                   Some ty1 ->
                      unify_subtype_types tenv info subst ty1 ty2
                 | None ->
                      match subst_find_opt subst v2 with
                         Some ty2 ->
                            unify_subtype_types tenv info subst ty1 ty2
                       | None ->
                            subst_add_var subst v1 ty2)
       | TypeVar v1, _ ->
            (match subst_find_opt subst v1 with
                Some ty1 ->
                   unify_subtype_types tenv info subst ty1 ty2
              | None ->
                   subst_add_var subst v1 ty2)
       | _, TypeVar v2 ->
            (match subst_find_opt subst v2 with
                Some ty2 ->
                   unify_subtype_types tenv info subst ty1 ty2
              | None ->
                   subst_add_var subst v2 ty1)

       | TypeTerm t1, TypeTerm t2 ->
            unify_subtype_term_types_normalized tenv info subst t1 t2

       | _, TypeTerm t2 ->
            unify_subtype_typeclass_term tenv info subst ty1 t2

       | TypeSoVar (ty_cvars1, ty_vars1, ty_res1), TypeSoVar (ty_cvars2, ty_vars2, ty_res2)
         when List.length ty_cvars1 = List.length ty_cvars2 && List.length ty_vars1 = List.length ty_vars2 ->
            let subst = unify_subtype_type_lists tenv info subst ty_cvars2 ty_cvars1 in
            let subst = unify_subtype_type_lists tenv info subst ty_vars2 ty_vars1 in
               unify_subtype_types tenv info subst ty_res1 ty_res2

       | TypeCVar (ty_cvars1, ty_vars1, ty_exp1, ty_res1), TypeCVar (ty_cvars2, ty_vars2, ty_exp2, ty_res2)
         when List.length ty_cvars1 = List.length ty_cvars2 && List.length ty_vars1 = List.length ty_vars2 ->
            let subst = unify_subtype_type_lists tenv info subst ty_cvars2 ty_cvars1 in
            let subst = unify_subtype_type_lists tenv info subst ty_vars2 ty_vars1 in
            let subst = unify_subtype_types tenv info subst ty_exp2 ty_exp1 in
               unify_subtype_types tenv info subst ty_res1 ty_res2

       | TypeSCVar (ty_cvars1, ty_vars1, ty_hyp1), TypeSCVar (ty_cvars2, ty_vars2, ty_hyp2)
         when List.length ty_cvars1 = List.length ty_cvars2 && List.length ty_vars1 = List.length ty_vars2 ->
            let subst = unify_subtype_type_lists tenv info subst ty_cvars2 ty_cvars1 in
            let subst = unify_subtype_type_lists tenv info subst ty_vars2 ty_vars1 in
               unify_subtype_types tenv info subst ty_hyp1 ty_hyp2

       | TypeSequent (ty_hyp1, ty_concl1, ty_seq1), TypeSequent (ty_hyp2, ty_concl2, ty_seq2) ->
            let subst = unify_subtype_types tenv info subst ty_hyp1 ty_hyp2 in
            let subst = unify_subtype_types tenv info subst ty_concl1 ty_concl2 in
               unify_subtype_types tenv info subst ty_seq1 ty_seq2

       | TypeHyp (ty_var1, ty_hyp1), TypeHyp (ty_var2, ty_hyp2) ->
            let subst = unify_subtype_types tenv info subst ty_hyp1 ty_hyp2 in
               unify_subtype_types tenv info subst ty_var2 ty_var1

       | TypeHypCases cases1, TypeHypCases cases2
         when List.length cases1 = List.length cases2 ->
            unify_subtype_case_lists tenv info subst cases1 cases2

       | TypeExists (v, ty_var, ty1), _ ->
            let subst, ty1 = instantiate_ty_var subst v ty_var ty1 in
               unify_subtype_types tenv info subst ty1 ty2

       | _, TypeExists (v, ty_var, ty2) ->
            let subst, ty2 = standardize_ty_var info subst v ty_var ty2 in
               unify_subtype_types tenv info subst ty1 ty2

       | _ ->
            unify_equal_types tenv info subst ty1 ty2

and unify_subtype_type_lists tenv info subst tl1 tl2 =
   List.fold_left2 (unify_equal_types tenv info) subst tl1 tl2

and unify_subtype_case_lists tenv info subst cases1 cases2 =
   List.fold_left2 (fun subst (t11, t12) (t21, t22) ->
         let subst = unify_subtype_types tenv info subst t12 t22 in
            unify_subtype_types tenv info subst t21 t11) subst cases1 cases2

(*
 * Unify with a term that defines a typeclass.
 *)
and unify_subtype_typeclass_term tenv info subst ty1 t2 =
   match reduce_type tenv subst t2 with
      Some t2 ->
         unify_subtype_types tenv info subst ty1 (TypeTerm t2)
    | None ->
         if is_typeclass_term tenv t2 then
            let opname2 = opname_of_term t2 in
            let shape = shape_of_type ty1 in
            let typeclasses = tenv_find_typeclasses tenv shape in
               if OpnameSet.mem typeclasses opname2 then
                  subst
               else
                  raise_type2_error subst info ty1 (TypeTerm t2)
         else
            raise_type2_error subst info ty1 (TypeTerm t2)

(*
 * Two term types.
 *)
and unify_subtype_term_types tenv info subst t1 t2 =
   if !debug_infer then
      eprintf "unify_subtype_term_types@.";
   let subst, t1 = normalize_term info subst t1 in
   let subst, t2 = normalize_term info subst t2 in
      unify_subtype_types tenv info subst t1 t2

and unify_subtype_term_types_normalized tenv info subst t1 t2 =
   if !debug_infer then
      eprintf "@[<hv 3>unify_subtype_term_types_normalized:@ %s@ %s@]@." (**)
         (string_of_term t1)
         (string_of_term t2);
   if is_fso_var_term t1 || not (is_fo_term t1) || is_so_var_term t1 || is_context_term t1 || is_sequent_term t1 then
      raise_illegal_term_error subst info t1
   else if is_fso_var_term t2 || not (is_fo_term t2) || is_so_var_term t2 || is_context_term t2 || is_sequent_term t2 then
      raise_illegal_term_error subst info t2
   else
      unify_subtype_normal_term_types tenv info true subst t1 t2

and unify_subtype_normal_term_types tenv info delayed subst t1 t2 =
   if !debug_infer then
      eprintf "@[<hv 3>unify_subtype_normal_term_types:@ %s@ %s@]@." (string_of_term t1) (string_of_term t2);
   let shape1 = shape_of_term t1 in
   let shape2 = shape_of_term t2 in
      if TermShape.eq shape1 shape2 then
         (* The types look the same *)
         let { term_op = op1; term_terms = bterms1 } = dest_term t1 in
         let { op_name = opname1; op_params = params1 } = dest_op op1 in
         let { term_op = op2; term_terms = bterms2 } = dest_term t2 in
         let { op_name = opname2; op_params = params2 } = dest_op op2 in
         let () =
            if List.length params1 <> List.length params2 || List.length bterms1 <> List.length bterms2 then
               raise_term2_error "unify_subtype_normal_term_types1" subst info t1 t2
         in
         let subst = unify_equal_param_lists info subst params1 params2 in
            unify_equal_bterm_lists tenv info subst bterms1 bterms2

      else
         unify_subtype_reduction tenv info delayed subst t1 t2 shape1 shape2

(* See if there is a reduction pair that matches the shapes *)
and unify_subtype_reduction tenv info delayed subst t1 t2 shape1 shape2 =
   let t1 = subst_term subst t1 in
   let t2 = subst_term subst t2 in
      match tenv_find_reduction tenv shape1 shape2 with
         Some (ty1, ty2) ->
            let ty1, ty2 = standardize_reduction ty1 ty2 in
            let subst = unify_subtype_normal_term_types tenv info delayed subst t1 ty1 in
            let subst = unify_subtype_normal_term_types tenv info delayed subst t2 ty2 in
               subst
       | None ->
            unify_subtype_reduce_term_types tenv info delayed subst t1 t2 shape1 shape2

(* Try reducing one of the types *)
and unify_subtype_reduce_term_types tenv info delayed subst t1 t2 shape1 shape2 =
   match reduce_type tenv subst t1 with
      Some t1 ->
         unify_subtype_normal_term_types tenv info delayed subst t1 t2
    | None ->
         match reduce_type tenv subst t2 with
            Some t2 ->
               unify_subtype_normal_term_types tenv info delayed subst t1 t2
          | None ->
               unify_subtype_term_typeclass tenv info delayed subst t1 t2 shape1 shape2

(* Try a typeclass typing *)
and unify_subtype_term_typeclass tenv info delayed subst t1 t2 shape1 shape2 =
   if is_typeclass_term tenv t2 then
      let opname2 = opname_of_term t2 in
      let typeclasses = tenv_find_typeclasses tenv shape1 in
         if OpnameSet.mem typeclasses opname2 then
            subst
         else
            raise_term2_error "unify_subtype_normal_term_types2" subst info t1 t2
   else if delayed then
      subst_add_constraint subst (ConstraintSubtype (info, t1, t2))
   else
      raise_term2_error "unify_subtype_normal_term_types3" subst info t1 t2

(************************************************
 * Inference.
 *)

(*
 * Fetch the type of a context var.
 *)
let infer_cvars_type_list tenv venv info subst cvars ty_cvars =
   List.fold_left2 (fun subst v ty_cvar' ->
         let ty_cvar = venv_find_var venv v in
         let info = UnifyCompose (info, UnifyContextVarType2 (v, ty_cvar, ty_cvar')) in
            unify_equal_types tenv info subst ty_cvar ty_cvar') subst cvars ty_cvars

(*
 * Token types.
 *)
let infer_token_type tenv venv info subst opname ty =
   let tok = mk_term (mk_op opname []) [] in
   let ty_tok = tenv_find_type tenv tok in
   let ty_type = ty_tok.ty_type in
   let info = UnifyCompose (info, UnifyTermType2 (tok, TypeTerm ty_type, TypeTerm ty)) in
      unify_subtype_term_types tenv info subst ty_type ty

let infer_token_var tenv venv info subst v ty2 =
   let ty1 = venv_find_var venv v in
   let ty2 = TypeTerm ty2 in
   let tok = mk_var_term v in
   let info = UnifyCompose (info, UnifyTermType2 (tok, ty1, ty2)) in
      unify_subtype_types tenv info subst ty1 ty2

(*
 * Typepe checking.
 *)
let rec infer_term tenv venv info subst e =
   if !debug_infer then
      eprintf "infer_term@.";
   if is_fo_var_term e then
      infer_var_term tenv venv info subst e
   else if is_so_var_term e then
      infer_sovar_term tenv venv info subst e
   else if is_sequent_term e then
      infer_sequent_term tenv venv info subst e
   else if is_context_term e then
      infer_context_term tenv venv info subst e
   else if subst.senv_mode = Relaxed && is_ty_constrain_term e then
      infer_constrain_term tenv venv info subst e
   else if is_ty_constant_term e then
      infer_constant_term tenv venv info subst e
   else
      infer_normal_term tenv venv info subst e

and infer_term_type tenv venv info subst e ty =
   let subst, ty_term = infer_term tenv venv info subst e in
   let info = UnifyCompose (info, UnifyTermType2 (e, ty_term, ty)) in
      unify_subtype_types tenv info subst ty_term ty

and infer_term_type_list tenv venv info subst el types =
   List.fold_left2 (infer_term_type tenv venv info) subst el types

(*
 * Type constraint.
 *)
and infer_constrain_term tenv venv info subst e =
   if !debug_infer then
      eprintf "infer_constrain_term@.";
   let e, ty = dest_ty_constrain_term e in
   let ty = TypeTerm ty in
   let subst = infer_term_type tenv venv info subst e ty in
      subst, ty

(*
 * A constant term added during type inference.
 *)
and infer_constant_term tenv venv info subst e =
   let ty = subst_find_type subst (dest_ty_constant_term e) in
      subst, ty

(*
 * Just look up variables from the environment.
 *)
and infer_var_term tenv venv info subst e =
   if !debug_infer then
      eprintf "infer_var_term@.";
   let v = dest_fso_var e in
      subst, venv_find_var venv v

(*
 * For second-order variables, unify the subterms with
 * the type of the sovar.
 *)
and infer_sovar_term tenv venv info subst e =
   if !debug_infer then
      eprintf "infer_sovar_term@.";
   let v, cvars, args = dest_so_var e in
   let cvars_len = List.length cvars in
   let args_len = List.length args in
   let ty_cvars', ty_args', ty_res =
      match venv_find_var venv v with
         TypeSoVar (ty_cvars, ty_args, ty_res) ->
            ty_cvars, ty_args, ty_res
       | ty ->
            let debug = sprintf "Term_ty_infer.infer_sovar_term: wanted %d cvars, %d args" cvars_len args_len in
               raise_term2_error debug subst info e (term_of_ty ty)
   in
   let cvars_len' = List.length ty_cvars' in
   let args_len' = List.length ty_args' in
   let () =
      if cvars_len <> cvars_len' || args_len <> args_len' then
         raise (RefineError (sprintf "Term_ty_infer.infer_sovar_term: expected %d cvars, %d args" cvars_len' args_len',
                             TermError e))
   in
   let info = UnifyCompose (info, UnifyTerm e) in
   let subst = infer_cvars_type_list tenv venv info subst cvars ty_cvars' in
   let subst = infer_term_type_list tenv venv info subst args ty_args' in
      subst, ty_res

(*
 * A context is a lot like a second-order variable,
 * but we have to check the type of the hole too.
 *)
and infer_context_term tenv venv info subst e =
   if !debug_infer then
      eprintf "infer_context_term@.";
   let v, term, cvars, args = dest_context e in
   let ty_cvars', ty_args', ty_exp', ty_res =
      match venv_find_var venv v with
         TypeCVar (ty_cvars', ty_args', ty_exp, ty_res) ->
            ty_cvars', ty_args', ty_exp, ty_res
       | _ ->
            raise (RefineError ("Term_ty_infer.infer_context_term: context variable has a bad type", VarError v))
   in
   let () =
      if List.length cvars <> List.length ty_cvars' || List.length args <> List.length ty_args' then
         raise (RefineError ("Term_ty_infer.infer_context_term: context arity mismatch", IntTermError (List.length ty_cvars', e)))
   in
   let subst = infer_cvars_type_list tenv venv info subst cvars ty_cvars' in
   let subst = infer_term_type_list tenv venv info subst args ty_args' in
   let subst = infer_term_type tenv venv info subst term ty_exp' in
      subst, ty_res

(*
 * Normal terms.
 *)
and infer_normal_term tenv venv info subst e =
   if !debug_infer then
      eprintf "infer_normal_term@.";

   (* Get the class definition for the term *)
   let ty_term = standardize_ty_term (tenv_find_type tenv e) in
   let { ty_params = ty_params;
         ty_bterms = ty_bterms;
         ty_type   = ty_type
       } = ty_term
   in

   (* Get the parts of the term *)
   let { term_op = op; term_terms = bterms } = dest_term e in
   let { op_name = opname; op_params = params } = dest_op op in

   (* Check that the parameters all have the right types *)
   let subst = infer_param_lists tenv venv info subst params ty_params in

   (* Infer each of the bterms *)
   let subst = infer_bterm_lists tenv venv info e ty_term subst bterms ty_bterms in
      subst, TypeTerm ty_type

(*
 * Check that the param has the right class.
 *)
and infer_params tenv venv info subst param ty_param =
   if !debug_infer then
      eprintf "infer_params@.";
   match dest_param param, ty_param with
      Number _, TyNumber
    | MNumber _, TyNumber
    | String _, TyString
    | MString _, TyString
    | Shape _, TyShape
    | MShape _, TyShape
    | Operator _, TyOperator
    | MOperator _, TyOperator
    | Var _, TyVar
    | Quote, TyQuote
    | MLevel _, TyLevel ->
         subst
    | MToken v, TyToken ty ->
         infer_token_var tenv venv info subst v ty
    | Token opname, TyToken ty ->
         infer_token_type tenv venv info subst opname ty
    | _ ->
         raise (RefineError ("Term_ty_infer.infer_params: param mismatch", ParamTyParamError (param, ty_param)))

and infer_param_lists tenv venv info subst params ty_params =
   List.fold_left2 (infer_params tenv venv info) subst params ty_params

(*
 * Bterm matching.
 *)
and infer_bterms tenv venv info term ty_term subst bterm ty_bterm =
   if !debug_infer then
      eprintf "infer_bterms@.";
   let { ty_bvars = ty_bvars; ty_bterm = ty_term } = ty_bterm in
   let { bvars = bvars; bterm = term } = dest_bterm bterm in
   let () =
      if List.length bvars <> List.length ty_bvars then
         raise (RefineError ("Term_ty_infer.infer_bterms: arity mismatch", Term2Error (term, ty_term)))
   in
   let venv = venv_add_vars venv bvars ty_bvars in
      infer_term_type tenv venv info subst term (TypeTerm ty_term)

and infer_bterm_lists tenv venv info term ty_term subst bterms ty_bterms =
   List.fold_left2 (infer_bterms tenv venv info term ty_term) subst bterms ty_bterms

(*
 * For sequents, the sequent arg determines the type.
 *)
and infer_sequent_term tenv venv info subst e =
   if !debug_infer then
      eprintf "infer_sequent_term@.";
   let { sequent_args = arg;
         sequent_hyps = hyps;
         sequent_concl = concl
       } = explode_sequent e
   in

   (* Argument should be a SequentType{'ty_hyps; 'ty_concl; 'ty_seq} *)
   let ty_hyp'   = TypeVar (new_symbol_string "sequent-hyp") in
   let ty_concl' = TypeVar (new_symbol_string "sequent-concl") in
   let ty_seq'   = TypeVar (new_symbol_string "sequent-type") in
   let ty_arg'   = TypeSequent (ty_hyp', ty_concl', ty_seq') in
   let subst = infer_term_type tenv venv info subst arg ty_arg' in

   (* Hyps should have type ty_hyp, concl should have type ty_concl given vars of type ty_var *)
   let subst, venv = infer_sequent_hyps tenv venv info subst arg ty_hyp' hyps 0 (SeqHyp.length hyps) in
   let subst = infer_term_type tenv venv info subst concl ty_concl' in
      subst, ty_seq'

and infer_sequent_hyps tenv venv info subst arg ty_hyp' hyps i len =
   if !debug_infer then
      eprintf "infer_sequent_hyps@.";
   if i = len then
      subst, venv
   else
      match SeqHyp.get hyps i with
         Hypothesis (v, hyp) ->
            let info' = UnifyCompose (info, UnifyType ty_hyp') in
            let subst, vars, ty_var, ty_hyp = expand_hyp_type info' subst ty_hyp' hyp in
            let subst, ty_var, ty_hyp = standardize_ty_hyp info' subst vars ty_var ty_hyp in
            let subst = infer_term_type tenv venv info subst hyp ty_hyp in
            let venv = venv_add_var venv v ty_var in
               infer_sequent_hyps tenv venv info subst arg ty_hyp' hyps (succ i) len
       | Context (v, cvars, args) ->
            let ty_cvars', ty_args', ty_hyp =
               match venv_find_var venv v with
                  TypeSCVar (ty_cvars', ty_args', ty_hyp) ->
                     ty_cvars', ty_args', ty_hyp
                | _ ->
                     raise (RefineError ("Term_ty_infer.infer_sequent_hyps: context variable has a bad type", VarError v))
            in
            let () =
               if List.length cvars <> List.length ty_cvars' || List.length args <> List.length ty_args' then
                  raise (RefineError ("Term_ty_infer.infer_sequent_hyps: context arity mismatch", VarError v))
            in
            let subst = infer_cvars_type_list tenv venv info subst cvars ty_cvars' in
            let subst = infer_term_type_list tenv venv info subst args ty_args' in
            let info' = UnifyCompose (info, UnifyContextVarType2 (v, ty_hyp, ty_hyp')) in
            let subst = unify_subtype_types tenv info' subst ty_hyp ty_hyp' in
               infer_sequent_hyps tenv venv info subst arg ty_hyp' hyps (succ i) len

(************************************************************************
 * Constraint solving.
 *)

(*
 * Check if a type corresponds to a variable.
 *)
let rec is_ty_var subst ty =
   match ty with
      TypeVar v ->
         (match subst_find_opt subst v with
             Some ty ->
                is_ty_var subst ty
           | None ->
                true)
    | _ ->
         false

(*
 * Choose a case by:
 *    1. Search for a matching unification for the hyp
 *    2. Or else, searching for a matching unification for the var
 *
 * This can clearly be smarter, by using some sort of guided search.
 *)
let rec unify_constraint_case1 tenv info subst t1 t2 cases =
   match cases with
      (c1, c2) :: cases ->
         (try
             let subst = unify_subtype_types tenv info subst c1 t1 in
                unify_subtype_types tenv info subst t2 c2
          with
             RefineError _ ->
                unify_constraint_case1 tenv info subst t1 t2 cases)
    | [] ->
         raise_case_error subst info t1 t2

let rec unify_constraint_case2 tenv info subst t1 t2 cases =
   match cases with
      (c1, c2) :: cases ->
         (try
             let subst = unify_subtype_types tenv info subst t2 c2 in
                unify_subtype_types tenv info subst c1 t1
          with
             RefineError _ ->
                unify_constraint_case2 tenv info subst t1 t2 cases)
    | [] ->
         raise_case_error subst info t1 t2

let unify_constraint_case tenv info subst t1 t2 cases =
   if not (is_ty_var subst t2) then
      unify_constraint_case2 tenv info subst t1 t2 cases
   else if is_ty_var subst t1 then
      raise_case_error subst info t1 t2
   else
      unify_constraint_case1 tenv info subst t1 t2 cases

(*
 * Constraint solver.
 *
 * XXX: JYH: the order of solving does in fact matter, but the problem
 * is hard.  It is currently ignored.
 *)
let solve_constraint tenv venv subst con =
   match con with
      ConstraintMember (info, t1, ty2) ->
         let t1 = subst_term subst t1 in
         let fv = free_vars_set t1 in
         let venv =
            SymbolSet.fold (fun venv v ->
                  if venv_mem_var venv v then
                     venv
                  else
                     venv_add_var venv v (TypeVar (new_symbol v))) venv fv
         in
         let subst, ty1 = infer_term tenv venv info subst t1 in
         let info = UnifyCompose (info, UnifyTermType2 (t1, ty1, ty2)) in
         let subst = unify_subtype_types tenv info subst ty1 ty2 in
            venv, subst
    | ConstraintSubtype (info, t1, t2) ->
         let subst = unify_subtype_normal_term_types tenv info false subst t1 t2 in
            venv, subst
    | ConstraintEqual (info, t1, t2) ->
         let subst = unify_equal_normal_term_types tenv info false subst t1 t2 in
            venv, subst
    | ConstraintCase (info, t1, t2, cases) ->
         let subst = unify_constraint_case tenv info subst t1 t2 cases in
            venv, subst

let rec solve_constraints tenv venv subst =
   match subst.senv_constraints with
      con :: constraints ->
         let subst = { subst with senv_constraints = constraints } in
         let venv, subst = solve_constraint tenv venv subst con in
            solve_constraints tenv venv subst
    | [] ->
         venv, subst

(************************************************************************
 * Wrapped versions.
 *)

(*
 * Wrap the info.
 *)
let infer_term tenv venv subst t =
   infer_term tenv venv (UnifyTerm t) subst t

let infer_term_type tenv venv subst t ty =
   infer_term_type tenv venv (UnifyTerm t) subst t ty

(*
 * Check that each term in the list has a type.
 *)
let check_arg_list tenv venv subst args =
   List.fold_left (fun subst arg ->
         if is_ty_constrain_term arg then
            let e, ty = dest_ty_constrain_term arg in
               infer_term_type tenv venv subst e (TypeTerm ty)
         else
            fst (infer_term tenv venv subst arg)) subst args

(************************************************
 * Infering rewrites and rules.
 *)

(*
 * Build a list of unconstrained types.
 *)
let rec new_ty_list tyl i =
   if i = 0 then
      tyl
   else
      new_ty_list (TypeVar (new_symbol_string "arg") :: tyl) (pred i)

(*
 * Construct the environment from the terms.
 *)
let venv_of_terms tl =
   let venv = venv_empty in
   let venv =
      SymbolSet.fold (fun venv v ->
            venv_add_var venv v (TypeVar (new_symbol v))) venv (param_vars_info_list (free_vars_terms tl) tl)
   in
   let venv =
      SymbolTable.fold (fun venv v (carity, arity) ->
            let ty =
               TypeSoVar (new_ty_list [] carity, new_ty_list [] arity, TypeVar (new_symbol v))
            in
               venv_add_var venv v ty) venv (so_vars_info_list SymbolTable.empty tl)
   in
   let venv =
      SymbolTable.fold (fun venv v (seq_context, carity, arity) ->
            let ty_cvars = new_ty_list [] carity in
            let ty_args = new_ty_list [] arity in
            let ty =
               if seq_context then
                  let ty_hyp = TypeVar (new_symbol v) in
                     TypeSCVar (ty_cvars, ty_args, ty_hyp)
               else
                  let ty_exp = TypeVar (new_symbol v) in
                  let ty_res = TypeVar (new_symbol v) in
                     TypeCVar (ty_cvars, ty_args, ty_exp, ty_res)
            in
               venv_add_var venv v ty) venv (context_vars_info_list SymbolTable.empty tl)
   in
      venv

(*
 * Get the initial env from the mterm.
 * Only include the terms in the redex.
 *)
let venv_of_redices args redices =
   let venv = venv_empty in

   (* Get additional info about the vars *)
   let terms = args @ redices in
   let cvars = context_vars_info_list SymbolTable.empty terms in
   let fv = param_vars_info_list (free_vars_terms terms) terms in
   let so_vars = so_vars_info_list SymbolTable.empty terms in

   (* Add type for all the vars *)
   let venv =
      SymbolSet.fold (fun venv v ->
            venv_add_var venv v (TypeVar (new_symbol v))) venv fv
   in
   let venv =
      SymbolTable.fold (fun venv v (carity, arity) ->
            let ty =
               TypeSoVar (new_ty_list [] carity, new_ty_list [] arity, TypeVar (new_symbol v))
            in
               venv_add_var venv v ty) venv so_vars
   in
   let venv =
      SymbolTable.fold (fun venv v (seq_context, carity, arity) ->
            let ty_cvars = new_ty_list [] carity in
            let ty_args = new_ty_list [] arity in
            let ty =
               if seq_context then
                  let ty_hyp = TypeVar (new_symbol v) in
                     TypeSCVar (ty_cvars, ty_args, ty_hyp)
               else
                  let ty_exp = TypeVar (new_symbol v) in
                  let ty_res = TypeVar (new_symbol v) in
                     TypeCVar (ty_cvars, ty_args, ty_exp, ty_res)
            in
               venv_add_var venv v ty) venv cvars
   in
      args, venv

(*
 * Check a rule.
 *)
let check_rule tenv mt args =
   let subst = new_subst Strict in
   let subgoals, goal = unzip_mfunction mt in
   let args, venv = venv_of_redices args [goal] in
   let subst = check_arg_list tenv venv subst args in

   (* The goal must be a judgment *)
   let subst = infer_term_type tenv venv subst goal ty_judgment in
   let venv, subst = solve_constraints tenv venv subst in
   let subst = instantiate_subst venv subst in

   (* The subgoals must be judgments *)
   let subst =
      List.fold_left (fun subst (_, ext, subgoal) ->
            let subst = infer_term_type tenv venv subst subgoal ty_judgment in
               (*
                * BUG: jyh: ignore extracts for now.
               match ext with
                  Some t ->
                     infer_term_type tenv venv subst t ty_judgment
                | None ->
                     subst
                *)
               subst) subst subgoals
   in
      ignore (solve_constraints tenv venv subst)

(*
 * Get the type of a rewrite.
 *)
let check_rewrite tenv mt args =
   let subst = new_subst Strict in
   let subgoals, redex, contractum = unzip_mrewrite mt in
   let args, venv = venv_of_redices args [redex] in
   let subst = check_arg_list tenv venv subst args in

   (* Verify the rewrite in the reverse direction *)
   let _ =
      let subst, ty1 = infer_term tenv venv subst redex in
      let subst, ty2 = infer_term tenv venv subst contractum in
      let subst = unify_subtype_types tenv (UnifyTermType2 (redex, ty1, ty2)) subst ty1 ty2 in
         solve_constraints tenv venv subst
   in

   (* Free variables in the redex are existentially quantified *)
   let subst, ty1 = infer_term tenv venv subst redex in
   let venv, subst = solve_constraints tenv venv subst in
   let subst = instantiate_subst venv subst in
   let () =
      if !debug_infer then
         eprintf "@[<v 3>check_rewrite2:@ %s@ %s@]@." (**)
            (string_of_term redex)
            (string_of_term (subst_type_term subst ty1))
   in

   (* The contractum must be a subtype of the redex *)
   let subst, ty2 = infer_term tenv venv subst contractum in
   let subst = unify_subtype_types tenv (UnifyTermType2 (contractum, ty2, ty1)) subst ty2 ty1 in

   (* The subgoals must have Term type *)
   let subst =
      List.fold_left (fun subst cond ->
            infer_term_type tenv venv subst cond ty_term) subst subgoals
   in
   let _, subst = solve_constraints tenv venv subst in
      ignore (subst_type subst ty1)

(*
 * Check a type rewrite.
 * The two sides must both be types, but they can be different.
 *)
let check_type_rewrite tenv redex contractum =
   let venv = venv_of_terms [redex; contractum] in
   let subst = new_subst Strict in
   let subst = infer_term_type tenv venv subst redex ty_type in
   let subst = infer_term_type tenv venv subst contractum ty_type in
      ignore (solve_constraints tenv venv subst)

(*
 * Check a display form.
 *)
let check_dform tenv redex contractum =
   let venv = venv_of_terms [redex; contractum] in
   let subst = new_subst Relaxed in
   let subst = infer_term_type tenv venv subst redex ty_dform in
   let subst = infer_term_type tenv venv subst contractum ty_dform in
      ignore (solve_constraints tenv venv subst)

(*
 * Check an iform.
 * We just care that each term in the iform has a type.
 * There is no effort to match the types.
 *)
let check_iform tenv mt =
   let subst = new_subst Relaxed in
   let subgoals, redex, contractum = unzip_mrewrite mt in
   let _, venv = venv_of_redices [] [redex; contractum] in

   (* It is ok if the types change *)
   let subst, _ = infer_term tenv venv subst redex in
   let subst, _ = infer_term tenv venv subst contractum in

   (* Verify each of the conditions *)
   let subst =
      List.fold_left (fun subst e ->
            infer_term_type tenv venv subst e ty_term) subst subgoals
   in
      ignore (solve_constraints tenv venv subst)

(*
 * Check a production.
 * Make sure each item is a Nonterminal.
 *)
let check_production tenv redices contractum =
   let terms = contractum :: redices in
   let venv = venv_of_terms terms in
   let subst = new_subst Relaxed in
   let subst =
      List.fold_left (fun subst e ->
            infer_term_type tenv venv subst e ty_nonterminal) subst redices
   in
   let subst = infer_term_type tenv venv subst contractum ty_nonterminal in
      ignore (solve_constraints tenv venv subst)

(*
 * Infer the type of a term.
 *)
let infer_term tenv t =
   if !debug_infer then
      eprintf "infer@.";
   let venv = venv_of_terms [t] in
   let subst = new_subst Strict in
   let subst, ty = infer_term tenv venv subst t in
   let _, subst = solve_constraints tenv venv subst in
   let ty = subst_type_term subst ty in
      if !debug_infer then
         eprintf "infer done@.";
      ty

(************************************************************************
 * Erase all type annotations.
 *)

let erase_top_term t =
   if is_ty_constrain_term t then
      fst (dest_ty_constrain_term t)
   else
      t

let erase_arg_term = erase_top_term

let erase_term t =
   map_up erase_top_term t

let erase_meta_term mt =
   map_mterm erase_term mt

let is_seq_ignore_bindings_tp ty =
   let ty = ty.ty_type in
      is_ty_sequent_term ty &&
      let ty, _, _ = dest_ty_sequent_term ty in
         is_ty_hyp_term ty &&
         let ty, _ = dest_ty_hyp_term ty in
            alpha_equal ty ignore_type

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
