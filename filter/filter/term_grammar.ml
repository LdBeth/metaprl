(*
 * Extend the language with a term parser as the default quotation.
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
 * Copyright (C) 1998-2005 MetaPRL Group, Cornell University and Caltech
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 * Modified By: Alexei Kopylov <kopylov@cs.cornell.edu>
 * Modified By: Adam Granicz <granicz@cs.caltech.edu>
 *)
open Lm_debug
open Lm_symbol
open Lm_printf

open Opname
open Term_sig
open Term_ty_sig
open Term_meta_sig
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermShape
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError

open Filter_type
open Filter_base_type
open Filter_util
open Filter_summary_util
open Simple_print.SimplePrint

open Term_ty_infer

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_grammar%t"

let debug_grammar =
   create_debug (**)
      { debug_name = "grammar";
        debug_description = "Debug the term grammar and parsing operations";
        debug_value = false
      }

let debug_spell =
   create_debug (**)
      { debug_name = "spell";
        debug_description = "Check spelling";
        debug_value = false
      }

let _ =
   Grammar.error_verbose := true;
   Grammar.warning_verbose := true

(*
 * Terms for representing comments.
 *)
let mk_comment_opname =
   let op = Opname.mk_opname "Comment" Opname.nil_opname in
      fun s -> Opname.mk_opname s op

let comment_white_op = mk_comment_opname "comment_white"
let comment_string_op = mk_comment_opname "comment_string"
let comment_block_op = mk_comment_opname "comment_block"
let comment_docon_op = mk_comment_opname "docon"
let comment_docoff_op = mk_comment_opname "docoff"
let comment_doc_op = mk_comment_opname "doc"

let (misspelled : (string * Ploc.t) list ref) = ref []
let dict_inited = ref false

let raise_spelling_error () =
   if !misspelled <> [] then begin
      let rec print word = function
         (h, (loc : Ploc.t)) :: t ->
            if word = h then
               eprintf "; "
            else
               eprintf "\n\t%s: " (Lm_ctype.quote h);
            let pos_lnum = Ploc.line_nb loc in
            if pos_lnum >= 0 then
               let pos_bol = Ploc.bol_pos loc in
                  eprintf "line %i, char %i" pos_lnum pos_bol
            else
               let pos_cnum = Ploc.first_pos loc in
                  eprintf "char %i" pos_cnum;
            print h t
       | [] ->
            ()
      in
      let (word, loc) = Lm_list_util.last !misspelled in
      let l = List.sort Stdlib.compare !misspelled in
         misspelled := [];
         eprintf "The following words may be misspelled:";
         print "" l;
         eflush stderr;
         Ploc.raise loc (Failure ("spelling (" ^ word ^ ")"))
      end

let create_meta_function t left right =
   let t =
      (*
       * XXX HACK: we assume that extracts from sequents must be sequents of the same shape
       * And whenever users specify a non-sequent extract, they must be meaning to specify a
       * conclusion of a sequent.
       * There is a complimentary HACK in Filter_parse.parse_mtlre
       *)
      let left = unfold_mlabeled "Term_grammar.create_meta_function" left in
         if is_sequent_term left && not (is_sequent_term t) then replace_concl left t else t
   in
      MetaFunction(t, left, right)

(*
 * Handle empty conclusions.
 *)
let concl_of_opt_concl concl =
   match concl with
      Some t -> t
    | None -> xconcl_term

(*
 * Opnames needed for param parsing.
 *)
let perv_opname          = Opname.mk_opname "Perv" nil_opname
let xparam_int_opname    = Opname.mk_opname "xparam_int" perv_opname
let xparam_neg_opname    = Opname.mk_opname "xparam_neg" perv_opname
let xparam_string_opname = Opname.mk_opname "xparam_string" perv_opname
let xparam_id_opname     = Opname.mk_opname "xparam_id" perv_opname
let xparam_succ_opname   = Opname.mk_opname "xparam_string" perv_opname
let xparam_max_opname    = Opname.mk_opname "xparam_max" perv_opname
let xparam_term_opname   = Opname.mk_opname "xparam_term" perv_opname
let xparam_opname        = Opname.mk_opname "xparam" perv_opname

(*
 * Build the grammar.
 *)
module MakeTermGrammar (TermGrammar : TermGrammarSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Parsed forms are the usual forms, used for abstraction.
    *)
   type parsed_term = term
   type parsed_meta_term = meta_term
   type parsed_bound_term = bound_term

   include TermGrammar

   (*
    * Also meta-terms.
    *)
(* unused
   type amterm = { mname : string option; mterm : meta_term }
*)
   type vmterm = { vname : term option; vterm : meta_term }

   (*
    * String or term.
    *)
   type string_or_term =
      ST_String of (string * MLast.loc)
    | ST_Term of (aterm * MLast.loc)

   (*
    * Binding vars for declares.
    *)
   type ty_ass_bvar =
      TyBVar1 of string_or_term
    | TyBVar2 of string_or_term * string_or_term

   (*
    * String or word.
    *)
   type string_or_word =
      SW_String of string
    | SW_Word of string

   let string_of_sw = function
      SW_String s
    | SW_Word s ->
         s

   (************************************************************************
    * For quoted terms.
    *)
   let unknown_opname = mk_opname "$unknown" nil_opname

   let unknown_token_opname = mk_opname "token" unknown_opname
   let unknown_bvar_opname  = mk_opname "bvar" unknown_opname
   let unknown_type_opname  = mk_opname "type" unknown_opname

   let unknown_token = mk_term (mk_op unknown_token_opname []) []
   let unknown_bvar  = mk_term (mk_op unknown_bvar_opname []) []
   let unknown_type  = mk_term (mk_op unknown_type_opname []) []

   let is_unknown_token_term = is_no_subterms_term unknown_token_opname
   let is_unknown_bvar_term  = is_no_subterms_term unknown_bvar_opname
   let is_unknown_type_term  = is_no_subterms_term unknown_type_opname

   (************************************************************************
    * STATEFUL FUNCTIONS                                                   *
    ************************************************************************)

   let opname_prefix loc      = (parsing_state loc).opname_prefix loc
   let mk_opname_kind loc     = (parsing_state loc).mk_opname_kind loc
   let find_shape_class loc   = (parsing_state loc).find_shape_class loc
   let mk_var_contexts loc    = (parsing_state loc).mk_var_contexts loc
   let infer_term loc         = (parsing_state loc).infer_term loc
   let check_rule loc         = (parsing_state loc).check_rule loc
   let check_rewrite loc      = (parsing_state loc).check_rewrite loc
   let check_type_rewrite loc = (parsing_state loc).check_type_rewrite loc
   let check_dform loc        = (parsing_state loc).check_dform loc
   let check_iform loc        = (parsing_state loc).check_iform loc
   let check_production loc   = (parsing_state loc).check_production loc
   let check_input_term loc   = (parsing_state loc).check_input_term loc
   let check_input_mterm loc  = (parsing_state loc).check_input_mterm loc
   let apply_iforms loc       = (parsing_state loc).apply_iforms loc
   let apply_iforms_mterm loc = (parsing_state loc).apply_iforms_mterm loc
   let term_of_string loc     = (parsing_state loc).term_of_string loc

   (************************************************************************
    * Whether to allow sequent bindings - pass that information to the post-parser.
    *)
   let term_of_parsed_term loc = term_of_parsed_term ((parsing_state loc).allow_seq_bindings loc)
   let term_of_parsed_term_with_vars loc = term_of_parsed_term_with_vars ((parsing_state loc).allow_seq_bindings loc)
   let mterms_of_parsed_mterms loc = mterms_of_parsed_mterms ((parsing_state loc).allow_seq_bindings loc)
   let rewrite_of_parsed_rewrite loc = rewrite_of_parsed_rewrite ((parsing_state loc).allow_seq_bindings loc)
   let mrewrite_of_parsed_mrewrite loc = mrewrite_of_parsed_mrewrite ((parsing_state loc).allow_seq_bindings loc)

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   let mk_opname loc names params bterms =
      mk_opname_kind loc NormalKind names params bterms

   let mk_opname_token loc names =
      mk_opname_kind loc TokenKind names [] []

   let mk_0opname loc name =
      mk_opname loc [name] [] []

   let mk_dep0_opname loc name =
      mk_opname loc [name] [] [0]

   let mk_dep1_opname loc name =
      mk_opname loc [name] [] [1]

   let mk_dep0_dep0_opname loc name =
      mk_opname loc [name] [] [0; 0]

   let mk_dep0_dep1_opname loc name =
      mk_opname loc [name] [] [0; 1]

   let mk_dep0_dep2_opname loc name =
      mk_opname loc [name] [] [0; 2]

   let mk_dep0_dep0_dep0_opname loc name =
      mk_opname loc [name] [] [0; 0; 0]

   let bterm_arities =
      List.map (fun bt -> List.length (dest_bterm bt).bvars)

   let mk_bopname loc names params bterms =
      mk_opname loc names (List.map param_type params) (bterm_arities bterms)

   let mk_token_opname loc name =
      mk_opname_kind loc TokenKind [name] [] []

   (*
    * Parameters.
    *)
   let make_param_opname loc l =
      match l with
         [SW_Word s] ->
            make_param (MString (Lm_symbol.add s))
       | [SW_String s] ->
            make_param (String s)
       | _ ->
            make_param (Token (mk_opname_token loc (List.map string_of_sw l)))

   let make_con_param_opname loc l =
      match l with
         [SW_Word s] ->
            ConPMeta (Lm_symbol.add s), ShapeString
       | [SW_String s] ->
            ConPStr s, ShapeString
       | _ ->
            ConPToken (mk_opname_token loc (List.map string_of_sw l)), ShapeToken

   let make_con_param_opname_shape loc l shape =
      match l, shape with
         _, ShapeToken
       | _ :: _ :: _, _
       | [], _ ->
            ConPToken (mk_opname_token loc (List.map string_of_sw l)), ShapeToken
       | [SW_Word s], _ ->
            ConPMeta (Lm_symbol.add s), shape
       | [SW_String s], _ ->
            ConPStr s, ShapeString

(* unused
   let make_param_class loc l =
      match l with
         [SW_Word s]
       | [SW_String s] ->
            (match s with
                "n" ->
                   TyNumber
              | "s" ->
                   TyString
              | "t" ->
                   TyToken Term_ty_infer.token_type
              | "sh" ->
                   TyShape
              | "op" ->
                   TyOperator
              | "v" ->
                   TyVar
              | "l" ->
                   TyLevel
              | _ ->
                   TyToken (mk_term (mk_op (mk_opname_token loc [s]) []) []))
       | _ ->
            TyToken (mk_term (mk_op (mk_opname_token loc (List.map string_of_sw l)) []) [])
*)

   (*
    * For new symbols.
    *)

   let mk_gensym =
      let gensym = ref 0 in
         fun () ->
            incr gensym;
            Lm_symbol.make "$" !gensym

   (* Currying *)
   let mk_bterm' (vars, bterm) =
      mk_bterm vars bterm

(* unused
   let mk_ty_bterm (vars, bterm, ty) =
      mk_bterm vars bterm, ty
*)

   (*
    * Get the class of a param.
    *)
   let ty_param_of_param param =
      match dest_param param with
         Number _
       | MNumber _ ->
            TyNumber
       | String _
       | MString _ ->
            TyString
       | Token _
       | MToken _ ->
            TyToken unknown_token
       | Var _ ->
            TyVar
       | MLevel _ ->
            TyLevel
       | Quote ->
            TyQuote
       | Shape _
       | MShape _ ->
            TyShape
       | Operator _
       | MOperator _ ->
            TyOperator
       | ObId _
       | ParamList _ ->
            raise (Invalid_argument "ty_param_of_param: unexpected Nuprl5 parameter")

   (*
    * Cast a parameter to a level expression.
    *)
   let rec level_var s =
      if s.[pred (String.length s)] = '\'' then
         incr_level_exp (level_var (String.sub s 0 (pred (String.length s))))
      else
         mk_var_level_exp (Lm_symbol.add s)

   let cast_level p =
      match dest_param p with
         Number n when Lm_num.is_integer_num n ->
           mk_const_level_exp (Lm_num.int_of_num n)
       | MLevel l ->
            l
       | MString v ->
            level_var (string_of_symbol v)
       | _ ->
            raise (BadParamCast (p, "l"))

   (*
    * Cast to a number.
    *)
   let cast_number p =
      match dest_param p with
         Number _
       | MNumber _ ->
            p
       | MString s ->
            make_param (MNumber s)
       | _ ->
            raise (BadParamCast (p, "n"))

   (*
    * Cast to a token.
    *)
   let cast_token loc p =
      match dest_param p with
         Number n ->
            make_param (Token (mk_token_opname loc (Lm_num.string_of_num n)))
       | String s ->
            make_param (Token (mk_token_opname loc s))
       | MString v ->
            make_param (MToken v)
       | Token _
       | MToken _ ->
            p
       | _ ->
            raise (BadParamCast (p, "t"))

   let cast_string loc p =
      match dest_param p with
         Number n ->
            make_param (String (Lm_num.string_of_num n))
       | String _
       | MString _ ->
            p
       | _ ->
            raise (BadParamCast (p, "s"))

   let cast_var loc p =
      match dest_param p with
         Number n ->
            make_param (Var (Lm_symbol.add (Lm_num.string_of_num n)))
       | String s ->
            make_param (Var (Lm_symbol.add s))
       | MString v ->
            make_param (Var v)
       | _ ->
            raise (BadParamCast (p, "v"))

   let cast_shape loc p =
      match dest_param p with
         MString v ->
            make_param (MShape v)
       | Token t ->
            make_param (Shape {shape_opname = t; shape_params = []; shape_arities = []})
       | Operator op -> make_param (Shape (shape_of_opparam op))
       | Shape _
       | MShape _ ->
            p
       | _ ->
            raise (BadParamCast (p, "sh"))

   let cast_operator loc p =
      match dest_param p with
         MString v ->
            make_param (MOperator v)
       | Token t ->
            make_param (Operator {opparam_name = t; opparam_params = []; opparam_arities = []})
       | Operator _
       | MOperator _ ->
            p
       | _ ->
            raise (BadParamCast (p, "op"))

   (*
    * Parameter casting.
    *)
   let cast_param_string loc p = function
      "n" ->
         cast_number p, TyNumber
    | "s" ->
         cast_string loc p, TyString
    | "v" ->
         cast_var loc p, TyVar
    | "l" ->
         make_param (MLevel (cast_level p)), TyLevel
    | "t" ->
         cast_token loc p, TyToken Term_ty_infer.token_type
    | "sh" ->
         cast_shape loc p, TyShape
    | "op" ->
         cast_operator loc p, TyOperator
    | opname ->
         (*
          * By default, everything else is a token, and the name
          * is the name of the class for the token.  Check that it
          * exists.
          *)
         let cl = mk_term (mk_op (mk_opname loc [opname] [] []) []) [] in
            cast_token loc p, TyToken cl

   let cast_param loc p ty =
      match ty with
         [SW_Word s] ->
            fst (cast_param_string loc p s)
       | _ ->
            ignore(mk_opname loc (List.map string_of_sw ty) [] []);
            cast_token loc p

   let get_aterm loc = function
      { aname = Some v; aterm = t } ->
         Ploc.raise loc (Invalid_argument
            ("Syntax Error: Named term where unnamed one is expected:\n" ^ (string_of_term v) ^ " : " ^ (string_of_term t)))
    | { aname = None; aterm = t } ->
         t

   let cast_ty_param loc p ty =
      match ty with
         ST_String (s, loc) ->
            cast_param_string loc p s
       | ST_Term (at, loc') ->
            let p = cast_token loc p in
               p, TyToken (get_aterm loc' at)

   (************************************************************************
    * xparam term parsing.
    *)
   let dest_xparam_string loc t =
      if is_string_term xparam_string_opname t then
         SW_String (dest_string_term xparam_string_opname t)
      else if is_string_term xparam_id_opname t then
         SW_Word (dest_string_term xparam_id_opname t)
      else
         Ploc.raise loc (Invalid_argument "not a paramater")

   let dest_xparam_type loc t =
      List.map (dest_xparam_string loc) (hyps t)

   let rec dest_xparam_exp loc t =
      if is_number_term xparam_int_opname t then
         let i = dest_number_term xparam_int_opname t in
            make_param (Number i)
      else if is_number_term xparam_neg_opname t then
         let i = dest_number_term xparam_neg_opname t in
            make_param (Number (Lm_num.mult_num i (Lm_num.num_of_int (-1))))
      else if is_string_term xparam_string_opname t then
         let s = dest_string_term xparam_string_opname t in
            make_param (String s)
      else if is_string_term xparam_id_opname t then
         let s = dest_string_term xparam_id_opname t in
            make_param (MString (Lm_symbol.add s))
      else if is_dep0_term xparam_succ_opname t then
         let t = dest_dep0_term xparam_succ_opname t in
         let t = dest_xparam_exp loc t in
            make_param (MLevel (incr_level_exp (cast_level t)))
      else if is_dep0_dep0_term xparam_max_opname t then
         let t1, t2 = dest_dep0_dep0_term xparam_max_opname t in
         let p1 = dest_xparam_exp loc t1 in
         let p2 = dest_xparam_exp loc t2 in
            make_param (MLevel (max_level_exp (cast_level p1) (cast_level p2) 0))
      else
         Ploc.raise loc (RefineError ("dest_xparam_exp", StringTermError ("not a parameter", t)))

   let dest_xparam loc t =
      if is_dep0_dep0_term xparam_opname t then
         let p, t = dest_dep0_dep0_term xparam_opname t in
         let p = dest_xparam_exp loc p in
         let t = dest_xparam_type loc t in
            cast_param loc p t
      else if is_dep0_term xparam_opname t then
         let p = dest_dep0_term xparam_opname t in
            dest_xparam_exp loc p
      else if is_dep0_dep0_term xparam_term_opname t then
         let t1, t2 = dest_dep0_dep0_term xparam_term_opname t in
         let t2 = dest_xparam_type loc t2 in
            cast_param loc (make_param (Operator (opparam_of_term t1))) t2
      else
         Ploc.raise loc (RefineError ("dest_xparam_exp", StringTermError ("not a parameter", t)))

   (*
    * Constructors.
    *)
   let mk_pair_term loc a b =
      mk_dep0_dep0_term (mk_dep0_dep0_opname loc "pair") a b

   let make_term = function
      ST_String (s, loc) ->
         mk_term (mk_op (mk_0opname loc s) []) []
    | ST_Term (at, loc) ->
         get_aterm loc at

   let make_aterm = function
      ST_Term(at, _) -> at
    | ST_String(s, loc) -> { aname = None; aterm = mk_term (mk_op (mk_0opname loc s) []) [] }

   let wrap_term loc t =
      ST_Term ({ aname = None; aterm = t}, loc)

   (*
    * Turn a reversed list of terms into a tuple.
    *)
   let rec tupelize loc = function
      [h] -> make_term h
    | h::t -> mk_pair_term loc (make_term h) (tupelize loc t)
    | [] -> raise (Invalid_argument "tupelize")

   (*
    * Construct an application.
    *)
   let make_application loc terms =
      let op = mk_dep0_dep0_opname loc "apply" in
      (* Convert the list to an application *)
      let rec aux x = function
         [] -> x
       | h::t ->
            aux (mk_dep0_dep0_term op x h) t
      in
         match terms with
            [] -> Ploc.raise loc (Invalid_argument "Term_grammar.make_application: internal error")
          | h::t -> wrap_term loc (aux h t)

   (*
    * Construct a binary term, with a possible dependency.
    *)
   let mk_type_term loc name t1 t2 =
      let t2 = make_term t2 in
      match make_aterm t1 with
         { aname = None; aterm = t } ->
            (* XXX HACK - this is to support ad-hoc I/O form for "fun" and alike *)
            begin try
               wrap_term loc (mk_dep0_dep0_term (mk_dep0_dep0_opname loc name) t t2)
            with Failure _ | Not_found | Ploc.Exc (_, Not_found) | Ploc.Exc (_, Failure _) ->
               wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc name) (Lm_symbol.add "") t t2)
            end
       | { aname = Some name'; aterm = t } ->
            wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc name) (dest_var name') t t2)

   let mk_arith_term loc name t1 t2 =
      wrap_term loc (mk_dep0_dep0_term (mk_dep0_dep0_opname loc name) (make_term t1) (make_term t2))

   (*
    * Make record terms
    *)
   let mk_field_term loc r field =
      let field_opname = mk_opname loc ["field"] [ShapeToken] [0] in
      let token_opname = mk_token_opname loc field in
         mk_term (mk_op field_opname [make_param (Token token_opname)]) [mk_simple_bterm r]

   let mk_field_self_term =
      let self_term = mk_var_term (Lm_symbol.add "self") in
         fun loc field -> mk_field_term loc self_term field

   (*
    * Check that all are strings.
    *)
   let make_bvar = function
      ST_Term (_, loc) ->
         Ploc.raise loc (ParseError "Not a binding var")
    | ST_String (s, _) ->
         Lm_symbol.add s

(* unused
   let make_ty_bvar (v, ty) =
      make_bvar v, ty
*)

   let make_ty_bvar = function
      TyBVar1 (ST_Term ({ aname = Some v; aterm = ty }, loc)) ->
         dest_var v, ty
    | TyBVar1 (ST_Term (_, loc)) ->
         Ploc.raise loc (ParseError "Not a binding var")
    | TyBVar1 (ST_String (s, _)) ->
         Lm_symbol.add s, unknown_bvar
    | TyBVar2 (id, ty) ->
         make_bvar id, make_term ty

   let make_ty_term loc = function
      [TyBVar1 (ST_Term ({ aname = Some v; aterm = ty }, _))] ->
         let term = mk_term (mk_op (mk_0opname loc (string_of_symbol (dest_var v))) []) [] in
            term, ty
    | [TyBVar1 (ST_Term ({ aname = None; aterm = t }, _))] ->
         t, unknown_type
    | [TyBVar1 (ST_String (s, _))] ->
         let term = mk_term (mk_op (mk_0opname loc s) []) [] in
            term, unknown_type
    | [TyBVar2 (id, ty)] ->
         make_term id, make_term ty
    | _ ->
         Ploc.raise loc (ParseError "illegal bterm")

   let get_var_contexts loc v terms =
      match mk_var_contexts loc v (List.length terms) with
         Some conts -> conts
       | None -> [default_contexts]

   (************************************************************************
    * QUOTATIONS                                                           *
    ************************************************************************)

   let dest_quot quot =
      try
         let i = String.index quot ':' in
            String.sub quot 0 i, String.sub quot (i+1) (String.length quot-i-1)
      with
         Not_found ->
            "term", quot

   (*
    * Parse a comment string.
    *)
   type spelling =
      SpellOff
    | SpellOn
    | SpellAdd

   let fake_arities =
      List.map (fun _ -> 0)

   let string_params =
      List.map (fun _ -> ShapeString)

   let expr_of_arg _loc s =
      <:expr< argv.( $int: s$ ) >>

   let expr_of_anti loc name s =
      try grammar_parse Pcaml.expr_eoi (Stream.of_string s) with
         Ploc.Exc (l, exn) ->
            let offset = String.length "$" in
            let offset = if name = "" then offset else offset + 1 + (String.length name) in
            let loc = shift_pos loc offset in
               Ploc.raise (adjust_pos loc l) exn

   let q_shift_loc loc nm =
      shift_pos loc (if nm = "" then String.length "<<" else (String.length "<:") + (String.length nm) + (String.length "<"))

   let rec parse_quotation loc curr nm s =
      if nm = curr then
         Ploc.raise loc (Failure (nm ^ " quotation inside a " ^ curr ^ " quotation"));
      match nm with
         "term"
       | "" ->
            (try
                let cs = Stream.of_string s in
                   grammar_parse TermGrammar.term_eoi cs
             with
                Ploc.Exc (l, exn) ->
                   let pos = q_shift_loc loc nm in
                      Ploc.raise (adjust_pos pos l) exn)
       | "doc" ->
            parse_comment (q_shift_loc loc nm) true SpellOn true s
       | "topdoc" ->
            parse_comment (q_shift_loc loc nm) false SpellOn  true s
       | "dform" ->
            parse_comment (q_shift_loc loc nm) false SpellOff false s
       | _ ->
            (* Otherwise, use the current grammar *)
            let state = mk_parse_state loc nm in
               try term_of_string (q_shift_loc loc nm) state nm s with
                  Not_found ->
                     Ploc.raise loc (Failure ("parse_quotation: grammar is not defined: " ^ nm))
                | exn ->
                     Ploc.raise loc exn

   and parse_comment loc math spell space s =
      let pos = loc in
      let () =
         if !debug_spell && not !dict_inited then
            begin
               Filter_spell.init ();
               dict_inited := true
            end
      in

      (*
       * Convert the result of the Comment_parse.
       *)
      let rec build_comment_term spelling space = function
         Comment_parse.White ->
            mk_simple_term comment_white_op []
       | Comment_parse.String s ->
            if !debug_spell then
               begin
                  match spelling with
                     SpellOff ->
                        ()
                   | SpellAdd ->
                        Filter_spell.add s
                   | SpellOn ->
                        if not (Filter_spell.check s) then
                           misspelled := (s, loc) :: !misspelled
               end;
            mk_string_term comment_string_op s
       | Comment_parse.Variable s ->
            mk_var_term (Lm_symbol.add s)
       | Comment_parse.Term ((opname, l), params, args) ->
            let spelling =
               if !debug_spell then
                  match opname with
                     ["spelling"] ->
                        SpellAdd
                   | ["misspelled"]
                   | ["math_misspelled"]
                   | ["license"]
                   | ["url"]
                   | ["tt"]
                   | ["math_tt"]
                   | ["code"]
                   | ["math_rulebox"]
                   | ["math_defrule"]
                   | ["comment"] ->
                        SpellOff
                   | _ ->
                        spelling
               else
                  spelling
            in
            let space =
               match opname with
                  ["text"] ->
                     true
                | _ ->
                     space
            in
            let opname =
               mk_opname (adjust_pos pos (ploc_of_lexing l)) opname (string_params params) (fake_arities args)
            in
            let params = List.map (fun s -> make_param (String s)) params in
            let args = List.map (fun t -> mk_simple_bterm (build_term spelling space t)) args in
            let op = mk_op opname params in
               mk_term op args
       | Comment_parse.Block items ->
            mk_simple_term comment_block_op [build_term spelling space items]
       | Comment_parse.Quote (l, tag, s) ->
            let t = parse_quotation (adjust_pos pos (ploc_of_lexing l)) "doc" tag s in
               term_of_parsed_term loc t

      (*
       * If spacing is ignored, ignore spaces.
       *)
      and build_inner_term items' spelling space items =
         match items with
            Comment_parse.White :: items when not space ->
               build_inner_term items' spelling space items
            (* Make sure concatenations are not considered misspellings *)
          | Comment_parse.String s1 :: Comment_parse.String "'" :: Comment_parse.String s2 :: items
               when !debug_spell && Filter_spell.check (s1 ^ "'" ^ s2) ->
                  build_inner_term items' spelling space ((Comment_parse.String (s1 ^ "'" ^ s2)) :: items)
          | Comment_parse.String "'" :: Comment_parse.String s :: items
               when !debug_spell && Filter_spell.check ("'" ^ s) ->
                  build_inner_term items' spelling space ((Comment_parse.String ("'" ^ s)) :: items)
          | item :: items ->
               let item = build_comment_term spelling space item in
                  build_inner_term (item :: items') spelling space items
          | [] ->
               List.rev items'
      and build_term spelling space items =
         mk_xlist_term (build_inner_term [] spelling space items)
      in

      (*
       * Parse the input string, and turn it into a term
       *)
      let items =
         try Comment_parse.parse math s with
            Comment_parse.Parse_error (s, l) ->
               Ploc.raise (adjust_pos pos (ploc_of_lexing l)) (ParseError s)
      in
         build_term spell space items

   and mk_parse_state loc name =
      { parse_quotation = parse_quotation loc name;
        parse_opname    = mk_opname_kind loc;
        parse_shape     = find_shape_class loc;
        parse_param     = dest_xparam loc
      }

   let rec strip_white_lst = function
      (t :: tl) as l ->
         if is_no_subterms_term comment_white_op t then
            strip_white_lst tl
         else
            if List.for_all (is_no_subterms_term comment_white_op) tl then strip_white t
         else
            l
    | [] ->
         []

   and strip_white t =
         if is_xlist_term t then
            strip_white_lst (dest_xlist t)
         else
            [t]

(* unused
   let rec is_docoff_lst = function
      t :: _ -> is_docoff t
    | [] -> false

   and is_docoff t =
      is_no_subterms_term comment_docoff_op t ||
         is_no_subterms_term comment_docon_op t ||
         ((is_xlist_term t) && (is_docoff_lst (dest_xlist t)))
*)

   let convert_comment loc t =
      let t =
         if is_string_term comment_string_op t then
            parse_comment (q_shift_loc loc "doc") false SpellOn true (dest_string_term comment_string_op t)
         else
            t
      in
      match strip_white t with
         [t] when is_no_subterms_term comment_docoff_op t || is_no_subterms_term comment_docon_op t ->
            t
       | tl ->
            mk_dep0_term comment_doc_op (mk_xlist_term tl)

   (************************************************************************
    * Parsing.
    *)

   let apply_iforms_raw t =
      let state = mk_parse_state dummy_loc "unknown" in
         apply_iforms dummy_loc state t

   let apply_iforms loc t =
      if !debug_grammar then
         eprintf "Term_grammar.apply_iforms <- %a%t" debug_print t eflush;
      let state = mk_parse_state loc "unknown" in
      let t = apply_iforms loc state t in
         if !debug_grammar then
            eprintf "Term_grammar.apply_iforms -> %a%t" debug_print t eflush;
         t

   let apply_iforms_mterm loc mt args =
      let state = mk_parse_state loc "unknown" in
         apply_iforms_mterm loc state mt args

   let check_input_terms loc terms =
      List.iter (check_input_term loc) terms

   (*
    * Parse the terms, and perform a type check.
    * In the following four functions, type constraints
    * are ignored.  That is, constraints are just
    * comments.
    *)
   let parse_term loc t =
      let t = apply_iforms loc t in
      let t = term_of_parsed_term loc t in
      let _ = infer_term loc t in
         check_input_term loc t;
         t

   let parse_term_with_vars loc t =
      let t = apply_iforms loc t in
      let t = term_of_parsed_term_with_vars loc t in
      let _ = infer_term loc t in
         check_input_term loc t;
         t

   let parse_bound_term loc bt =
      { bt with aterm = parse_term loc bt.aterm }

   let parse_rule loc name mt args =
      let mt, args = apply_iforms_mterm loc mt args in
      let mt, args, f = mterms_of_parsed_mterms loc mt args in

      (* Check with the refiner first for rewrite errors *)
      let cvars = context_vars mt in
      let params = extract_params cvars args in
      let terms = collect_terms params in
         Refine.check_rule name (collect_cvars params) terms (strip_mfunction mt);
         (* Then check for type errors *)
         check_input_mterm loc mt;
         check_input_terms loc terms;
         check_rule loc mt terms;
         cvars, mt, List.map erase_arg_term args, f

   let parse_rewrite loc name mt args =
      let mt, args = apply_iforms_mterm loc mt args in
      let mt, args, f = mterms_of_parsed_mterms loc mt args in

      (* Check with the refiner first for rewrite errors *)
      let cvars = context_vars mt in
      let params = extract_params cvars args in
      let args', redex, contractum = unzip_rewrite name mt in
      let terms = collect_terms params in
         Refine.check_rewrite name (collect_cvars params) terms args' redex contractum;
         (* Then check for type errors *)
         check_input_mterm loc mt;
         check_input_terms loc args;
         check_rewrite loc mt terms;
         mt, List.map erase_arg_term args, f

   let parse_define loc name redex contractum =
      let redex = apply_iforms loc redex in
      let contractum = apply_iforms loc contractum in
      let redex, contractum = rewrite_of_parsed_rewrite loc redex contractum in
         check_input_term loc redex;
         check_input_term loc contractum;

         (* Check with the rewriter first *)
         Refine.check_definition name redex contractum;

         (* Check the types of both parts *)
         check_rewrite loc (MetaIff (MetaTheorem redex, MetaTheorem contractum)) [];
         redex, contractum

   let parse_type_rewrite loc redex contractum =
      let redex = apply_iforms loc redex in
      let contractum = apply_iforms loc contractum in
      let redex, contractum = rewrite_of_parsed_rewrite loc redex contractum in
         check_input_term loc redex;
         check_input_term loc contractum;
         Refine.check_rewrite "type" empty_args_spec [] [] redex contractum;
         check_type_rewrite loc redex contractum;
         redex, contractum

   (*
    * In the following functions, type constraints
    * are erased after inference.  In the informal
    * code, constraints are legal.
    *)
   let parse_iform loc name mt =
      let mt, _, f = mterms_of_parsed_mterms loc mt [] in

      (* Check with the refiner for rewrite errors *)
      let _, redex, contractum = unzip_rewrite name mt in
         Refine.check_iform name redex contractum;
         (* Check for type errors *)
         check_iform loc mt;
         erase_meta_term mt

   let parse_dform loc redex contractum =
      let redex = apply_iforms loc redex in
      let contractum = apply_iforms loc contractum in
      let redex, contractum = rewrite_of_parsed_rewrite loc redex contractum in
      let () = check_dform loc redex contractum in
      let redex = erase_term redex in
      let contractum = erase_term contractum in
         check_input_term loc redex;
         check_input_term loc contractum;
         redex, contractum

   let parse_production loc redices contractum =
      let redices = List.map (apply_iforms loc) redices in
      let contractum = apply_iforms loc contractum in
      let redices, contractum = mrewrite_of_parsed_mrewrite loc redices contractum in
      let () = check_production loc redices contractum in
      let redices = List.map erase_term redices in
      let contractum = erase_term contractum in
         redices, contractum

   (*
    * For terms from other grammars.
    * The other grammars will expand their iforms,
    * so we don't need to do it.
    *)
   let mk_parsed_term t =
      t

   let mk_parsed_meta_term mt =
      mt

   (************************************************
    * !!! WARNING !!!
    * !!! The following functions bypass either the
    * !!! parser or the type checker.
    *)

   (* For bypassing the type checker *)
   let raw_term_of_parsed_term t =
      t

   let raw_meta_term_of_parsed_meta_term t =
      t

   let raw_input_term_of_parsed_term t =
      apply_iforms_raw t

   let unparsed_term_of_parsed_term loc t =
      let t = apply_iforms loc t in
      let _ = infer_term loc t in
         erase_term t

   let unchecked_term_of_parsed_term loc t =
      let t = apply_iforms_raw t in
      let t = term_of_parsed_term loc t in
         erase_term t

   let quoted_term_of_parsed_term loc t =
      term_of_parsed_term loc t

   (*
    * Parameterize declares over default types.
    *)
   let subst_unknown token_type bvar_type term_type t =
      let subst t =
         if is_unknown_token_term t then
            token_type
         else if is_unknown_bvar_term t then
            bvar_type
         else if is_unknown_type_term t then
            term_type
         else
            t
      in
         map_up subst t

   (* Make sure the declared term has a type *)
   let parse_quoted_term loc token_type bvar_type term_type t =
      let t = apply_iforms loc t in
      let t = subst_unknown token_type bvar_type term_type t in
      let _ = infer_term loc t in
         t

   (************************************************************************
    * GRAMMAR                                                              *
    ************************************************************************)

   EXTEND
      GLOBAL: opname
              opname_name
              term_eoi
              term
              parsed_term
              quote_term
              mterm
              bmterm
              singleterm
              parsed_bound_term
              xdform
              term_con_eoi;

      (*
       * Meta-terms include meta arrows.
       *)
      mterm:
         [[ t = amterm ->
             t.vterm
          ]];

      bmterm:
         [[ t = bbmterm ->
             t.vterm
          ]];

      amterm:
         [[ t = noncommaterm ->
             match make_aterm t with
                { aname = None; aterm = t } ->
                   { vname = None; vterm = MetaTheorem t }
              | { aname = Some n; aterm = t } ->
                   { vname = Some n; vterm = MetaTheorem t }
          ]
          | [ "["; name = word_or_string; "]"; t = singleterm ->
              match t with
                 { aname = None; aterm = t } ->
                   { vname = None; vterm = MetaLabeled (name, MetaTheorem t) }
               | { aname = Some n; aterm = t } ->
                   { vname = Some n; vterm = MetaLabeled (name, MetaTheorem t) }
            ]
          | "meta_implies" LEFTA
            [ t1 = amterm; sl_meta_right_arrow; t2 = amterm ->
               begin
                  match t1 with
                     { vname = None; vterm = t } ->
                        { vname = None; vterm = MetaImplies (t, t2.vterm) }
                   | { vname = Some n; vterm = t } ->
                        { vname = None; vterm = create_meta_function n  t  t2.vterm }
               end
            ]
          | "meta_rev_implies" RIGHTA
            [ t2 = amterm; sl_meta_left_arrow; t1 = amterm ->
               begin
                  match t1 with
                     { vname = None; vterm = t } ->
                        { vname = None; vterm = MetaImplies (t, t2.vterm) }
                   | { vname = Some n; vterm = t } ->
                        { vname = None; vterm = create_meta_function n t t2.vterm }
               end
            ]
          | "meta_iff" LEFTA
            [ t1 = amterm; sl_meta_left_right_arrow; t2 = amterm ->
               { vname = None; vterm = MetaIff (t1.vterm, t2.vterm) }
            ]
         ];

      bbmterm:
         [[ t = bound_term ->
             match t with
                { aname = None; aterm = t } ->
                   { vname = None; vterm = MetaTheorem t }
              | { aname = Some n; aterm = t } ->
                   { vname = Some n; vterm = MetaTheorem t }
          ]
          | [ "["; name = word_or_string; "]"; t = bound_term ->
              match t with
                 { aname = None; aterm = t } ->
                   { vname = None; vterm = MetaLabeled (name, MetaTheorem t) }
               | { aname = Some n; aterm = t } ->
                   { vname = Some n; vterm = MetaLabeled (name, MetaTheorem t) }
            ]
          | "meta_implies" LEFTA
            [ t1 = bbmterm; sl_meta_right_arrow; t2 = bbmterm ->
               begin
                  match t1 with
                     { vname = None; vterm = t } ->
                        { vname = None; vterm = MetaImplies (t, t2.vterm) }
                   | { vname = Some n; vterm = t } ->
                        { vname = None; vterm = create_meta_function n t t2.vterm }
               end
            ]
          | "meta_rev_implies" RIGHTA
            [ t2 = bbmterm; sl_meta_left_arrow; t1 = bbmterm ->
               begin
                  match t1 with
                     { vname = None; vterm = t } ->
                        { vname = None; vterm = MetaImplies (t, t2.vterm) }
                   | { vname = Some n; vterm = t } ->
                        { vname = None; vterm = create_meta_function n t t2.vterm }
               end
            ]
          | "meta_iff" LEFTA
            [ t1 = bbmterm; sl_meta_left_right_arrow; t2 = bbmterm ->
               { vname = None; vterm = MetaIff (t1.vterm, t2.vterm) }
            ]
         ];

      (*
       * Regular terms.
       * term: any possible term
       * aterm: annotated term
       * noncommaterm: any possible term that is not a pair
       * nonsimpleterm: a term that is not an expression, and
       *     not a simple operator
       * simpleterm: a term that is just an operator
       * quote_term: a tuple of opname * params * bterms
       *)
      term_eoi: [[ x = term; EOI -> x ]];

      parsed_term:
         [[ t = term ->
             parse_term _loc t
          ]];

      parsed_bound_term:
         [[ t = bound_term ->
             parse_bound_term _loc t
          ]];

      term:
         [[ x = aterm ->
               get_aterm _loc x
         ]];

      aterm:
         ["comma" LEFTA
          [ x = noncommaterm ->
             make_aterm x
           | x = noncommaterm; ","; y = noncommaterm ->
             { aname = None; aterm = mk_pair_term _loc (make_term x) (make_term y) }
         ]];

      noncommaterm:
         [
           "ite" LEFTA
            [ "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
               wrap_term _loc (mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname _loc "ifthenelse") (make_term e1) (make_term e2) (make_term e3))
            ]

          (* Logical operators *)
          | "implies" RIGHTA
            [ t1 = SELF; op = sl_implies; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            ]
          | "or" RIGHTA
            [ t1 = SELF; op = sl_or; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            ]
          | "and" RIGHTA
            [ t1 = SELF; op = sl_and; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            ]
          | "quantify" LEFTA
            [ (* all/exists*)
               op = sl_quantify; v = var; ":"; t1 = SELF; "."; t2 = SELF ->
                  wrap_term _loc (mk_dep0_dep1_term (mk_dep0_dep1_opname _loc op) v (make_term t1) (make_term t2))
            |(* dall/dexists*)
               op = sl_quantify; v = var; sl_set_in; t1 = SELF; "."; t2 = SELF ->
                  wrap_term _loc (mk_dep0_dep1_term (mk_dep0_dep1_opname _loc  ("d"^op)) v (make_term t1) (make_term t2))
            |(* sall/sexists*)
              op = sl_quantify; v = var; "."; t2 = SELF ->
                  wrap_term _loc (mk_dep1_term (mk_dep1_opname _loc ("s"^op)) v (make_term t2))
            |(* thereis/forall *)
              op = sl_open_quantify; t1 = SELF; "."; t2 = SELF ->
                  wrap_term _loc (mk_dep0_dep1_term (mk_dep0_dep1_opname _loc op) (Lm_symbol.add "self") (make_term t1) (make_term t2))
            ]

          (* Relations *)
          | "equal" NONA
            [ (* t1 = t2 in ty,   t1 <> t2 in ty -- (in)equality in type *)
              t1 = SELF; (op,_) = sl_equal_rel; t2 = NEXT; sl_in; ty = SELF ->
               wrap_term _loc (mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname _loc op) (make_term ty) (make_term t1) (make_term t2))
            | (* t1 in ty  *)
              t = SELF; op = sl_in; ty = NEXT ->
               (* XXX HACK - this is to support ad-hoc I/O form "member" - see TODO 2.14 -2.15 *)
                  let t =
                     let ty = make_term ty in let t = make_term t in
                     try mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname _loc "equal") ty t t
                     with Failure _ | Not_found | Ploc.Exc (_, Not_found) | Ploc.Exc (_, Failure _) ->
                        mk_dep0_dep0_term (mk_dep0_dep0_opname _loc "member") t ty
                  in
                     wrap_term _loc t
            | (* t1 in t2 subset t3 *)
              t1 = SELF; sl_in; t2 = NEXT; sl_subset; t3 = SELF ->
               wrap_term _loc (mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname _loc "member") (make_term t1) (make_term t2) (make_term t3))
            | (* t1 subset t2 *)
              t1 = SELF; op = sl_subset; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            | (* t1 = t2, t1 <> t2  *)
              t1 = SELF; (op,_) = sl_equal_rel; t2 = NEXT ->
                mk_arith_term _loc op t1 t2
              (* t1 =[g] t2, t1 <>[g] t2  - algebraic relations for g *)
            | t1 = SELF; (_,op) = sl_equal_rel; "["; g = aterm; "]"; t2 = SELF ->
               make_application _loc [mk_field_term _loc (get_aterm _loc g) op; make_term t1; make_term t2]
            | (* t1 ~ t2 - squiggle equality  *)
              t1 = SELF; "~"; t2 = SELF ->
               (* XXX: HACK - Perv!rewrite should be eventially replaced by mk_opname _loc ["sqeq"] *)
               (* wrap_term _loc (mk_dep0_dep0_term (mk_dep0_dep0_opname _loc "sqeq") (make_term t1) (make_term t2)) *)
                  wrap_term _loc (mk_xrewrite_term (make_term t1) (make_term t2))
            | (* t1 In t2  - membership for set theory *)
              t1 = SELF; op = sl_set_in; t2 = SELF ->
               wrap_term _loc (mk_dep0_dep0_term (mk_dep0_dep0_opname _loc op) (make_term t1) (make_term t2))
            ]
          | "let" LEFTA
            [ "let"; x = var; "="; e1 = NEXT; "in"; e2 = SELF ->
               wrap_term _loc (mk_dep0_dep1_term (mk_dep0_dep1_opname _loc "let") x (make_term e1) (make_term e2))
            | e2 = SELF; "where"; x = var; "="; e1 = SELF ->
               wrap_term _loc (mk_dep0_dep1_term (mk_dep0_dep1_opname _loc "let") x (make_term e1) (make_term e2))
            | "open";  e1 = SELF; "in"; e2 = SELF ->
               wrap_term _loc (mk_dep0_dep1_term (mk_dep0_dep1_opname _loc "let") (Lm_symbol.add "self") (make_term e1) (make_term e2))
            ]
          | "compare" NONA
            [ (* t1 =@ t2, t1 <>@ t2, t1 <@ t2, t1 >@ t2, ...  - integer relations as booleans *)
               t1 = SELF; op = sl_arith_rel; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            | (* t1 < t2, t1 > t2, ...  - integer relations as propositions *)
               t1 = SELF; (op,_) = sl_rel;  t2 = SELF ->
               mk_arith_term _loc op t1 t2
            | (* t1 ^= t2, t1 ^<> t2, t1 ^< t2, t1 ^> t2, ...  - algebraic relations for self *)
               t1 = SELF; op = sl_label_self_rel;  t2 = SELF ->
               make_application _loc [mk_field_self_term _loc op; make_term t1; make_term t2]
            | (* t1 <[g] t2, t1 >[g] t2, ...  - algebraic relations for g *)
               t1 = SELF; (_,op) = sl_rel; "["; g = aterm; "]"; t2 = SELF ->
               make_application _loc [mk_field_term _loc (get_aterm _loc g) op; make_term t1; make_term t2]
            ]

          (* Other operations *)
          | "cons" RIGHTA
            [ t1 = SELF; op = sl_double_colon; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            ]
          | "fun" RIGHTA
           [ t1 = SELF; op = sl_arrow; t2 = SELF ->
              mk_type_term _loc op t1 t2
           ]
          | "rev_fun" LEFTA
            [ t1 = SELF; op = sl_left_arrow; t2 = SELF ->
               mk_arith_term _loc op t2 t1
            ]
          | "isect" RIGHTA
            [ (* Isect x: A. B[x], Union x:A. B[x]  - intersection, union of family of types *)
               op = sl_Isect; v = var; ":"; t1 = SELF; "."; t2 = SELF ->
               wrap_term _loc (mk_dep0_dep1_term (mk_dep0_dep1_opname _loc op) v (make_term t1) (make_term t2))
             | (* A union B, A isect B, x: A isect B[x]  - binary union, intersection and dependent intersection *)
               t1 = SELF; op = sl_isect; t2 = SELF ->
               mk_type_term _loc op t1 t2
             |(* quot x,y: t1 // t2  - quotient type *)
               op = sl_quotient; x = var; ","; y = var; ":"; t1 = SELF; "//"; t2 = SELF ->
               wrap_term _loc (mk_dep0_dep2_term (mk_dep0_dep2_opname _loc op) x y (make_term t1) (make_term t2))
            ]
          | "plus" RIGHTA
            [  (* t1 +[g] t2  - algebraic plus *)
               t1 = SELF; op = sl_plus; "["; g = aterm;  "]"; t2 = SELF ->
               make_application _loc [mk_field_term _loc (get_aterm _loc g) op; make_term t1; make_term t2]
             | (* t1 + t2 - disjoint union *)
               t1 = SELF; sl_plus; t2 = SELF ->
               mk_arith_term _loc "union" t1 t2
             | (* t1 ^+ t2   - algebraic plus for self *)
               t1 = SELF; op = sl_label_self_plus;  t2 = SELF ->
               make_application _loc [mk_field_self_term _loc op; make_term t1; make_term t2]
            ]
          | "add" LEFTA
            [ (* t1 +@ t2, t1 -@ t2 - integer plus, minus *)
               t1 = SELF; op = sl_arith_add; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            | (* t1 ^- t2   - algebraic minus for self *)
               t1 = SELF; op = sl_label_self_minus;  t2 = SELF ->
               make_application _loc [mk_field_self_term _loc op; make_term t1; make_term t2]
            | (* t1 -[g] t2   - algebraic minus for g *)
               t1 = SELF; op = sl_add; "["; g = aterm; "]"; t2 = SELF ->
               make_application _loc [mk_field_term _loc (get_aterm _loc g) op; make_term t1; make_term t2 ]
            ]
          | "minus"
            [ "-"; n = INT ->
                let n = Lm_num.neg_num (Lm_num.num_of_string n) in
                    wrap_term _loc (mk_term (mk_op (mk_opname _loc ["number"] [ShapeNumber] []) [make_param (Number n)]) [])
            | "-"; x = SELF ->
                wrap_term _loc (mk_dep0_term (mk_dep0_opname _loc "minus") (make_term x))
            ]
          | "prod" RIGHTA
            [ (* t1 *[g] t2  - algebraic multiplication (e.g. group operation) *)
               t1 = SELF; op = sl_star; "["; g = aterm;  "]"; t2 = SELF ->
               make_application _loc [mk_field_term _loc (get_aterm _loc g) op; make_term t1; make_term t2]
             | (* t1 * t2 - type product *)
               t1 = SELF; sl_star; t2 = SELF ->
               mk_type_term _loc "prod" t1 t2
             | (* t1 ^* t2   - algebraic multiplication for self *)
               t1 = SELF; op = sl_label_self_star;  t2 = SELF ->
               make_application _loc [mk_field_self_term _loc op; make_term t1; make_term t2]
            ]
          | "mul" LEFTA
            [ (* t1 *@ t2, t1 /@ t2, t1 %@ t2  - integer multiplication, division, reminder *)
               t1 = SELF; op = sl_arith_mul; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            | (* t1 ^/ t2   - algebraic division for self *)
               t1 = SELF; op = sl_label_self_div;  t2 = SELF ->
               make_application _loc [mk_field_self_term _loc op; make_term t1; make_term t2]
            | (* t1 /[g] t2   - algebraic right division for g *)
               t1 = SELF; op = sl_div; "["; g = aterm; "]"; t2 = SELF ->
               make_application _loc [mk_field_term _loc (get_aterm _loc g) op; make_term t1; make_term t2]
            ]

          | "apply" LEFTA
            [ t1 = SELF; t2 = SELF ->
               make_application _loc [make_term t1; make_term t2]
            ]

          | "power" RIGHTA
            [ (* t1 ^@ t2  - integer power *)
               t1 = SELF; op = sl_arith_power; t2 = SELF ->
               mk_arith_term _loc op t1 t2
            | (* t1 ^^ t2   - algebraic power for self *)
               t1 = SELF; op = sl_label_self_power;  t2 = SELF ->
               make_application _loc [mk_field_self_term _loc op; (make_term t1); (make_term t2)]
            | (* t1 ^[g] t2   - algebraic power for g *)
               t1 = SELF; op = sl_power; "["; g = aterm; "]"; t2 = SELF ->
               make_application _loc [mk_field_term _loc (get_aterm _loc g) op; (make_term t1); (make_term t2)]
            | (* r ^ lab - field selection for records *)
              r = SELF; sl_power; lab = word_or_string  ->
               wrap_term _loc (mk_field_term _loc (make_term r) lab)
            | (* r ^ lab := t - field update for records *)
               r = SELF; sl_power; lab = word_or_string; sl_assign; t = noncommaterm  ->
               wrap_term _loc (**)
                  (mk_term (mk_op (mk_opname _loc ["rcrd"] [ShapeToken] [0;0])
                           [make_param (Token (mk_token_opname _loc lab))])  [mk_simple_bterm (make_term t); mk_simple_bterm (make_term r)])
            | (* ^ lab  - field selection for self *)
              sl_power; lab = word_or_string  ->
               wrap_term _loc (mk_field_self_term _loc lab)
            | (* ^ lab - field update for records *)
               sl_power; lab = word_or_string; sl_assign; t = noncommaterm   ->
               wrap_term _loc (**)
                  (mk_term (mk_op (mk_opname _loc ["rcrd"] [ShapeToken] [0;0]) [make_param (Token (mk_token_opname _loc lab))]) (**)
                           [mk_simple_bterm (make_term t); mk_simple_bterm (mk_var_term (Lm_symbol.add "self"))])
           ]

          | "raw" RIGHTA
            [ op = opname_list ->
              begin match op with
                 [name] ->
                    ST_String(name, _loc)
               | _ ->
                    wrap_term _loc (mk_term (mk_op (mk_opname _loc op [] []) []) [])
              end
            | op = opname_list; (params, bterms) = termsuffix ->
              wrap_term _loc (mk_term (mk_op (mk_bopname _loc op params bterms) params) bterms)
            | op = opname_list; ":"; t = SELF ->
              match op with
                 [name] ->
                    ST_Term({ aname = Some (mk_var_term (Lm_symbol.add name)); aterm = make_term t }, _loc)
               | _ ->
                    Ploc.raise _loc (ParseError "illegal binding variable")
            ]

            (* type constraints *)
          | "constrain" NONA
            [ arg = SELF; ":>"; ty = SELF ->
                 wrap_term _loc (mk_ty_constrain_term (make_term arg) (make_term ty))
            ]

            (* short form for sequents *)
          | "sequent" NONA
            [ arg = SELF; "{|"; (hyps, concl) = sequent_body; "|}" ->
               wrap_term _loc (mk_sequent_term { sequent_args = make_term arg; sequent_hyps = hyps; sequent_concl = concl })
            ]

          | "type" NONA
            [ t = SELF; op = sl_type ->
               wrap_term _loc (mk_dep0_term (mk_dep0_opname _loc op) (make_term t))
            ]

          | [ t = nonwordterm ->
               wrap_term _loc t
            ]
         ];

      (* Singleterm is a distinct term and no colons *)
      singleterm:
         [ [ op = opname_list ->
              { aname = None; aterm = mk_term (mk_op (mk_opname _loc op [] []) []) [] }
            | op = opname_list; (params, bterms) = termsuffix ->
              { aname = None; aterm = mk_term (mk_op (mk_bopname _loc op params bterms) params) bterms }
           ]
          | "sequent" NONA
            [ arg = SELF; "{|"; (hyps, concl) = sequent_body; "|}" ->
               { aname = None; aterm = mk_sequent_term
                   { sequent_args = get_aterm _loc arg; sequent_hyps = hyps; sequent_concl = concl }}
            ]

          | [ t = nonwordterm ->
               { aname = None; aterm = t }
            ]
         ];

      bound_term:
         [ [ "("; v = varterm; ":"; t = aterm; ")" ->
               { aname = Some v; aterm = (get_aterm _loc t) }
           ]
          | [ t = singleterm ->
              t
           ]
         ];

      termsuffix:
         [[ p = params ->
             p, []
           | p = params; "{"; bterms = btermslist; "}" ->
             p, bterms
           | "{"; bterms = btermslist; "}" ->
             [], bterms
          ]];

      rcrdterm:
         [[ ";"; lab = word_or_string; "="; t = aterm  ->
               lab, t
         ]];

      recordterm:
         [[ lab = word_or_string; ":"; t = aterm  ->
             Some lab, t
           ]
         |[ t = aterm  ->
             None, t
           ]];

      nonwordterm:
         [[ (* vars *)
             v = varterm -> v

             (* Abbreviations *)
           | i = sl_number ->
             mk_term (mk_op (mk_opname _loc ["number"] [ShapeNumber] [])
                               [make_param (Number i)]) []
           | x = sequent ->
             x

             (* records {x1=a1;x2=a2;...} *)
           | "{"; lab = word_or_string; "="; t = aterm; rest = LIST0 rcrdterm; "}" ->
                let r0 =   mk_term (mk_op (mk_opname _loc ["rcrd"] [] []) []) [] in
                let aux = fun r -> function (lab,t) ->
                           mk_term (mk_op (mk_opname _loc ["rcrd"] [ShapeToken] [0;0])
                               [make_param (Token (mk_token_opname _loc lab))])  [mk_simple_bterm (get_aterm _loc t); mk_simple_bterm  r]
                in
                   List.fold_left aux r0 ((lab,t)::rest)
             (* record typess {x1:A1;x2:a2;...} *)
           | "{"; lab = word_or_string; ":"; t = aterm; ";"; r = LIST0 recordterm SEP ";"; "}" ->
                let r0 =   mk_term (mk_op (mk_opname _loc ["record"] [ShapeToken] [0])
                               [make_param (Token (mk_token_opname _loc lab))])  [mk_simple_bterm (get_aterm _loc t)] in
                let aux = fun r -> function
                      (Some lab,t) ->
                           mk_term (mk_op (mk_opname _loc ["record"] [ShapeToken] [1;0])
                               [make_param (Token (mk_token_opname _loc lab))])  [mk_bterm [Lm_symbol.add "self"]
                               (get_aterm _loc t); mk_simple_bterm  r]
                   |  (None,t) ->
                           mk_dep0_dep1_term (mk_dep0_dep1_opname _loc "set") (Lm_symbol.add "self") r (get_aterm _loc t)
                in
                    List.fold_left aux r0 r
             (* single record types {x1:A1} *)
           | "{"; lab = word_or_string; ":"; t = aterm; "}" ->
                mk_term (mk_op (mk_opname _loc ["record"] [ShapeToken] [0])
                        [make_param (Token (mk_token_opname _loc lab))])  [mk_simple_bterm (get_aterm _loc t)]
             (* sets {x:A | P[x]} *)
           | "{"; v = word_or_string; ":"; ty = aterm; "|"; b = aterm; "}" ->
             mk_dep0_dep1_term (mk_dep0_dep1_opname _loc "set") (Lm_symbol.add v) (get_aterm _loc ty) (get_aterm _loc b)
             (* very dependent functions {f | x:A -> B[x]} *)
           | "{"; f =  word_or_string; "|"; t = aterm; "}" ->
             let t = get_aterm _loc t in let f = Lm_symbol.add f in
             let rfun_op = mk_dep0_dep2_opname _loc "rfun" in
                if is_dep0_dep1_term (mk_dep0_dep1_opname _loc "fun") t then
                   let v, a, b = dest_dep0_dep1_any_term t in
                      mk_dep0_dep2_term rfun_op f v a b
                else if is_dep0_dep0_term (mk_dep0_dep0_opname _loc "fun") t then
                   let a, b = two_subterms t in
                      mk_dep0_dep2_term rfun_op f (mk_gensym ()) a b
                else
                   Ploc.raise _loc (ParseError "body of <rfun> is not a function")
            | "!"; v = var ->
               encode_free_var v
            | x = QUOTATION ->
               let name, s = dest_quot x in
                  parse_quotation _loc "term" name s
            | x = ANTIQUOT ->
               let name, s = dest_quot x in
                  parse_quotation _loc "term" name s
            | "("; t = aterm; ")" ->
               get_aterm _loc t
          ]];

      var: [[ v = word_or_string -> Lm_symbol.add v ]];

      varterm:
         [[ "'"; v = var ->
             begin match mk_var_contexts _loc v 0 with Some conts -> mk_so_var_term v conts [] | None -> mk_var_term v end
           | "'"; v = var; sl_contexts_left; conts = LIST0 var SEP ";"; sl_contexts_right ->
               mk_so_var_term v conts []
           | "'"; v = var; sl_contexts_left; conts = LIST0 var SEP ";"; sl_contexts_right; "["; terms = termlist; "]" ->
               mk_so_var_term v conts terms
           | "'"; v = var; sl_contexts_empty ->
               mk_so_var_term v [] []
           | "'"; v = var; sl_contexts_empty; "["; terms = termlist; "]" ->
               mk_so_var_term v [] terms
           | "'"; v = var; "["; terms = termlist; "]" ->
               mk_so_var_term v (get_var_contexts _loc v terms) terms
           | "'"; v = var; sl_contexts_left; conts = LIST0 var SEP ";"; sl_contexts_right; "["; "["; t = term; "]"; "]"; terms = optbrtermlist ->
               mk_context_term v t conts terms
           | "'"; v = var; sl_contexts_empty; "["; "["; t = term; "]"; "]"; terms = optbrtermlist ->
               mk_context_term v t [] terms
           | "'"; v = var; "["; "["; t = term; "]"; "]"; terms = optbrtermlist ->
               mk_context_term v t (get_var_contexts _loc v terms) terms
          ]];

      (* List of terms *)
      termlist:
         [[ l = LIST0 term SEP ";" -> l ]];

      (* Local opname *)
      opname_name:
         [[ s = word_or_string ->
             Opname.mk_opname s (opname_prefix _loc)
          ]];

      opname_list:
         [[ op = LIST1 word_or_string SEP "!" ->
             op
          ]];

      opname_param:
         [[ op = LIST1 word_or_string_sw SEP "!" ->
             op
         ]];

      opname:
         [[ op = opname_list ->
             mk_opname _loc op [] []
          ]];

      (* Parameters *)
      params:
         [[ "["; params = LIST0 param SEP ","; "]" ->
             params
          ]];

      param:
         [[ w = param_const ->
             w
           | w = param_const; ":"; t = opname_param ->
             cast_param _loc w t
           | "("; t = term; ")"; ":"; pt = opname_param ->
             cast_param _loc (make_param (Operator (opparam_of_term t))) pt
          ]];

      param_const:
         [[  w = opname_param ->
             make_param_opname _loc w
           | n = sl_number ->
             make_param (Number n)
           | "-"; n = sl_number ->
             make_param (Number (Lm_num.mult_num n (Lm_num.num_of_int (-1))))
           | "@" ->
             make_param Quote
          ]
          |
          [ p = param_const; "'" ->
             make_param (MLevel (incr_level_exp (cast_level p)))
           | p1 = param_const; "|"; p2 = param_const ->
             make_param (MLevel (max_level_exp (cast_level p1) (cast_level p2) 0))
          ]];

      (* Normal bound terms *)
      btermslist:
         [[ bterms = LIST0 bterm SEP ";" ->
                List.map mk_bterm' bterms
          ]];

      bterm:
         [[ h = bhead ->
             [], tupelize _loc h
           | h = bhead; "."; t = term ->
             List.map make_bvar h, t
           | "."; t = term ->
             [], t
          ]];

      bhead:
         [[ l = LIST1 noncommaterm SEP "," ->
             l
          ]];

      (************************************************************************
       * Typed versions of bterms.
       *)

      (*
       * For declarations.
       *)
      quote_term:
         [[ (t, opname, ty_params, ty_bterms) = quote_term_arg; ty = ty_opt_constraint ->
             { ty_term   = t;
               ty_opname = opname;
               ty_params = ty_params;
               ty_bterms = ty_bterms;
               ty_type   = ty
             }
           | "sequent"; (ty_hyp, ty_concl) = ty_sequent; ty_seq = ty_opt_constraint ->
             let opname = Opname.mk_opname "sequent_arg" (opname_prefix _loc) in
             let t = mk_term (mk_op opname []) [] in
                { ty_term   = t;
                  ty_opname = opname;
                  ty_params = [];
                  ty_bterms = [];
                  ty_type   = mk_ty_sequent_term ty_hyp ty_concl ty_seq
                }
           | "sequent"; "["; (t, opname, ty_params, ty_bterms) = quote_term_arg; "]";
             (ty_hyp, ty_concl) = ty_sequent; ty_seq = ty_opt_constraint ->
                { ty_term   = t;
                  ty_opname = opname;
                  ty_params = ty_params;
                  ty_bterms = ty_bterms;
                  ty_type   = mk_ty_sequent_term ty_hyp ty_concl ty_seq
                }
          ]];

      quote_term_arg:
         [[ opname = word_or_string; params = ty_opt_params; bterms = ty_opt_bterms ->
             let params, ty_params = params in
             let bterms, ty_bterms = bterms in
             let opname = Opname.mk_opname opname (opname_prefix _loc) in
             let t = mk_term (mk_op opname params) bterms in
                t, opname, ty_params, ty_bterms
          ]];

      (*
       * Sequent type.
       *)
      ty_sequent:
         [[ ty_seq = OPT ty_sequent_terms ->
             match ty_seq with
                Some (ty_hyp, ty_concl) ->
                   ty_hyp, ty_concl
              | None ->
                   mk_ty_hyp_term unknown_type unknown_type, unknown_type
          ]];

      ty_sequent_terms:
         [[ "{"; ty_hyp = ty_hyp; ">-"; ty_concl = term; "}" ->
             ty_hyp, ty_concl
          ]];

      ty_hyp:
         [[ cases = ty_hyp_cases ->
               (match cases with
                  [ty_var, ty_hyp] ->
                     mk_ty_hyp_term ty_var ty_hyp
                | _ ->
                     mk_ty_hyp_cases_term (List.rev cases))
           | "exst"; v = var; ":"; ty_var = singleterm; "."; ty_hyp = SELF ->
             let ty_var = get_aterm _loc ty_var in
                mk_ty_exists_term v ty_var ty_hyp
          ]];

      ty_hyp_cases:
         [[ case = ty_hyp_case ->
             [case]
          | cases = ty_hyp_cases; "|"; case = ty_hyp_case ->
             case :: cases
          ]];

      ty_hyp_case:
         [[ ty_var = singleterm; ":"; ty_hyp = singleterm ->
               let ty_var = get_aterm _loc ty_var in
               let ty_hyp = get_aterm _loc ty_hyp in
                  ty_var, ty_hyp
          ]];

      (*
       * Parameters.
       *)
      ty_opt_params:
         [[ params = OPT ty_params ->
             match params with
                Some params -> params
              | None -> [], []
          ]];

      ty_params:
         [[ "["; params = LIST0 ty_param SEP ","; "]" ->
             List.split params
          ]];

      ty_param:
         [[ w = param_const ->
             w, ty_param_of_param w
           | w = param_const; ":"; t = noncommaterm ->
             cast_ty_param _loc w t
          ]];

      (*
       * Bterms.
       *)
      ty_opt_bterms:
         [[ bterms = OPT ty_bterms ->
             match bterms with
                Some bterms -> bterms
              | None -> [], []
         ]];

      ty_bterms:
         [[ "{"; bterms = ty_bterms_list; "}" ->
             bterms
         ]];

      ty_bterms_list:
         [[ bterms = LIST0 ty_bterm SEP ";" ->
             List.split bterms
         ]];

      ty_bterm:
         [[ h = ty_bhead ->
             let term, ty = make_ty_term _loc h in
             let bterm = mk_bterm [] term in
             let ty_bterm = { ty_bvars = []; ty_bterm = ty } in
                bterm, ty_bterm
          | h = ty_bhead; "."; t = term; ty = ty_opt_constraint ->
             let bvars = List.map make_ty_bvar h in
             let bvars, ty_bvars = List.split bvars in
             let bterm = mk_bterm bvars t in
             let ty_bterm = { ty_bvars = ty_bvars; ty_bterm = ty } in
                bterm, ty_bterm
          | "."; t = term; ty = ty_opt_constraint ->
             let bterm = mk_bterm [] t in
             let ty_bterm = { ty_bvars = []; ty_bterm = ty } in
                bterm, ty_bterm
         ]];

      ty_bhead:
         [[ l = LIST1 ty_bvar SEP "," ->
             l
         ]];

      ty_bvar:
         [[ id = noncommaterm ->
             TyBVar1 id
          | id = noncommaterm; ":"; ty = noncommaterm ->
             TyBVar2 (id, ty)
         ]];

      (*
       * Generic type constraints.
       *)
      ty_opt_constraint:
         [[ ty = OPT ty_constraint ->
             match ty with
                Some ty ->
                   ty
              | None ->
                   unknown_type
         ]];

      ty_constraint:
         [[ ":"; ty = singleterm ->
             get_aterm _loc ty
         ]];

      (************************************************************************
       * Special forms.
       *)
      sequent:
         [[ sl_sequent; "{"; (hyps, concl) = sequent_body; "}" ->
               mk_sequent_term {
                  sequent_args = mk_term (mk_op (mk_opname _loc ["sequent_arg"] [] []) []) [];
                  sequent_hyps = hyps;
                  sequent_concl = concl;
               }
          | sl_sequent; "["; arg = term; "]";
            "{"; (hyps, concl) = sequent_body; "}" ->
               mk_sequent_term {
                  sequent_args = arg;
                  sequent_hyps = hyps;
                  sequent_concl = concl
               }
          | sl_sequent; "[|"; arg = term; "|]";
            "{"; (hyps, concl) = sequent_body; "}" ->
               let arg = mk_simple_bterm arg in
               mk_sequent_term {
                  sequent_args = mk_term (mk_op (mk_opname _loc ["sequent_arg"] [] [0]) []) [arg];
                  sequent_hyps = hyps;
                  sequent_concl = concl
               }
          | sl_sequent; "("; arg = term; ")";
               "{"; (hyps, concl) = sequent_body; "}" ->
               mk_sequent_term { sequent_args = arg; sequent_hyps = hyps; sequent_concl = concl }
          ]];

      sequent_body:
         [[ hyps = LIST0 hyp SEP ";"; sl_turnstile; concl = OPT term ->
               SeqHyp.of_list hyps, concl_of_opt_concl concl
          | hyps = LIST0 hyp SEP ";" ->
               SeqHyp.of_list hyps, xconcl_term
          ]];

      hyp:
         [[ "<"; hash = OPT "#"; name = var; conts = OPT contslist; args=optbrtermlist; ">" ->
             let conts = match conts with Some conts -> conts | None -> get_var_contexts _loc name args in
             let conts = match hash with Some _ -> hash_sym :: conts | None -> conts in
               Context(name, conts, args)
          | v = LIDENT; ":"; t = aterm ->
             Hypothesis(Lm_symbol.add v, get_aterm _loc t)
          | v = UIDENT; ":"; t = aterm ->
             Hypothesis(Lm_symbol.add v, get_aterm _loc t)
          | v = STRING; ":"; t = aterm ->
             Hypothesis(Lm_symbol.add (Token.eval_string _loc v), get_aterm _loc t)
          | t = aterm ->
             Hypothesis(empty_var, get_aterm _loc t)
          ]];

      contslist:
         [[ sl_contexts_left; vl = LIST0 var SEP ";" ; sl_contexts_right -> vl
          | sl_contexts_empty -> []
          ]];

      optbrtermlist:
         [[ tl = OPT brtermlist ->
             match tl with
                Some l -> l
              | None -> []
          ]];

      brtermlist:
         [[ "["; l = termlist; "]" -> l ]];

      (*
       * A term describing the display form.
       * We allow a concatenation of terms.
       *)
      xdform:
         [[ l = LIST0 df_item ->
             mk_xlist_term l
         ]];

      df_item:
         [[ t = singleterm ->
             get_aterm _loc t
           | "`"; name = STRING ->
             mk_xstring_term (Token.eval_string _loc name)
          ]];

      (* Term constructor "programs" *)
      term_con_eoi:
         [[ con = con_term; EOI -> con ]];

      con_termlist:
         [[ l = LIST1 con_term SEP ";" -> l ]];

      con_term:
         [[ "!"; t = term; "!" -> ConTerm t
          | e = ANTIQUOT "arg" -> ConExpr (expr_of_arg _loc e)
          | e = ANTIQUOT -> ConExpr (expr_of_anti _loc "" e)
          | "'"; v = ANTIQUOT "arg" -> ConVar (expr_of_arg _loc v)
          | "'"; v = ANTIQUOT -> ConVar (expr_of_anti _loc "" v)
          | ophead = word_or_string; rest = con_term_after_word -> rest ophead
          | con = con_sequent -> con
         ]];

      con_opname_rest:
         [[ l = LIST0 con_opname1_rest -> l ]];

      con_opname1_rest:
         [[ "!"; w = word_or_string -> w ]];

      con_term_after_word:
         [[ op = con_opname_rest ->
               (fun rest -> ConConstruct (mk_opname _loc (rest :: op) [] [], [], []))
          | op = con_opname_rest; (params, bterms) = con_term_suffix ->
               let param_types = List.map snd params in
               let bterm_arities = List.map (fun (bvars, _) -> List.length bvars) bterms in
                  (fun rest -> ConConstruct (mk_opname _loc (rest :: op) param_types bterm_arities, params, bterms))
         ]];

      con_term_suffix:
         [[ p = con_params ->
             p, []
           | p = con_params; "{"; bterms = con_btermslist; "}" ->
             p, bterms
           | "{"; bterms = con_btermslist; "}" ->
             [], bterms
          ]];

      con_params:
         [[ "["; params = LIST0 con_param SEP ","; "]" ->
             params
          ]];

      con_param:
         [[ s = opname_param -> make_con_param_opname _loc s
          | s = opname_param; ":"; shape = con_param_shape -> make_con_param_opname_shape _loc s shape
          | n = sl_number -> ConPNum n, ShapeNumber
          | e = ANTIQUOT "int" -> ConPInt (expr_of_anti _loc "int" e), ShapeNumber
          | e = ANTIQUOT; ":"; shape = con_param_shape -> ConPExpr (expr_of_anti _loc "" e), shape
          ]];

      con_param_shape:
         [[ s = LIDENT ->
               match s with
                  "n" -> ShapeNumber
                | "s" -> ShapeString
                | "l" -> ShapeLevel
                | "t" -> ShapeToken
                | "v" -> ShapeVar
                | "sh" -> ShapeShape
                | "op" -> ShapeOperator
                |  s  -> raise (BadParamCast (make_param (String ""), s))
          ]];

      con_btermslist:
         [[ l = LIST0 con_bterm SEP ";" -> l ]];

      con_bterm:
         [[ v = LIDENT; b = con_bterm_body_or_suffix ->
               b v
          | e = ANTIQUOT "arg"; suff = OPT con_bterm_suffix ->
               let e = expr_of_arg _loc e in
                  (match suff with
                      Some (bv, bt) ->
                         (ConBVarExpr e :: bv), bt
                    | None ->
                         [], ConExpr e)
          | e = ANTIQUOT; suff = OPT con_bterm_suffix ->
               let e = expr_of_anti _loc "" e in
                  (match suff with
                      Some (bv, bt) ->
                         (ConBVarExpr e :: bv), bt
                    | None ->
                         [], ConExpr e)
          ]
          |
          [ t = con_term -> [], t ]];

      con_bterm_suffix:
         [[ ","; bt = con_bterm -> bt;
          | "."; t = con_term -> [], t
         ]];

      con_bterm_body_or_suffix:
         [[ ","; bt = con_bterm ->
               let bv, bt = bt in
                  (fun v -> ((ConBVarConst v :: bv), bt))
          | "."; t = con_term ->
               (fun v -> ([ConBVarConst v], t))
          | rest = con_term_after_word ->
               fun v -> [], rest v
          ]];

      con_hyps: [[ hyps = LIST0 con_hyp SEP ";"; sl_turnstile -> hyps ]];

      (* Special forms *)
      con_sequent:
         [[ sl_sequent; "{"; hyps = con_hyps; concl = con_concl; "}" ->
               let arg = ConTerm (mk_term (mk_op (mk_opname _loc ["sequent_arg"] [] []) []) []) in
                  ConSequent (arg, hyps, concl)
          | sl_sequent; "["; arg = con_term; "]"; "{"; hyps = con_hyps; concl = con_concl; "}" ->
                  ConSequent (arg, hyps, concl)
          | sl_sequent; "("; arg = con_term; ")"; "{"; hyps = con_hyps; concl = con_concl; "}" ->
                  ConSequent (arg, hyps, concl)
          ]];

      con_hyp:
         [[ "<"; name = word_or_string; args = con_optbrtermlist; ">" ->
              ConContext (<:expr< $str:name$ >>, args)
          | expr = ANTIQUOT "list" ->
              ConHypList (expr_of_anti _loc "list" expr)
          | v = word_or_string; rest = con_hyp_suffix ->
              rest v
          | v = ANTIQUOT "arg"; ":"; t = con_term ->
              ConHypothesis (expr_of_arg _loc v, t)
          | v = ANTIQUOT; ":"; t = con_term ->
              ConHypothesis (expr_of_anti _loc "" v, t)
          | t = con_term ->
              ConHypothesis (<:expr< Lm_symbol.empty_var >>, t)
          ]];

      con_hyp_suffix:
         [[ ":"; t = con_term ->
               fun v -> ConHypothesis (<:expr< Lm_symbol.add $str:v$ >>, t)
          | (params, bterms) = con_term_suffix ->
               (fun op ->
                  let param_types = List.map snd params in
                  let bterm_arities = List.map (fun (bvars, _) -> List.length bvars) bterms in
                     ConHypothesis (<:expr< Lm_symbol.empty_var >>, ConConstruct (mk_opname _loc [op] param_types bterm_arities, params, bterms)))
          | ->
               fun op -> ConHypothesis (<:expr< Lm_symbol.empty_var >>, ConTerm (mk_term (mk_op (mk_opname _loc [op] [] []) []) []))
          ]];

      con_concl:
         [[ concl = OPT con_term ->
               match concl with
                  Some con -> con
                | None -> ConTerm xconcl_term
          ]];

      con_optbrtermlist:
         [[ tl = OPT con_brtermlist ->
             match tl with
                Some l -> l
              | None -> []
          ]];

      con_brtermlist:
         [[ "["; l = con_termlist; "]" -> l ]];

      (* Terminals *)
      sl_contexts_left: [[ "<|" -> () ]];
      sl_contexts_right: [[ "|>" -> () ]];
      sl_contexts_empty: [[ "<||>" -> () ]];

      sl_meta_left_right_arrow:
         [[ "<-->" -> () ]];

      sl_meta_right_arrow:
         [[ "-->" -> () ]];

      sl_meta_left_arrow:
         [[ "<--" -> () ]];

      sl_in:
         [[ "in" -> ()
          | "IN" -> () ]];

      sl_set_in:
         [[ "In" -> "mem"
         ]];

      sl_double_colon:
         [[ "::" -> "cons" ]];

      sl_sequent:
         [[ "sequent" -> () ]];

      sl_turnstile:
         [[ ">-" -> () ]];

      sl_assign:
         [[ ":=" -> "assign" ]];

      sl_equal_rel:
         [[ "=" -> "equal","="
          | "<>" -> "nequal","<>"
          ]];

      sl_subset:
         [[ "subset" -> "subset" ]];

      sl_rel:
         [[ "<" -> "lt","<"
          | ">" -> "gt",">"
          | "<=" -> "le", "<="
          | ">=" -> "ge", ">="
          | "subtype" -> "subtype","subtype"
          ]];

      sl_arith_rel:
         [[ "=@" -> "beq_int"
          | "<@" -> "lt_bool"
          | ">@" -> "gt_bool"
          | "<=@" -> "le_bool"
          | ">=@" -> "ge_bool"
          | "<>@" -> "bneq_int"
          ]];

      sl_label_self_rel:
         [[ "^=" -> "="
          | "^<" -> "<"
          | "^>" -> ">"
          | "^<=" -> "<="
          | "^>=" -> ">="
          | "^<>" -> "<>"
          ]];

      sl_plus:
         [[ "+" -> "+"
          | "+|" -> "+|"
          | "+:"; op = LIDENT -> "+:" ^ op
         ]];

      sl_add: (* other operations with addition prioruty *)
         [[ "-" -> "-" ]];

      sl_arith_add:
         [[ "+@" -> "add"
          | "-@" -> "sub" ]];

      sl_label_self_plus:
         [[ "^+" -> "+"]];

      sl_label_self_minus:
         [[ "^-" -> "-"]];

      sl_star:
         [[ "*" -> "*"
          | "*." -> "*."
          | "*|" -> "*|"
          | "*:"; op = LIDENT -> "*:" ^ op
         ]];

      sl_div:
         [[ "/" -> "/"
          ]];

      sl_arith_mul:
         [[ "*@" -> "mul"
          | "/@" -> "div"
          | "%@" -> "rem" ]];

      sl_label_self_star:
         [[ "^*" -> "*"
          ]];

      sl_label_self_div:
         [[ "^/" -> "/"
          ]];

      sl_power:
         [[ "^" -> "^" ]];

      sl_arith_power:
         [[ "^@" -> "power" ]];

      sl_label_self_power:
         [[ "^^" -> "^" ]];

      sl_arrow:
         [[ "->" -> "fun" ]];

      sl_left_arrow:
         [[ "<-" -> "fun" ]];

      sl_or:
         [[ "or" -> "or" ]];

      sl_and:
         [[ "and" -> "and"
           | "&" -> "and"
          ]];

      sl_implies:
         [[ "=>" -> "implies"
          | "<=>" -> "iff"
          ]];

      sl_open_quantify:
         [[ "forany" -> "all"
          | "thereis" -> "exists"
          ]];

      sl_quantify:
         [[ "all" -> "all"
          | "exst" -> "exists"
          ]];

      sl_isect:
         [[ "isect" -> "bisect"
          | "bunion" -> "bunion"
          ]];

      sl_Isect:
         [[ "Isect" -> "isect"
          | "Union" -> "tunion"
          ]];

      sl_quotient:
         [[ "quot" -> "quot" ]];

      sl_type:
         [[ "Type" -> "type" ]];

(*
      sl_bind:
         [[ "bind" -> "bind" ]];
*)

      sl_number:
         [[ n = INT ->
             Lm_num.num_of_string n
          ]];

      (* Take a word or a string as an identifier *)
      word_or_string_sw:
         [[ name = UIDENT ->
             SW_Word name
           | name = LIDENT ->
             SW_Word name
           | name = STRING ->
             SW_String (Token.eval_string _loc name)
          ]];

      word_or_string:
         [[ name = UIDENT ->
             name
           | name = LIDENT ->
             name
           | name = STRING ->
             Token.eval_string _loc name
          ]];
   END
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
