(*
 * Extend the language with a term parser as the default quotation.
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

open Printf

open Mp_debug
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.RefineError
open Ml_string
open Simple_print
open Simple_print.SimplePrint

open Filter_type
open Filter_summary

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_grammar%t" eflush

let debug_grammar =
   create_debug (**)
      { debug_name = "grammar";
        debug_description = "display term parsing operations";
        debug_value = false
      }

(*
 * Grammars to extend.
 *)
module type TermGrammarSig =
sig
   val mk_opname : MLast.loc -> string list -> opname
   val term_eoi : term Grammar.Entry.e
   val term : term Grammar.Entry.e
   val quote_term : quote_term Grammar.Entry.e
   val mterm : meta_term Grammar.Entry.e
   val singleterm : aterm Grammar.Entry.e
   val bound_term : aterm Grammar.Entry.e
   val xdform : term Grammar.Entry.e
end

(*
 * Build the grammar.
 *)
module MakeTermGrammar (TermGrammar : TermGrammarSig) =
struct
   open TermGrammar

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Also meta-terms.
    *)
   type amterm = { mname : string option; mterm : meta_term }

   (*
    * String or term.
    *)
   type string_or_term =
      ST_String of string
    | ST_Term of (term * (int * int))

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   (*
    * For new symbols.
    *)
   let gensym = ref 0

   let mk_gensym () =
      incr gensym;
      "$" ^ (string_of_int !gensym)

   (* Currying *)
   let mk_bterm' (vars, bterm) = mk_bterm vars bterm

   (*
    * Construct an application.
    *)
   let mk_apply_term loc a b =
      mk_dep0_dep0_term (mk_opname loc ["apply"]) a b

   let make_application loc terms =
      (* Convert the list to an application *)
      let rec aux x = function
         [] -> x
       | h::t ->
            aux (mk_apply_term loc x h) t
      in
         match terms with
            [] -> raise (Invalid_argument "make_application")
          | h::t -> aux h t

   (*
    * Cast a parameter to a level expression.
    *)
   let cast_level p =
      match dest_param p with
         Number (Mp_num.Int n) ->
           mk_const_level_exp n
       | String s -> mk_var_level_exp s
       | Token t -> mk_var_level_exp t
       | Var v -> mk_var_level_exp v
       | Level l -> l
       | MNumber v -> mk_var_level_exp v
       | MString v -> mk_var_level_exp v
       | MVar v -> mk_var_level_exp v
       | _ -> raise (BadParamCast (p, "l"))

   (*
    * Cast to a number.
    *)
   let cast_number p =
      match dest_param p with
         Number _ -> p
       | MNumber _ -> p
       | MVar v -> make_param (MNumber v)
       | MString s -> make_param (MNumber s)
       | _ -> raise (BadParamCast (p, "n"))

   (*
    * Parameter casting.
    *)
   let cast_param p = function
      "n" -> cast_number p
    | "s" as x ->
         begin
            match dest_param p with
               Number(n) -> make_param (String (Mp_num.string_of_num n))
             | String(s) -> p
             | Token(t) -> make_param (String t)
             | Var(v) -> make_param (String v)
             | MString(v) -> p
             | _ -> raise (BadParamCast (p, x))
         end
    | "t" as x ->
         begin
            match dest_param p with
               Number(n) -> make_param (Token (Mp_num.string_of_num n))
             | String(s) -> make_param (Token s)
             | Token(t) -> p
             | Var(v) -> make_param (Token v)
             | MString(v) -> make_param (MToken v)
             | _ -> raise (BadParamCast (p, x))
         end

    | "v" as x ->
         begin
            match dest_param p with
               Number(n) -> make_param (Var (Mp_num.string_of_num n))
             | String(s) -> make_param (Var s)
             | Token(t) -> make_param (Var t)
             | Var(v) -> p
             | MString(v) -> make_param (MVar v)
             | _ -> raise (BadParamCast (p, x))
         end

    | "l" ->
         begin
            match dest_param p with
               MString v -> make_param (MLevel v)
             | _ -> make_param (Level (cast_level p))
         end

    | x -> raise (BadParamCast (p, x))

   (*
    * Constructors.
    *)
   let mk_pair_term loc a b =
      mk_dep0_dep0_term (mk_opname loc ["pair"]) a b

   (*
    * Turn a reversed list of terms into a tuple.
    *)
   let make_term loc = function
      ST_String s ->
         mk_term (mk_op (mk_opname loc [s]) []) []
    | ST_Term (t, _) ->
         t

   let rec tupelize loc = function
      [h] -> make_term loc h
    | h::t -> mk_pair_term loc (make_term loc h) (tupelize loc t)
    | [] -> raise (Invalid_argument "tupelize")

   (*
    * Construct a binary term, with a possible dependency.
    *)
   let mk_type_term loc name t1 t2 =
      match t1 with
         { aname = None; aterm = t } ->
            { aname = None; aterm = mk_dep0_dep0_term (mk_opname loc [name]) t t2.aterm }
       | { aname = Some name'; aterm = t } ->
            { aname = None; aterm = mk_dep0_dep1_term (mk_opname loc [name]) (dest_var name') t t2.aterm }

   let mk_arith_term loc name t1 t2 =
      { aname = None; aterm = mk_dep0_dep0_term (mk_opname loc [name]) t1.aterm t2.aterm }

   (*
    * Check that all are strings.
    *)
   let check_bvars l =
      let check = function
         ST_Term (_, loc) ->
            Stdpp.raise_with_loc loc (ParseError "Not a binding var")
       | ST_String s ->
            s
      in
         List.map check l

   (************************************************************************
    * GRAMMAR                                                              *
    ************************************************************************)

   EXTEND
      GLOBAL: term_eoi term quote_term mterm singleterm bound_term xdform;

      (*
       * Meta-terms include meta arrows.
       *)
      mterm:
         [[ t = amterm ->
             t.mterm
          ]];

      amterm:
         [[ t = noncommaterm ->
             match t with
                { aname = None; aterm = t } ->
                   { mname = None; mterm = MetaTheorem t }
              | { aname = Some n; aterm = t } ->
                   { mname = Some (dest_var n); mterm = MetaLabeled (dest_var n, MetaTheorem t) }
          ]
          | "meta_implies" LEFTA
            [ t1 = amterm; sl_meta_right_arrow; t2 = amterm ->
               begin
                  match t1 with
                     { mname = None; mterm = t } ->
                        { mname = None; mterm = MetaImplies (t, t2.mterm) }
                   | { mname = Some n; mterm = t } ->
                        { mname = None; mterm = MetaFunction (mk_var_term n, t, t2.mterm) }
               end
            ]
          | "meta_rev_implies" RIGHTA
            [ t2 = amterm; sl_meta_left_arrow; t1 = amterm ->
               begin
                  match t1 with
                     { mname = None; mterm = t } ->
                        { mname = None; mterm = MetaImplies (t, t2.mterm) }
                   | { mname = Some n; mterm = t } ->
                        { mname = None; mterm = MetaFunction (mk_var_term n, t, t2.mterm) }
               end
            ]
          | "meta_iff" LEFTA
            [ t1 = amterm; sl_meta_left_right_arrow; t2 = amterm ->
               { mname = None; mterm = MetaIff (t1.mterm, t2.mterm) }
            ]
          | NONA
            [ sl_open_paren; t = amterm; sl_close_paren ->
               t
            ]
         ];


      (*
       * Regular terms.
       * term: any possible term
       * aterm: annotated term
       * nocommaterm: any possible term that is not a pair
       * noncommaterm: a term that is not an operator or a pair
       * nonsimpleterm: a term that is not an expression, and
       *     not a simple operator
       * simpleterm: a term that is just an operator
       * quote_term: a tuple of opname * params * bterms
       *)
      term_eoi: [[ x = term; EOI -> x ]];

      term:
         [[ x = aterm ->
             x.aterm
          ]];

      aterm:
         ["comma" LEFTA
          [ x = noncommaterm ->
             x
           | x = noncommaterm; sl_comma; y = noncommaterm ->
             { aname = None; aterm = mk_pair_term loc x.aterm y.aterm }
          ]
         ];

      noncommaterm:
         [ "equal" LEFTA
            [ t1 = noncommaterm; op = sl_not_equal; t2 = noncommaterm; sl_in; ty = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep0_dep0_term (mk_opname loc [op]) ty.aterm t1.aterm t2.aterm }
             | t1 = noncommaterm; op = sl_equal; t2 = noncommaterm; sl_in; ty = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep0_dep0_term (mk_opname loc [op]) ty.aterm t1.aterm t2.aterm }
            ]
          | "fun" RIGHTA
           [ t1 = noncommaterm; op = sl_arrow; t2 = noncommaterm ->
              mk_type_term loc op t1 t2
           ]
          | "rev_fun" LEFTA
            [ t1 = noncommaterm; op = sl_left_arrow; t2 = noncommaterm ->
               mk_arith_term loc op t2 t1
            ]
          | "union" LEFTA
            [ t1 = noncommaterm; op = sl_plus; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "prod" LEFTA
            [ t1 = noncommaterm; op = sl_star; t2 = noncommaterm ->
               mk_type_term loc op t1 t2
            ]
          | "cons" RIGHTA
            [ t1 = noncommaterm; op = sl_double_colon; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "compare" LEFTA
            [ t1 = noncommaterm; op = sl_less_than; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
             | t1 = noncommaterm; op = sl_less_equal; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
             | t1 = noncommaterm; op = sl_greater_equal; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
             | t1 = noncommaterm; op = sl_greater_than; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "implies" RIGHTA
            [ t1 = noncommaterm; op = sl_implies; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "or" RIGHTA
            [ t1 = noncommaterm; op = sl_or; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "and" RIGHTA
            [ t1 = noncommaterm; op = sl_and; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "add" RIGHTA
            [ t1 = noncommaterm; op = sl_add; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
             | t1 = noncommaterm; op = sl_sub; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "mul" RIGHTA
            [ t1 = noncommaterm; op = sl_mul; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
             | t1 = noncommaterm; op = sl_div; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
             | t1 = noncommaterm; op = sl_rem; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "quantify" LEFTA
            [ op = sl_all; t1 = noncommaterm; sl_period; t2 = noncommaterm ->
               begin
                  match t1 with
                     { aname = None; aterm = t } ->
                        raise (ParseError (sprintf "no binding var for %s quantifier" op))
                   | { aname = Some name; aterm = t } ->
                        { aname = None; aterm = mk_dep0_dep1_term (mk_opname loc [op]) (dest_var name) t t2.aterm }
               end
             | op = sl_isect; t1 = noncommaterm; sl_period; t2 = noncommaterm ->
               begin
                  match t1 with
                     { aname = None; aterm = t } ->
                        raise (ParseError (sprintf "no binding var for %s quantifier" op))
                   | { aname = Some name; aterm = t } ->
                        { aname = None; aterm = mk_dep0_dep1_term (mk_opname loc [op]) (dest_var name) t t2.aterm }
               end
             | op = sl_exists; t1 = noncommaterm; sl_period; t2 = noncommaterm ->
               begin
                  match t1 with
                     { aname = None; aterm = t } ->
                        raise (ParseError (sprintf "no binding var for %s quantifier" op))
                   | { aname = Some name; aterm = t } ->
                        { aname = None; aterm = mk_dep0_dep1_term (mk_opname loc [op]) (dest_var name) t t2.aterm }
               end
             | op = sl_quotient; x = sl_word; sl_comma; y = sl_word; sl_colon; t1 = noncommaterm; sl_double_slash; t2 = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep2_term (mk_opname loc [op]) x y t1.aterm t2.aterm }
            ]
          | "apply" LEFTA
            [ t = applyterm ->
               t
             | t = applyterm; l = applytermlist ->
               { aname = None; aterm = make_application loc (t.aterm :: l) }
            ]
          | "uminus" RIGHTA
            [ op = sl_minus; x = noncommaterm ->
               { aname = None; aterm = mk_dep0_term (mk_opname loc [op]) x.aterm }
             | op = sl_not; x = noncommaterm ->
               { aname = None; aterm = mk_dep0_term (mk_opname loc [op]) x.aterm }
            ]
         ];

      (* Term that can be used in application lists *)
      applyterm:
         [ [ op = opname ->
              { aname = None; aterm = mk_term (mk_op (mk_opname loc op) []) [] }
            | op = opname; (params, bterms) = termsuffix ->
              { aname = None; aterm = mk_term (mk_op (mk_opname loc op) params) bterms }
            | op = opname; sl_colon; t = applyterm ->
              match op with
                 [name] ->
                    if !debug_grammar then
                       eprintf "Got bound term: %s%t" name eflush;
                    { aname = Some (mk_var_term name); aterm = t.aterm }
               | _ ->
                    Stdpp.raise_with_loc loc (ParseError "illegal binding variable")
           ]
          | [ t = nonwordterm ->
               t
            ]
         ];

      (* Singleterm is a distinct term and no colons *)
      singleterm:
         [ [ op = opname ->
              { aname = None; aterm = mk_term (mk_op (mk_opname loc op) []) [] }
            | op = opname; (params, bterms) = termsuffix ->
              { aname = None; aterm = mk_term (mk_op (mk_opname loc op) params) bterms }
           ]
          | [ t = nonwordterm ->
               t
            ]
         ];

      bound_term:
         [ [ sl_open_paren; v = varterm; sl_colon; t = aterm; sl_close_paren ->
               { aname = Some v; aterm = t.aterm }
           ]
          | [ t = singleterm ->
              t
           ]
         ];

      termsuffix:
         [[ p = params ->
             p, []
           | p = params; sl_open_curly; bterms = btermslist; sl_close_curly ->
             p, bterms
           | sl_open_curly; bterms = btermslist; sl_close_curly ->
             [], bterms
          ]];

      nonwordterm:
         [[ (* vars *)
             v = varterm ->
             { aname = None; aterm = v }
           | sl_wild_card ->
             { aname = None; aterm = mk_var_term (mk_gensym ()) }

             (* Abbreviations *)
           | i = sl_number ->
             { aname = None; aterm = mk_term (mk_op (mk_opname loc ["natural_number"])
                                              [make_param (Number i)]) (**)
                  []
             }
           | x = sequent ->
             { aname = None; aterm = x }

             (* Parenthesized terms *)
           | sl_open_paren; t = aterm; sl_close_paren ->
             t
           | sl_open_brack; t = aterm; sl_close_brack ->
             t
           | sl_open_curly; v = sl_word; sl_colon; ty = applyterm; sl_pipe; b = aterm; sl_close_curly ->
             { aname = None; aterm = mk_dep0_dep1_term (mk_opname loc ["set"]) v ty.aterm b.aterm }
           | sl_open_curly; f = sl_word; sl_pipe; t = aterm; sl_close_curly ->
             let t = t.aterm in
             let fun_opname = mk_opname loc ["fun"] in
             let t' =
                if is_dep0_dep0_term fun_opname t then
                   let a, b = dest_dep0_dep0_term fun_opname t in
                      mk_dep0_dep2_term (mk_opname loc ["rfun"]) f (mk_gensym ()) a b
                else if is_dep0_dep1_term fun_opname t then
                   let v, a, b = dest_dep0_dep1_term fun_opname t in
                      mk_dep0_dep2_term (mk_opname loc ["rfun"]) f v a b
                else
                   raise (ParseError "body of <rfun> is not a function")
             in
                { aname = None; aterm = t' }
          ]];

      varterm:
         [[ sl_single_quote; v = sl_word ->
             mk_var_term v
           | sl_single_quote; v = sl_word; sl_open_brack; terms = opttermlist; sl_close_brack ->
             mk_so_var_term v terms
          ]];

      quote_term:
         [[ v = word_or_string; params = optparams; bterms = optbterms ->
             v, params, bterms
          ]];

      (* Application lists *)
      applytermlist:
         [[ x = applyterm ->
             [x.aterm]
           | l = applytermlist; x = applyterm ->
             l @ [x.aterm]
          ]];

      (* List of terms *)
      opttermlist:
         [[ l = OPT termlist ->
             match l with
                Some l' -> l'
              | None -> []
          ]];

      termlist:
         [[ t = term ->
             [t]
           | l = termlist; sl_semi_colon; t = term ->
             l @ [t]
          ]];

      (* Parameters and bterm lists *)
      opname:
         [[ op = rev_opname ->
             List.rev op
          ]];

      rev_opname:
         [[ w = word_or_string ->
             [w]
           | l = rev_opname; sl_exclamation; w = word_or_string ->
             w :: l
          ]];

      optparams:
         [[ params = OPT params ->
             match params with
                Some params' -> params'
              | None -> []
          ]];

      params:
         [[ sl_open_brack; params = LIST0 param SEP ","; sl_close_brack ->
             params
          ]];

      (* Parameters *)
      param:
         [[ sl_at; w = sl_word ->
             make_param (MString w)
           | w = word_or_string ->
             make_param (String w)
           | n = sl_number ->
             make_param (Number n)
          ]
          | [ p = param; sl_colon; w = sl_word ->
               cast_param p w
             | p = param; sl_single_quote ->
               make_param (Level (incr_level_exp (cast_level p)))
             | p1 = param; sl_pipe; p2 = param ->
               make_param (Level (max_level_exp (cast_level p1) (cast_level p2)))
             | p1 = param; sl_plus; p2 = param ->
               make_param (MSum (cast_number p1, cast_number p2))
             | p1 = param; sl_minus; p2 = param ->
               make_param (MDiff (cast_number p1, cast_number p2))
             | p1 = param; sl_star; p2 = param ->
               make_param (MProduct (cast_number p1, cast_number p2))
             | p1 = param; sl_slash; p2 = param ->
               make_param (MQuotient(cast_number p1, cast_number p2))
             | p1 = param; sl_percent; p2 = param ->
               make_param (MRem (cast_number p1, cast_number p2))
             | p1 = param; sl_not_equal; p2 = param ->
               make_param (MNotEqual (cast_number p1, cast_number p2))
             | p1 = param; sl_equal; p2 = param ->
               make_param (MEqual (cast_number p1, cast_number p2))
             | p1 = param; sl_less_than; p2 = param ->
               make_param (MLessThan (cast_number p1, cast_number p2))
             | sl_open_paren; p = param; sl_close_paren ->
               p
            ]
         ];

      (* Bound terms *)
      optbterms:
         [[ bterms = OPT bterms ->
             match bterms with
                Some bterms' -> bterms'
              | None -> []
          ]];

      bterms:
         [[ sl_open_curly; bterms = btermslist; sl_close_curly ->
             bterms
          ]];

      btermslist:
         [[ l = OPT btermlist ->
             let l' =
                match l with
                   Some l' -> l'
                 | None -> []
             in
                List.map mk_bterm' l'
          ]];

      btermlist:
         [[ t = bterm ->
             [t]
           | l = btermlist; sl_semi_colon; t = bterm ->
             l @ [t]
          ]];

      bterm:
         [[ h = bhead ->
             [], tupelize loc h
           | h = bhead; sl_period; t = term ->
             check_bvars h, t
           | sl_period; t = term ->
             [], t
          ]];

      bhead:
         [[ t = bsingle ->
             [t]
           | h = bhead; sl_comma; w = bsingle ->
             h @ [w]
          ]];

      bsingle:
         [[ w = sl_word ->
             ST_String w
           | w = sl_word; (params, bterms) = termsuffix ->
             ST_Term (mk_term (mk_op (mk_opname loc [w]) params) bterms, loc)
           | t = nonwordterm ->
             ST_Term (t.aterm, loc)
          ]];

      (* Special forms *)
      sequent:
         [[ sl_sequent; args = optseqargs; sl_open_curly;
            hyps = LIST0 hyp SEP ";"; sl_turnstile;
            concl = LIST1 term SEP ";"; sl_close_curly ->
             let mk_hyp_term v t = Hypothesis (v, t) in
             let mk_context_term v subterms = Context (v, subterms) in
             let rec proc_hyps = function
                [] ->
                   []
              | h::tl ->
                   if !debug_grammar then
                      eprintf "Got hyp: %s%t" (SimplePrint.string_of_term h.aterm) eflush;
                   match h with
                      { aname = Some v; aterm = t } ->
                         mk_hyp_term (dest_var v) t :: proc_hyps tl
                    | { aname = None; aterm = t } ->
                         try
                            let v, subterms = dest_so_var t in
                               mk_context_term v subterms :: proc_hyps tl
                         with
                            RefineError (_, TermMatchError _) ->
                               Stdpp.raise_with_loc loc (**)
                                  (Failure (sprintf "Not a variable: %s" (SimplePrint.string_of_term t)))
             in
             let esequent =
                { sequent_args = mk_xlist_term args;
                  sequent_hyps = SeqHyp.of_list (proc_hyps hyps);
                  sequent_goals = SeqGoal.of_list concl
                }
             in
                if !debug_grammar then
                   eprintf "Constructing sequent: %d, %d%t" (List.length hyps) (List.length concl) eflush;
                mk_sequent_term esequent
          ]];

      hyp:
         [[ bvar = OPT [ name = sl_word; sl_colon -> name]; t = aterm ->
             let v =
                match bvar with
                   Some v' ->
                      Some (mk_var_term v')
                 | None ->
                      None
             in
                { aname = v; aterm = t.aterm }
          ]];


      optseqargs:
         [[ args = OPT seqargs ->
             match args with
                Some l -> l
              | None -> []
          ]];

      seqargs:
         [[ sl_open_brack; l = LIST0 term SEP ";"; sl_close_brack ->
             l
          ]];

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
             t.aterm
           | sl_back_quote; name = STRING ->
             mk_xstring_term name
          ]];

      (* Terminals *)
      sl_meta_left_right_arrow:
         [[ "<-->" -> () ]];

      sl_meta_right_arrow:
         [[ "-->" -> () ]];

      sl_meta_left_arrow:
         [[ "<--" -> () ]];

      sl_open_curly:
         [[ "{" -> () ]];

      sl_close_curly:
         [[ "}" -> () ]];

      sl_open_paren:
         [[ "(" -> () ]];

      sl_close_paren:
         [[ ")" -> () ]];

      sl_open_brack:
         [[ "[" -> () ]];

      sl_close_brack:
         [[ "]" -> () ]];

      sl_in:
         [[ "in" -> () ]];

      sl_colon:
         [[ ":" -> () ]];

      sl_double_colon:
         [[ "::" -> "cons" ]];

      sl_semi_colon:
         [[ ";" -> () ]];

      sl_slash:
         [[ "/" -> () ]];

      sl_double_slash:
         [[ "//" -> () ]];

      sl_comma:
         [[ "," -> () ]];

      sl_period:
         [[ "." -> () ]];

      sl_pipe:
         [[ "|" -> () ]];

      sl_at:
         [[ "@" -> () ]];

      sl_single_quote:
         [[ "'" -> () ]];

      sl_back_quote:
         [[ "`" -> () ]];

      sl_wild_card:
         [[ "_" -> () ]];

      sl_sequent:
         [[ "sequent" -> () ]];

      sl_turnstile:
         [[ ">-" -> () ]];

      sl_exclamation:
         [[ "!" -> () ]];

      sl_add:
         [[ "+@" -> "add" ]];

      sl_sub:
         [[ "-@" -> "sub" ]];

      sl_mul:
         [[ "*@" -> "mul" ]];

      sl_div:
         [[ "/@" -> "div" ]];

      sl_rem:
         [[ "%@" -> "rem" ]];

      sl_plus:
         [[ "+" -> "union" ]];

      sl_minus:
         [[ "-" -> "sub" ]];

      sl_star:
         [[ "*" -> "prod" ]];

      sl_percent:
         [[ "%" -> "rem" ]];

      sl_arrow:
         [[ "->" -> "fun" ]];

      sl_left_arrow:
         [[ "<-" -> "fun" ]];

      sl_less_than:
         [[ "<" -> "lt" ]];

      sl_less_equal:
         [[ "<=" -> "le" ]];

      sl_greater_than:
         [[ ">" -> "gt" ]];

      sl_greater_equal:
         [[ ">=" -> "ge" ]];

      sl_equal:
         [[ "=" -> "equal" ]];

      sl_not_equal:
         [[ "<>" -> "nequal" ]];

      sl_not:
         [[ "neg" -> "not" ]];

      sl_or:
         [[ "or" -> "or" ]];

      sl_and:
         [[ "and" -> "and"
           | "&" -> "and"
          ]];

      sl_implies:
         [[ "=>" -> "implies" ]];

      sl_all:
         [[ "all" -> "all" ]];

      sl_exists:
         [[ "exst" -> "exists" ]];

      sl_isect:
         [[ "isect" -> "isect" ]];

      sl_quotient:
         [[ "quot" -> "quot" ]];

(*
      sl_bind:
         [[ "bind" -> "bind" ]];
*)

      sl_number:
         [[ n = INT ->
             Mp_num.num_of_string n
          ]];

      (* Take a word or a string as an identifier *)
      word_or_string:
         [[ name = UIDENT ->
             name
           | name = LIDENT ->
             name
           | name = STRING ->
             name
          ]];

      sl_word:
         [[ s = LIDENT -> s
           | s = UIDENT -> s
          ]];
   END

   (* Implementation *)
   let mterm = mterm
   let quote_term = quote_term
   let term = term
   let term_eoi = term_eoi
   let singleterm = singleterm
   let bound_term = bound_term
   let mk_opname = mk_opname
   let xdform = xdform
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
