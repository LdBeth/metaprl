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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 * Modified By: Alexei Kopylov <kopylov@cs.cornell.edu>
 * Modified By: Adam Granicz <granicz@cs.caltech.edu>
 *)
open Lm_debug
open Lm_symbol
open Lm_printf

open Term_sig
open Term_shape_sig
open Refiner.Refiner
open Term
open TermType
open TermOp
open TermMan
open TermMeta
open TermShape

open Lexing
open Filter_type
open Filter_util

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_grammar%t"

let debug_grammar =
   create_debug (**)
      { debug_name = "grammar";
        debug_description = "display term parsing operations";
        debug_value = false
      }

let debug_spell =
   create_debug (**)
      { debug_name = "spell";
        debug_description = "check spelling";
        debug_value = false
      }

let _ =
   Grammar.error_verbose:=true;
   Grammar.warning_verbose:=true

(*
 * Terms for representing comments.
 *)
let mk_comment_opname =
   let op = Opname.mk_opname "Comment" Opname.nil_opname in
      fun s -> Opname.mk_opname s op

let comment_white_op = mk_comment_opname "comment_white"
let comment_string_op = mk_comment_opname "comment_string"
let comment_block_op = mk_comment_opname "comment_block"
let comment_term_op = mk_comment_opname "comment_term"

let misspelled = ref []
let dict_inited = ref false

let raise_spelling_error () =
   if !misspelled <> [] then begin
      let rec print word = function
         (h, (bp, ep)) :: t ->
            if word = h then
               eprintf "; "
            else
               eprintf "\n\t%s: " h;
            if bp.pos_lnum >= 0 then
               eprintf "line %i, char %i" bp.pos_lnum bp.pos_bol
            else
               eprintf "char %i" bp.pos_cnum;
            print h t
       | [] ->
            ()
      in
      let (word, loc) = Lm_list_util.last !misspelled in
      let l = Sort.list (<) !misspelled in
         misspelled := [];
         eprintf "The following words may be misspelled:";
         print "" l;
         eflush stderr;
         Stdpp.raise_with_loc loc (Failure ("spelling (" ^ word ^ ")"))
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
         if is_sequent_term left && not (is_sequent_term t) then replace_goal left t else t
   in
      MetaFunction(t, left, right)

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
   type vmterm = { vname : term option; vterm : meta_term }

   (*
    * String or term.
    *)
   type string_or_term =
      ST_String of (string * MLast.loc)
    | ST_Term of (aterm * MLast.loc)

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   let mk_0opname loc name =
      mk_opname loc [name] [] []

   let mk_dep0_opname loc name =
      mk_opname loc [name] [] [0]

   let mk_dep1_opname loc name =
      mk_opname loc [name] [] [1]

   let mk_dep0_dep0_opname loc name =
      mk_opname loc [name] [] [0;0]

   let mk_dep0_dep1_opname loc name =
      mk_opname loc [name] [] [0;1]

   let mk_dep0_dep2_opname loc name =
      mk_opname loc [name] [] [0;2]

   let mk_dep0_dep0_dep0_opname loc name =
      mk_opname loc [name] [] [0;0;0]

   let bterm_arities =
      List.map ( fun bt -> List.length (dest_bterm bt).bvars)

   let mk_bopname loc names params bterms =
      mk_opname loc names (List.map param_type params) (bterm_arities bterms)

   (*
    * For new symbols.
    *)
   let gensym = ref 0

   let mk_gensym () =
      incr gensym;
      Lm_symbol.make "$" !gensym

   (* Currying *)
   let mk_bterm' (vars, bterm) = mk_bterm vars bterm

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
       | MLevel l -> l
       | MString v -> level_var (string_of_symbol v)
       | _ -> raise (BadParamCast (p, "l"))

   (*
    * Cast to a number.
    *)
   let cast_number p =
      match dest_param p with
         Number _ | MNumber _ -> p
       | MString s -> make_param (MNumber s)
       | _ -> raise (BadParamCast (p, "n"))

   (*
    * Parameter casting.
    *)
   let cast_param p = function
      "n" -> cast_number p
    | "s" ->
         begin
            match dest_param p with
               Number(n) -> make_param (String (Lm_num.string_of_num n))
             | String _ | MString _ -> p
             | _ -> raise (BadParamCast (p, "s"))
         end
    | "t" ->
         begin
            match dest_param p with
               Number(n) -> make_param (Token (Lm_num.string_of_num n))
             | String(s) -> make_param (Token s)
             | MString(v) -> make_param (MToken v)
             | _ -> raise (BadParamCast (p, "t"))
         end

    | "v" ->
         begin
            match dest_param p with
               Number(n) -> make_param (Var (Lm_symbol.add (Lm_num.string_of_num n)))
             | MString(v) -> make_param (Var v)
             | _ -> raise (BadParamCast (p, "v"))
         end

    | "l" ->
         make_param (MLevel (cast_level p))

    | x -> raise (BadParamCast (p, x))

   (*
    * Constructors.
    *)
   let mk_pair_term loc a b =
      mk_dep0_dep0_term (mk_dep0_dep0_opname loc "pair") a b

   let get_aterm loc at =
      if at.aname != None then
         Stdpp.raise_with_loc loc (Invalid_argument "Syntax Error: Named term where unnamed one is expected")
      else at.aterm

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
            [] -> Stdpp.raise_with_loc loc (Invalid_argument "Term_grammar.make_application: internal error")
          | h::t -> wrap_term loc (aux h t)

   (*
    * Construct a binary term, with a possible dependency.
    *)
   let mk_type_term loc name t1 t2 =
      let t2 = make_term t2 in
      match make_aterm t1 with
         { aname = None; aterm = t } ->
            wrap_term loc (mk_dep0_dep0_term (mk_dep0_dep0_opname loc name) t t2)
       | { aname = Some name'; aterm = t } ->
            wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc name) (dest_var name') t t2)

   let mk_arith_term loc name t1 t2 =
      wrap_term loc (mk_dep0_dep0_term (mk_dep0_dep0_opname loc name) (make_term t1) (make_term t2))

   (*
    * Make record terms
    *)
   let mk_field_term loc r field =
                mk_term (mk_op (mk_opname loc ["field"] [ShapeToken] [0])
                               [make_param (Token field)])  [mk_simple_bterm r]

   let mk_field_self_term =
      let self_term = mk_var_term (Lm_symbol.add "self") in
         fun loc field -> mk_field_term loc self_term field

   (*
    * Check that all are strings.
    *)
   let make_bvar = function
      ST_Term (_, loc) ->
         Stdpp.raise_with_loc loc (ParseError "Not a binding var")
    | ST_String (s, _) ->
         Lm_symbol.add s

   let get_var_contexts loc v terms =
      match mk_var_contexts loc v (List.length terms) with Some conts -> conts | None -> [v]

   (************************************************************************
    * QUOTATIONS                                                           *
    ************************************************************************)

   let pho_grammar_filename =
      try
         Sys.getenv "LANG_FILE"
      with
         Not_found ->
            !Phobos_state.mp_grammar_filename

   let pho_desc_grammar_filename =
      try
         Sys.getenv "DESC_LANG_FILE"
      with
         Not_found ->
            !Phobos_state.mp_desc_grammar_filename

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
      List.map ( fun _ -> 0)

   let string_params =
      List.map ( fun _ -> ShapeString)

   let expr_of_anti (loc, _) name s =
      try Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s) with
         Stdpp.Exc_located ((l1, l2), exn) ->
            let offset = String.length "$" in
            let offset = if name = "" then offset else offset + 1 + (String.length name) in
            let loc = shift_pos loc offset in
               Stdpp.raise_with_loc (adjust_pos loc l1, adjust_pos loc l2) exn

   let q_shift_loc loc nm =
      shift_pos (fst loc) (if nm = "" then String.length "<<" else (String.length "<:") + (String.length nm) + (String.length "<")), (snd loc)

   let rec parse_quotation loc curr (nm, s) =
      if nm = curr then
         Stdpp.raise_with_loc loc (Failure (nm ^ " quotation inside a " ^ curr ^ " quotation"));
      match nm with
         "ext" ->
            Phobos_exn.catch (Phobos_compile.term_of_string [] pho_grammar_filename) s
       | "desc"->
            Phobos_exn.catch (Phobos_compile.term_of_string [] pho_desc_grammar_filename) s
       | "term"
       | "" ->
            (try
                let cs = Stream.of_string s in
                   term_of_parsed_term (Grammar.Entry.parse TermGrammar.term_eoi cs)
             with
                Stdpp.Exc_located ((l1, l2), exn) ->
                   let pos = fst (q_shift_loc loc nm) in
                      Stdpp.raise_with_loc (adjust_pos pos l1, adjust_pos pos l2) exn)
       | "doc" ->
            parse_comment (q_shift_loc loc nm) true SpellOn true s
       | "topdoc" ->
            parse_comment (q_shift_loc loc nm) false SpellOn  true s
       | "dform" ->
            parse_comment (q_shift_loc loc nm) false SpellOff false s
       | _ ->
            Stdpp.raise_with_loc loc (Invalid_argument ("Camlp4 term grammar: unknown " ^ nm ^ " quotation"))

   and parse_comment loc math spell space s =
      let pos = fst loc in
      if !debug_spell && not !dict_inited then
         begin
            Filter_spell.init ();
            dict_inited := true
         end;

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
       | Comment_parse.Term ((opname, (l1, l2)), params, args) ->
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
               mk_opname (adjust_pos pos l1, adjust_pos pos l2) opname (string_params params) (fake_arities args)
            in
            let params = List.map (fun s -> make_param (String s)) params in
            let args = List.map (fun t -> mk_simple_bterm (build_term spelling space t)) args in
            let op = mk_op opname params in
            let t = mk_term op args in
               mk_simple_term comment_term_op [t]
       | Comment_parse.Block items ->
            mk_simple_term comment_block_op [build_term spelling space items]
       | Comment_parse.Quote ((l1, l2), tag, s) ->
            mk_simple_term comment_term_op [parse_quotation (adjust_pos pos l1, adjust_pos pos l2) "doc" (tag, s)]

      (*
       * If spacing is ignored, ignore spaces.
       *)
      and build_inner_term items' spelling space items =
         match items with
            Comment_parse.White :: items when not space ->
               build_inner_term items' spelling space items
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
            Comment_parse.Parse_error (s, (l1, l2)) ->
               Stdpp.raise_with_loc (adjust_pos pos l1, adjust_pos pos l2) (ParseError s)
      in
         mk_simple_term comment_term_op [build_term spell space items]

   let mk_comment_term tl =
      let mk_comment t =
         mk_simple_term comment_term_op [t]
      in
         mk_comment (mk_xlist_term (List.map mk_comment tl))

   let convert_comment loc t =
      if is_string_term comment_string_op t then
         parse_comment (q_shift_loc loc "doc") false SpellOn true (dest_string_term comment_string_op t)
      else
         t

   (************************************************************************
    * GRAMMAR                                                              *
    ************************************************************************)

   EXTEND
      GLOBAL: term_eoi term parsed_term quote_term mterm bmterm singleterm applytermlist parsed_bound_term xdform term_con_eoi;

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
          | [ sl_open_brack; name = word_or_string; sl_close_brack; t = singleterm ->
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
          | [ sl_open_brack; name = word_or_string; sl_close_brack; t = bound_term ->
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

      parsed_term: [[ t = term -> term_of_parsed_term t ]];

      parsed_bound_term:
         [[ t = bound_term -> { t with aterm = term_of_parsed_term t.aterm } ]];

      term:
         [[ x = aterm ->
               get_aterm loc x
          ]|[ s = ANTIQUOT ->
               Phobos_exn.catch (Phobos_compile.term_of_string [] pho_grammar_filename) s
         ]];

      aterm:
         ["comma" LEFTA
          [ x = noncommaterm ->
             make_aterm x
           | x = noncommaterm; sl_comma; y = noncommaterm ->
             { aname = None; aterm = mk_pair_term loc (make_term x) (make_term y) }
         ]];

      noncommaterm:
         [
           "ite" LEFTA
            [ "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
               wrap_term loc (mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname loc "ifthenelse") (make_term e1) (make_term e2) (make_term e3))
            | "let"; x = var; sl_equal; e1 = applyterm; "in"; e2 = SELF ->
               wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc "let") x (make_term e1) (make_term e2))
            | e2 = SELF; "where";  x = var; sl_equal; e1 = SELF ->
               wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc "let") x (make_term e1) (make_term e2))
            | "open";  e1 =  applyterm; "in"; e2 = SELF ->
               wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc "let") (Lm_symbol.add "self") (make_term e1) (make_term e2))
            ]
          (* short form for sequents *)
          | "sequent" NONA
            [ arg = SELF; "{|"; (hyps, concls) = sequent_body; "|}" ->
                  let arg_bt = [mk_simple_bterm (make_term arg)] in
                  let arg = mk_term (mk_op (mk_bopname loc ["sequent_arg"] [] arg_bt) []) arg_bt in
                     wrap_term loc (mk_sequent_term { sequent_args = arg; sequent_hyps = hyps; sequent_goals = concls })
            ]

          (* Logical operators *)
          | "implies" RIGHTA
            [ t1 = SELF; op = sl_implies; t2 = SELF ->
               mk_arith_term loc op t1 t2
            ]
          | "or" RIGHTA
            [ t1 = SELF; op = sl_or; t2 = SELF ->
               mk_arith_term loc op t1 t2
            ]
          | "and" RIGHTA
            [ t1 = SELF; op = sl_and; t2 = SELF ->
               mk_arith_term loc op t1 t2
            ]
          | "quantify" LEFTA
            [ (* all/exists*)
               op = sl_quantify; v = var; sl_colon; t1 = SELF; sl_period; t2 = SELF ->
                  wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc op) v (make_term t1) (make_term t2))
            |(* dall/dexists*)
               op = sl_quantify; v = var; sl_set_in; t1 = SELF; sl_period; t2 = SELF ->
                  wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc  ("d"^op)) v (make_term t1) (make_term t2))
            |(* sall/sexists*)
              op = sl_quantify; v = var; sl_period; t2 = SELF ->
                  wrap_term loc (mk_dep1_term (mk_dep1_opname loc ("s"^op)) v (make_term t2))
            |(* thereis/forall *)
              op = sl_open_quantify; t1 = SELF; sl_period; t2 = SELF ->
                  wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc op) (Lm_symbol.add "self") (make_term t1) (make_term t2))
            ]

          (* Relations *)
          | "equal" NONA
            [ (* t1 = t2 in ty,   t1 <> t2 in ty -- (in)equality in type *)
              t1 = SELF; (op,_) = sl_equal_rel; t2 = NEXT; sl_in; ty = SELF ->
               wrap_term loc (mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname loc op) (make_term ty) (make_term t1) (make_term t2))
            | (* t1 in ty  *)
              t = SELF; op = sl_in; ty = NEXT ->
               (* XXX HACK - this is to support ad-hoc I/O form "member" - see TODO 2.14 -2.15 *)
                  let t =
                     let ty = make_term ty in let t = make_term t in
                     try mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname loc "equal") ty t t
                     with Failure _ | Not_found | Stdpp.Exc_located (_, Not_found) | Stdpp.Exc_located (_, Failure _) ->
                        mk_dep0_dep0_term (mk_dep0_dep0_opname loc "member") t ty
                  in
                     wrap_term loc t
            | (* t1 in t2 subset t3 *)
              t1 = SELF; sl_in; t2 = NEXT; sl_subset; t3 = SELF ->
               wrap_term loc (mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname loc "member") (make_term t1) (make_term t2) (make_term t3))
            | (* t1 subset t2 *)
              t1 = SELF; op = sl_subset; t2 = SELF ->
               mk_arith_term loc op t1 t2
            | (* t1 = t2, t1 <> t2  *)
              t1 = SELF; (op,_) = sl_equal_rel; t2 = NEXT ->
                mk_arith_term loc op t1 t2
              (* t1 =[g] t2, t1 <>[g] t2  - algebraic relations for g *)
            | t1 = SELF; (_,op) = sl_equal_rel; sl_open_brack; g = aterm; sl_close_brack; t2 = SELF ->
               make_application loc [mk_field_term loc (get_aterm loc g) op; make_term t1; make_term t2]
            | (* t1 ~ t2 - squiggle equality  *)
              t1 = SELF; sl_tilde; t2 = SELF ->
               (* XXX: HACK - Perv!rewrite should be eventially replaced by mk_opname loc ["sqeq"] *)
               (* wrap_term loc (mk_dep0_dep0_term (mk_dep0_dep0_opname loc "sqeq") (make_term t1) (make_term t2)) *)
                  wrap_term loc (mk_xrewrite_term (make_term t1) (make_term t2))
            | (* t1 In t2  - membership for set theory *)
              t1 = SELF; op = sl_set_in; t2 = SELF ->
               wrap_term loc (mk_dep0_dep0_term (mk_dep0_dep0_opname loc op) (make_term t1) (make_term t2))
            ]
          | "compare" NONA
            [ (* t1 =@ t2, t1 <>@ t2, t1 <@ t2, t1 >@ t2, ...  - integer relations as booleans *)
               t1 = SELF; op = sl_arith_rel; t2 = SELF ->
               mk_arith_term loc op t1 t2
            |(* t1 < t2, t1 > t2, ...  - integer relations as propositions *)
               t1 = SELF; (op,_) = sl_rel;  t2 = SELF ->
               mk_arith_term loc op t1 t2
            |(* t1 ^= t2, t1 ^<> t2, t1 ^< t2, t1 ^> t2, ...  - algebraic relations for self *)
               t1 = SELF; op = sl_label_self_rel;  t2 = SELF ->
               make_application loc [mk_field_self_term loc op; make_term t1; make_term t2]
            |(* t1 <[g] t2, t1 >[g] t2, ...  - algebraic relations for g *)
               t1 = SELF; (_,op) = sl_rel; sl_open_brack; g = aterm; sl_close_brack; t2 = SELF ->
               make_application loc [mk_field_term loc (get_aterm loc g) op; make_term t1; make_term t2]
            ]

          (* Other operations *)
          | "cons" RIGHTA
            [ t1 = SELF; op = sl_double_colon; t2 = SELF ->
               mk_arith_term loc op t1 t2
            ]
          | "fun" RIGHTA
           [ t1 = SELF; op = sl_arrow; t2 = SELF ->
              mk_type_term loc op t1 t2
           ]
          | "rev_fun" LEFTA
            [ t1 = SELF; op = sl_left_arrow; t2 = SELF ->
               mk_arith_term loc op t2 t1
            ]
          | "isect" RIGHTA
            [ (* Isect x: A. B[x], Union x:A. B[x]  - intersection, union of family of types *)
               op = sl_Isect; v = var; sl_colon; t1 = SELF; sl_period; t2 = SELF ->
               wrap_term loc (mk_dep0_dep1_term (mk_dep0_dep1_opname loc op) v (make_term t1) (make_term t2))
             | (* A union B, A isect B, x: A isect B[x]  - binary union, intersection and dependent intersection *)
               t1 = SELF; op = sl_isect; t2 = SELF ->
               mk_type_term loc op t1 t2
             |(* quot x,y: t1 // t2  - quotient type *)
               op = sl_quotient; x = var; sl_comma; y = var; sl_colon; t1 = SELF; sl_double_slash; t2 = SELF ->
               wrap_term loc (mk_dep0_dep2_term (mk_dep0_dep2_opname loc op) x y (make_term t1) (make_term t2))
            ]
          | "plus" RIGHTA
            [  (* t1 +[g] t2  - algebraic plus *)
               t1 = SELF; sl_plus; sl_open_brack; g = aterm;  sl_close_brack; t2 = SELF ->
               make_application loc [mk_field_term loc (get_aterm loc g) "+"; make_term t1; make_term t2]
             | (* t1 + t2 - disjoint union *)
               t1 = SELF; op = sl_plus; t2 = SELF ->
               mk_arith_term loc op t1 t2
             | (* t1 ^+ t2   - algebraic plus for self *)
               t1 = SELF; op = sl_label_self_plus;  t2 = SELF ->
               make_application loc [mk_field_self_term loc op; make_term t1; make_term t2]
            ]
          | "add" LEFTA
            [ (* t1 +@ t2, t1 -@ t2 - integer plus, minus *)
               t1 = SELF; op = sl_arith_add; t2 = SELF ->
               mk_arith_term loc op t1 t2
            |(* t1 ^- t2   - algebraic minus for self *)
               t1 = SELF; op = sl_label_self_minus;  t2 = SELF ->
               make_application loc [mk_field_self_term loc op; make_term t1; make_term t2]
            |(* t1 -[g] t2   - algebraic minus for g *)
               t1 = SELF; op = sl_add; sl_open_brack; g = aterm; sl_close_brack; t2 = SELF ->
               make_application loc [mk_field_term loc (get_aterm loc g) op; make_term t1; make_term t2 ]
            ]
          | "uminus"
            [ op = sl_minus; x = SELF ->
               wrap_term loc (mk_dep0_term (mk_dep0_opname loc op) (make_term x))
            ]
          | "prod" RIGHTA
            [  (* t1 *[g] t2  - algebraic multiplication (e.g. group operation) *)
               t1 = SELF; sl_star; sl_open_brack; g = aterm;  sl_close_brack; t2 = SELF ->
               make_application loc [mk_field_term loc (get_aterm loc g) "*"; make_term t1; make_term t2]
             | (* t1 * t2 - type product *)
               t1 = SELF; op = sl_star; t2 = SELF ->
               mk_type_term loc op t1 t2
             |(* t1 ^* t2   - algebraic multiplication for self *)
               t1 = SELF; op = sl_label_self_star;  t2 = SELF ->
               make_application loc [mk_field_self_term loc op; make_term t1; make_term t2]
            ]
          | "mul" LEFTA
            [ (* t1 *@ t2, t1 /@ t2, t1 %@ t2  - integer multiplication, division, reminder *)
               t1 = SELF; op = sl_arith_mul; t2 = SELF ->
               mk_arith_term loc op t1 t2
            |(* t1 ^/ t2   - algebraic division for self *)
               t1 = SELF; op = sl_label_self_div;  t2 = SELF ->
               make_application loc [mk_field_self_term loc op; make_term t1; make_term t2]
            |(* t1 /[g] t2   - algebraic right division for g *)
               t1 = SELF; op = sl_div; sl_open_brack; g = aterm; sl_close_brack; t2 = SELF ->
               make_application loc [mk_field_term loc (get_aterm loc g) op; make_term t1; make_term t2]
            ]
          | "apply" LEFTA
            [ t = applyterm ->
               t
             | t = applyterm; l = applytermlist ->
               make_application loc (make_term t :: l)
            ]
          | "type" NONA
            [ t = SELF; op = sl_type ->
               wrap_term loc (mk_dep0_term (mk_dep0_opname loc op) (make_term t))
            ]
         ];

      (* Term that can be used in application lists *)
      applyterm:
         [ [ op = opname ->
              begin match op with
                 [name] ->
                    ST_String(name, loc)
               | _ ->
                    wrap_term loc (mk_term (mk_op (mk_opname loc op [] []) []) [])
              end
            | op = opname; (params, bterms) = termsuffix ->
              wrap_term loc (mk_term (mk_op (mk_bopname loc op params bterms) params) bterms)
            | op = opname; sl_colon; t = applyterm ->
              match op with
                 [name] ->
                    if !debug_grammar then
                       eprintf "Got bound term: %s%t" name eflush;
                    ST_Term({ aname = Some (mk_var_term (Lm_symbol.add name)); aterm = make_term t }, loc)
               | _ ->
                    Stdpp.raise_with_loc loc (ParseError "illegal binding variable")
           ]
          | "power" RIGHTA
            [ (* t1 ^@ t2  - integer power *)
               t1 = applyterm; op = sl_arith_power; t2 = applyterm ->
               mk_arith_term loc op t1 t2
            |(* t1 ^^ t2   - algebraic power for self *)
               t1 = applyterm; op = sl_label_self_power;  t2 = applyterm ->
               make_application loc [mk_field_self_term loc op; (make_term t1); (make_term t2)]
            |(* t1 ^[g] t2   - algebraic power for g *)
               t1 = applyterm; op = sl_power; sl_open_brack; g = aterm; sl_close_brack; t2 = applyterm ->
               make_application loc [mk_field_term loc (get_aterm loc g) op; (make_term t1); (make_term t2)]
            |(* r ^ lab - field selection for records *)
              r = applyterm; sl_power; lab = word_or_string  ->
               wrap_term loc (mk_field_term loc (make_term r) lab)
            |(* r ^ lab := t - field update for records *)
               r = applyterm; sl_power; lab = word_or_string; sl_assign; t = noncommaterm  ->
               wrap_term loc (**)
                  (mk_term (mk_op (mk_opname loc ["rcrd"] [ShapeToken] [0;0])
                           [make_param (Token lab )])  [mk_simple_bterm (make_term t); mk_simple_bterm (make_term r)])
            |(* ^ lab  - field selection for self *)
              sl_power; lab = word_or_string  ->
               wrap_term loc (mk_field_self_term loc lab)
            |(* ^ lab - field update for records *)
               sl_power; lab = word_or_string; sl_assign; t = noncommaterm   ->
               wrap_term loc (**)
                  (mk_term (mk_op (mk_opname loc ["rcrd"] [ShapeToken] [0;0]) [make_param (Token lab )]) (**)
                           [mk_simple_bterm (make_term t); mk_simple_bterm (mk_var_term (Lm_symbol.add "self"))])
           ]
          | [ t = nonwordterm ->
               wrap_term loc t
            ]
         ];

      (* Singleterm is a distinct term and no colons *)
      singleterm:
         [ [ op = opname ->
              { aname = None; aterm = mk_term (mk_op (mk_opname loc op [] []) []) [] }
            | op = opname; (params, bterms) = termsuffix ->
              { aname = None; aterm = mk_term (mk_op (mk_bopname loc op params bterms) params) bterms }
           ]
          | [ t = nonwordterm ->
               { aname = None; aterm = t }
            ]
          | [ s = ANTIQUOT ->
               { aname = None; aterm = Phobos_exn.catch (Phobos_compile.term_of_string [] pho_grammar_filename) s}
            ]
         ];

      bound_term:
         [ [ sl_open_paren; v = varterm; sl_colon; t = aterm; sl_close_paren ->
               { aname = Some v; aterm = (get_aterm loc t) }
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

      rcrdterm:
         [[ ";"; lab = word_or_string; sl_equal; t = aterm  ->
               (lab,t)
         ]];

      recordterm:
         [[ lab = word_or_string; sl_colon; t = aterm  ->
             (Some lab,t)
           ]
         |[ t = aterm  ->
             (None,t)
           ]];

      nonwordterm:
         [[ (* vars *)
             v = varterm ->
             v

             (* Abbreviations *)
           | i = sl_number ->
             mk_term (mk_op (mk_opname loc ["number"] [ShapeNumber] [])
                               [make_param (Number i)]) []
           | x = sequent ->
             x

             (* Parenthesized terms *)
           | sl_open_paren; t = aterm; sl_close_paren ->
             get_aterm loc t
             (* records {x1=a1;x2=a2;...} *)
           | sl_open_curly; lab = word_or_string; sl_equal; t = aterm; rest = LIST0 rcrdterm; sl_close_curly ->
                let r0 =   mk_term (mk_op (mk_opname loc ["rcrd"] [] []) []) [] in
                let aux = fun r -> function (lab,t) ->
                           mk_term (mk_op (mk_opname loc ["rcrd"] [ShapeToken] [0;0])
                               [make_param (Token lab )])  [mk_simple_bterm (get_aterm loc t); mk_simple_bterm  r]
                in
                   List.fold_left aux r0 ((lab,t)::rest)
             (* record typess {x1:A1;x2:a2;...} *)
           | sl_open_curly; lab = word_or_string; sl_colon; t = aterm; ";"; r = LIST0 recordterm SEP ";"; sl_close_curly ->
                let r0 =   mk_term (mk_op (mk_opname loc ["record"] [ShapeToken] [0])
                               [make_param (Token lab )])  [mk_simple_bterm (get_aterm loc t)] in
                let aux = fun r -> function
                      (Some lab,t) ->
                           mk_term (mk_op (mk_opname loc ["record"] [ShapeToken] [1;0])
                               [make_param (Token lab )])  [mk_bterm [Lm_symbol.add "self"] (get_aterm loc t); mk_simple_bterm  r]
                   |  (None,t) ->
                           mk_dep0_dep1_term (mk_dep0_dep1_opname loc "set") (Lm_symbol.add "self") r (get_aterm loc t)
                in
                    List.fold_left aux r0 r
             (* single record types {x1:A1} *)
           | sl_open_curly; lab = word_or_string; sl_colon; t = aterm; sl_close_curly ->
                mk_term (mk_op (mk_opname loc ["record"] [ShapeToken] [0])
                        [make_param (Token lab )])  [mk_simple_bterm (get_aterm loc t)]
             (* sets {x:A | P[x]} *)
           | sl_open_curly; v = word_or_string; sl_colon; ty = aterm; sl_pipe; b = aterm; sl_close_curly ->
             mk_dep0_dep1_term (mk_dep0_dep1_opname loc "set") (Lm_symbol.add v) (get_aterm loc ty) (get_aterm loc b)
             (* very dependent functions {f | x:A -> B[x]} *)
           | sl_open_curly; f =  word_or_string; sl_pipe; t = aterm; sl_close_curly ->
             let t = get_aterm loc t in let f = Lm_symbol.add f in
             let rfun_op = mk_dep0_dep2_opname loc "rfun" in
                if is_dep0_dep1_term (mk_dep0_dep1_opname loc "fun") t then
                   let v, a, b = dest_dep0_dep1_any_term t in
                      mk_dep0_dep2_term rfun_op f v a b
                else if is_dep0_dep0_term (mk_dep0_dep0_opname loc "fun") t then
                   let a, b = two_subterms t in
                      mk_dep0_dep2_term rfun_op f (mk_gensym ()) a b
                else
                   Stdpp.raise_with_loc loc (ParseError "body of <rfun> is not a function")
            | sl_exclamation; v = var ->
               encode_free_var v
            | x = QUOTATION ->
               parse_quotation loc "term" (dest_quot x)
          ]];

      var: [[ v = word_or_string -> Lm_symbol.add v ]];

      varterm:
         [[ sl_single_quote; v = var ->
             begin match mk_var_contexts loc v 0 with Some conts -> mk_so_var_term v conts [] | None -> mk_var_term v end
           | sl_single_quote; v = var; sl_contexts_left; conts = LIST0 var SEP ";"; sl_contexts_right ->
               mk_so_var_term v conts []
           | sl_single_quote; v = var; sl_contexts_left; conts = LIST0 var SEP ";"; sl_contexts_right; sl_open_brack; terms = opttermlist; sl_close_brack ->
               mk_so_var_term v conts terms
           | sl_single_quote; v = var; sl_contexts_empty ->
               mk_so_var_term v [] []
           | sl_single_quote; v = var; sl_contexts_empty; sl_open_brack; terms = opttermlist; sl_close_brack ->
               mk_so_var_term v [] terms
           | sl_single_quote; v = var; sl_open_brack; terms = opttermlist; sl_close_brack ->
               mk_so_var_term v (get_var_contexts loc v terms) terms
          ]];

      quote_term:
         [[ v = word_or_string; params = optparams; bterms = optbterms ->
             v, params, bterms
          ]];

      (* Application lists *)
      applytermlist:
         [[ x = applyterm ->
             [make_term x]
           | l = applytermlist; x = applyterm ->
             l @ [make_term x]
          ]];

      (* List of terms *)
      opttermlist:
         [[ l = OPT termlist ->
             match l with
                Some l' -> l'
              | None -> []
          ]];

      termlist:
         [[ l = LIST1 term SEP ";" -> l ]];

      (* Parameters and bterm lists *)
      opname:
         [[ op = LIST1 word_or_string SEP sl_exclamation ->
             op
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
         [[  w = sl_word ->
             make_param (MString (Lm_symbol.add w))
           | w = STRING ->
             make_param (String (Token.eval_string loc w))
           | n = sl_number ->
             make_param (Number n)
           | "-"; n = sl_number ->
             make_param (Number (Lm_num.mult_num n (Lm_num.num_of_int (-1))))
           | sl_atsign ->
             make_param (Quote)
          ]
          | [ p = param; sl_colon; w = sl_word ->
               cast_param p w
             | p = param; sl_single_quote ->
               make_param (MLevel (incr_level_exp (cast_level p)))
             | p1 = param; sl_pipe; p2 = param ->
               make_param (MLevel (max_level_exp (cast_level p1) (cast_level p2) 0))
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
         [[ bterms = LIST0 bterm SEP sl_semi_colon ->
                List.map mk_bterm' bterms
          ]];

      bterm:
         [[ h = bhead ->
             [], tupelize loc h
           | h = bhead; sl_period; t = term ->
             List.map make_bvar h, t
           | sl_period; t = term ->
             [], t
          ]];

      bhead:
         [[ l = LIST1 noncommaterm SEP sl_comma ->
             l
          ]];

      (* Special forms *)
      sequent:
         [[ sl_sequent; sl_open_curly; (hyps, concls) = sequent_body; sl_close_curly ->
               mk_sequent_term {
                  sequent_args = mk_term (mk_op (mk_opname loc ["sequent_arg"] [] []) []) [];
                  sequent_hyps = hyps;
                  sequent_goals = concls;
               }
          | sl_sequent; sl_open_brack; args = termlist; sl_close_brack;
            sl_open_curly; (hyps, concls) = sequent_body; sl_close_curly ->
               let args_bt = List.map mk_simple_bterm args in
               mk_sequent_term {
                  sequent_args = mk_term (mk_op (mk_bopname loc ["sequent_arg"] [] args_bt) []) args_bt;
                  sequent_hyps = hyps;
                  sequent_goals = concls
               }
          | sl_sequent; sl_open_paren; arg = term; sl_close_paren;
               sl_open_curly; (hyps, concls) = sequent_body; sl_close_curly ->
               mk_sequent_term { sequent_args = arg; sequent_hyps = hyps; sequent_goals = concls }
          ]];

      sequent_body:
         [[ hyps = LIST0 hyp SEP ";"; sl_turnstile; concls = LIST0 term SEP ";" ->
               (SeqHyp.of_list hyps, SeqGoal.of_list concls)
          ]];

      hyp:
         [[ "<"; name = var; conts = OPT contslist; args=optbrtermlist; ">" ->
             let conts = match conts with Some conts -> conts | None -> get_var_contexts loc name args in
             Context(name, conts, args)
          | v = LIDENT; ":"; t = aterm ->
             Hypothesis(Lm_symbol.add v, get_aterm loc t)
          | v = UIDENT; ":"; t = aterm ->
             Hypothesis(Lm_symbol.add v, get_aterm loc t)
          | v = STRING; ":"; t = aterm ->
             Hypothesis(Lm_symbol.add v, get_aterm loc t)
          | t = aterm ->
             Hypothesis(empty_var, get_aterm loc t)
          ]];

      contslist: [[ sl_contexts_left; vl = LIST0 var SEP ";" ; sl_contexts_right -> vl ]];

      optbrtermlist:
         [[ tl = OPT brtermlist ->
             match tl with
                Some l -> l
              | None -> []
          ]];

      brtermlist:
         [[ sl_open_brack; l = termlist; sl_close_brack -> l ]];

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
             term_of_parsed_term (get_aterm loc t)
           | sl_back_quote; name = STRING ->
             mk_xstring_term (Token.eval_string loc name)
          ]];

      (* Term constructor "programs" *)
      term_con_eoi:
         [[ con = con_term; EOI -> con ]];

      con_termlist:
         [[ l = LIST1 con_term SEP ";" -> l ]];

      con_term:
         [[ sl_exclamation; t = term; sl_exclamation -> ConTerm t
          | e = ANTIQUOT -> ConExpr (expr_of_anti loc "" e)
          | sl_single_quote; v = ANTIQUOT -> ConVar (expr_of_anti loc "" v)
          | op = opname -> ConConstruct (mk_opname loc op [] [], [], [])
          | op = opname; (params, bterms) = con_term_suffix ->
               let param_types = List.map snd params in
               let bterm_arities = List.map (fun (bvars, _) -> List.length bvars) bterms in
                  ConConstruct (mk_opname loc op param_types bterm_arities, params, bterms)
          | con = con_sequent -> con
         ]];

      con_term_suffix:
         [[ p = con_params ->
             p, []
           | p = con_params; sl_open_curly; bterms = con_btermslist; sl_close_curly ->
             p, bterms
           | sl_open_curly; bterms = con_btermslist; sl_close_curly ->
             [], bterms
          ]];

      con_params:
         [[ sl_open_brack; params = LIST0 con_param SEP ","; sl_close_brack ->
             params
          ]];

      con_param:
         [[ s = STRING    -> ConPStr s, ShapeString
          | s = sl_word   -> ConPMeta (Lm_symbol.add s), ShapeString
          | n = sl_number -> ConPNum n, ShapeNumber
          | e = ANTIQUOT "int" -> ConPInt (expr_of_anti loc "int" e), ShapeNumber
          | s = STRING  ; sl_colon; shape = con_param_shape -> ConPStr s, shape
          | s = sl_word ; sl_colon; shape = con_param_shape -> ConPMeta (Lm_symbol.add s), shape
          | e = ANTIQUOT; sl_colon; shape = con_param_shape -> ConPExpr (expr_of_anti loc "" e), shape
          ]];

      con_param_shape:
         [[ s = LIDENT ->
               match s with
                  "n" -> ShapeNumber
                | "s" -> ShapeString
                | "l" -> ShapeLevel
                | "t" -> ShapeToken
                | "v" -> ShapeVar
                |  s  -> raise (BadParamCast (make_param (String ""), s))
          ]];

      con_btermslist:
         [[ l = LIST0 con_bterm SEP ";" -> l ]];

      con_bterm:
         [[ v = LIDENT; b = con_bterm_suffix ->
               let bv, bt = b in (ConBVarConst v :: bv), bt
          | e = ANTIQUOT; suff = OPT con_bterm_suffix ->
               let e = expr_of_anti loc "" e in
               match suff with
                  Some (bv, bt) ->
                     (ConBVarExpr e :: bv), bt
                | None ->
                     [], ConExpr e
          ] | [ t = con_term -> [], t
          ]];

      con_bterm_suffix:
         [[ ","; bt = con_bterm -> bt
          | "."; t = con_term -> [], t
          ]];

      con_hyps: [[ hyps = LIST0 con_hyp SEP ";"; sl_turnstile -> hyps ]];

      (* Special forms *)
      con_sequent:
         [[ sl_sequent; sl_open_curly; hyps = con_hyps;
            concl = LIST0 con_term SEP ";"; sl_close_curly ->
               let arg = ConTerm (mk_term (mk_op (mk_opname loc ["sequent_arg"] [] []) []) []) in
                  ConSequent (arg, hyps, concl)
          | sl_sequent; sl_open_brack; args = con_termlist; sl_close_brack; sl_open_curly; hyps = con_hyps;
            concl = LIST0 con_term SEP ";"; sl_close_curly ->
               let bterm_arities = List.map (fun _ -> 0) args in
               let op = mk_opname loc ["sequent_arg"] [] bterm_arities in
               let bterms = List.map (fun t -> [], t) args in
               let arg = ConConstruct (op, [], bterms) in
                  ConSequent (arg, hyps, concl)
          | sl_sequent; sl_open_paren; arg = con_term; sl_close_paren; sl_open_curly; hyps = con_hyps;
            concl = LIST0 con_term SEP ";"; sl_close_curly ->
                  ConSequent (arg, hyps, concl)
          ]];

      con_hyp:
         [[ "<"; name = word_or_string; args = con_optbrtermlist; ">" ->
              ConContext (<:expr< $str:name$ >>, args)
          | expr = ANTIQUOT "list" ->
              ConHypList (expr_of_anti loc "list" expr)
          | v = word_or_string; rest = con_hyp_suffix ->
              rest v
          | v = ANTIQUOT; sl_colon; t = con_term ->
              ConHypothesis (expr_of_anti loc "" v, t)
          | t = con_term ->
              ConHypothesis (<:expr< Lm_symbol.empty_var >>, t)
          ]];

      con_hyp_suffix:
         [[ sl_colon; t = con_term ->
               fun v -> ConHypothesis (<:expr< Lm_symbol.add $str:v$ >>, t)
          | (params, bterms) = con_term_suffix ->
               (fun op ->
                  let param_types = List.map snd params in
                  let bterm_arities = List.map (fun (bvars, _) -> List.length bvars) bterms in
                     ConHypothesis (<:expr< Lm_symbol.empty_var >>, ConConstruct (mk_opname loc [op] param_types bterm_arities, params, bterms)))
          | ->
               fun op -> ConHypothesis (<:expr< Lm_symbol.empty_var >>, ConTerm (mk_term (mk_op (mk_opname loc [op] [] []) []) []))
          ]];

      con_optbrtermlist:
         [[ tl = OPT con_brtermlist ->
             match tl with
                Some l -> l
              | None -> []
          ]];

      con_brtermlist:
         [[ sl_open_brack; l = con_termlist; sl_close_brack -> l ]];

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
         [[ "in" -> ()
          | "IN" -> () ]];

      sl_set_in:
         [[ "In" -> "mem"
         ]];

      sl_colon:
         [[ ":" -> () ]];

      sl_double_colon:
         [[ "::" -> "cons" ]];

      sl_semi_colon:
         [[ ";" -> () ]];

      sl_double_slash:
         [[ "//" -> () ]];

      sl_comma:
         [[ "," -> () ]];

      sl_period:
         [[ "." -> () ]];

      sl_pipe:
         [[ "|" -> () ]];

      sl_single_quote:
         [[ "'" -> () ]];

      sl_back_quote:
         [[ "`" -> () ]];

      sl_sequent:
         [[ "sequent" -> () ]];

      sl_turnstile:
         [[ ">-" -> () ]];

      sl_exclamation:
         [[ "!" -> () ]];

      sl_assign:
         [[ ":=" -> "assign" ]];

      sl_equal:
         [[ "=" -> () ]];

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
         [[ "+" -> "union" ]];

      sl_add: (* other operations with addition prioruty *)
         [[ "-" -> "-" ]];

      sl_minus: (* unary minus *)
         [[ "-" -> "minus" ]];

      sl_arith_add:
         [[ "+@" -> "add"
          | "-@" -> "sub" ]];

      sl_label_self_plus:
         [[ "^+" -> "+"]];

      sl_label_self_minus:
         [[ "^-" -> "-"]];

      sl_star:
         [[ "*" -> "prod" ]];

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

      sl_atsign:
         [[ "@" -> ()]];

      sl_and:
         [[ "and" -> "and"
           | "&" -> "and"
          ]];

      sl_implies:
         [[ "=>" -> "implies" ]];

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

      sl_tilde:
         [[ "~" -> () ]];

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
      word_or_string:
         [[ name = UIDENT ->
             name
           | name = LIDENT ->
             name
           | name = STRING ->
             Token.eval_string loc name
          ]];

      sl_word:
         [[ s = LIDENT -> s
           | s = UIDENT -> s
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
