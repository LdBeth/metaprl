(*
 * Define a basic parser and lexer.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_debug
open Lm_symbol
open Lm_printf
open Lm_lexer
open Lm_parser
open Lm_string_set

open Opname
open Term_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermShape
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError

open Tactic_type

open Term_hash_code

type quotation_expander = string -> string -> term

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Filter_grammar%t"

let debug_grammar =
   create_debug (**)
      { debug_name = "grammar";
        debug_description = "Debug the grammar";
        debug_value = false
      }

(************************************************************************
 * Terms.
 *)

(*
 * Terms used by the grammar.
 *)
let lexer_opname     = mk_opname "Lexer" nil_opname
let lexer_arg_opname = mk_opname "lexer_arg" lexer_opname

let parser_opname     = mk_opname "Parser" nil_opname
let parser_arg_opname = mk_opname "parser_arg" parser_opname

(************************************************************************
 * Lexer and parser modules.
 *)

(* %%MAGICBEGIN%% *)

(*
 * Positions and actions.
 *)
type id = Lm_symbol.symbol
type lexer_id = opname

module Action =
struct
   type action = id

   let pp_print_action = pp_print_symbol

   let choose = max

   let hash = Hashtbl.hash
   let compare = Lm_symbol.compare
end;;

module ActionTable = SymbolTable;;

module Lexer = MakeLexer (Lm_channel.LexerInput) (Action);;

(*
 * Boolean lexer is for parsing comments and quotations.
 *)
module BoolCompare =
struct
   type t = bool

   let compare b1 b2 =
      if b1 then
         if b2 then
            0
         else
            1
      else if b2 then
         -1
      else
         0
end

module BoolSet = Lm_set.LmMake (BoolCompare);;

module BoolAction =
struct
   type action = bool

   let pp_print_action = pp_print_bool

   let hash b =
      if b then
         0x2cf1124
      else
         0x5ffa1124

   let compare a b =
      if a = b then
         0
      else if a then
         1
      else
         -1

   let choose = max
end;;

module BoolLexer = MakeLexer (Lm_channel.LexerInput) (BoolAction);;

(*
 * CFG variables are symbols.
 *)
module ParserArg =
struct
   type symbol = shape

   let eof_term = mk_simple_term (mk_opname "<eof>" nil_opname) []
   let eof = shape_of_term eof_term

   let to_string = short_string_of_shape

   let pp_print_symbol buf op =
      pp_print_string buf (to_string op)

   let hash_symbol = Hashtbl.hash
   let compare_symbol = shape_compare

   type action = Action.action

   let hash_action = Action.hash
   let compare_action = Action.compare

   let pp_print_action = Action.pp_print_action
end;;

(*
 * The actual parser.
 *)
module Parser = Lm_parser.MakeParser (ParserArg) (ParserPrecedence);;

(************************************************************************
 * Grammar.
 *)

(*
 * A lexer action contains the rewrite,
 * and the name of the lexeme.
 *)
type lexer_clause =
   { lexer_regex      : string;
     lexer_redex      : term;
     lexer_contractum : term option
   }

type lexer_action =
   LexerRewrite of rewrite_rule
 | LexerPair of BoolLexer.t * lexer_action
 | LexerNone

(*
 * A parser action contains a term list for the variables
 * on the right-hand-size of the production, and a rewrite.
 *)
type parser_action = rewrite_rule

type parser_clause =
   { parser_prec       : shape option;
     parser_redex      : term;
     parser_contractum : term
   }

(*
 * An input form.
 *)
type iform =
   { iform_redex : term;
     iform_contractum : term
   }

(*
 * The info includes a lexer and a parser.
 *
 * The name is mainly used to detect if the grammar has
 * changed.  Whenever the grammar is modified, the name reverts
 * to None.  A grammar is given a name just before
 * marshaling.  If time comes to marshal it again, and it still
 * has a name, then it is unchanged, and there is no need to
 * marshal again.
 *
 * There can be multiple lexers.  Each lexer is associated with a start
 * symbol.
 *)
type gram =
   { gram_magic               : int;
     mutable gram_name        : string option;
     mutable gram_subnames    : StringSet.t;
     gram_lexers              : Lexer.t OpnameTable.t;
     gram_lexer_actions       : lexer_action ActionTable.t;
     gram_lexer_clauses       : lexer_clause ActionTable.t;
     gram_lexer_start         : opname ShapeTable.t;
     gram_parser              : Parser.t;
     gram_parser_actions      : parser_action ActionTable.t;
     gram_parser_clauses      : parser_clause ActionTable.t;
     gram_iforms              : iform ActionTable.t;
     mutable gram_iform_table : TacticTypes.conv Term_match_table.term_table option
   }

(*
 * We compute the actual grammars lazily.
 * First, we just build a collection of grammars.
 * Then, when the actual grammar is needed, we take
 * the union of all the roots.
 *
 * This is imperative so that we remember the flattening.
 *)
type info =
   GramCollection of gram StringTable.t
 | GramInfo of gram

type t =
   { mutable gram_info : info }
(* %%MAGICEND%% *)

let gram_magic = 0x18b24ef1

let empty_name = ".empty"

let empty =
   { gram_magic          = gram_magic;
     gram_name           = Some empty_name;
     gram_subnames       = StringSet.singleton empty_name;
     gram_lexers         = OpnameTable.empty;
     gram_lexer_actions  = ActionTable.empty;
     gram_lexer_clauses  = ActionTable.empty;
     gram_lexer_start    = ShapeTable.empty;
     gram_parser         = Parser.empty;
     gram_parser_actions = ActionTable.empty;
     gram_parser_clauses = ActionTable.empty;
     gram_iforms         = ActionTable.empty;
     gram_iform_table    = None
   }

let is_empty gram =
   let { gram_lexer_actions  = lexer_actions;
         gram_parser_actions = parser_actions;
         gram_iforms         = iforms
       } = gram
   in
      ActionTable.is_empty lexer_actions
      && ActionTable.is_empty parser_actions
      && ActionTable.is_empty iforms

(*
 * Hash code for the grammar.
 * The order of items is undefined, so make sure the
 * hash is associative and commutative.
 *)
let hash_int code i =
   code lxor i

let hash_item code item =
   hash_int code (Hashtbl.hash_param max_int max_int item)

let hash_grammar gram =
   let { gram_lexer_clauses  = lexer_clauses;
         gram_parser_clauses = parser_clauses;
         gram_iforms         = iforms
       } = gram
   in
   let code = 0x3192c372 in
   let code =
      ActionTable.fold (fun code v clause ->
            let { lexer_regex = regex;
                  lexer_redex = redex;
                  lexer_contractum = contractum
                } = clause
            in
            let code = hash_item code v in
            let code = hash_item code regex in
            let code = hash_int code (hash_term redex) in
               match contractum with
                  Some contractum ->
                     hash_int code (hash_term contractum)
                | None ->
                     code) code lexer_clauses
   in
   let code =
      ActionTable.fold (fun code v clause ->
            let { parser_prec = pre;
                  parser_redex = redex;
                  parser_contractum = contractum
                } = clause
            in
            let code = hash_item code v in
            let code = hash_item code pre in
            let code = hash_int code (hash_term redex) in
               hash_int code (hash_term contractum)) code parser_clauses
   in
   let code =
      ActionTable.fold (fun code _ iform ->
            let code = hash_int code (hash_term iform.iform_redex) in
               hash_int code (hash_term iform.iform_contractum)) code iforms
   in
      code

(*
 * Printing.
 *)
let pp_print_string_opt buf s =
   let s =
      match s with
         Some s ->
            s
       | None ->
            "<none>"
   in
      pp_print_string buf s

let pp_print_strings buf names =
   StringSet.iter (fun name ->
         fprintf buf "@ %s" name) names

(*
 * Take the union of two grammars.
 *)
let union gram1 gram2 =
   if !debug_grammar then
      eprintf "Grammar union: %a, %a@." pp_print_string_opt gram1.gram_name pp_print_string_opt gram2.gram_name;
   let { gram_subnames       = subnames1;
         gram_lexers         = lexers1;
         gram_lexer_actions  = lexer_actions1;
         gram_lexer_clauses  = lexer_clauses1;
         gram_lexer_start    = starts1;
         gram_parser         = parser1;
         gram_parser_actions = parser_actions1;
         gram_parser_clauses = parser_clauses1;
         gram_iforms         = iforms1
       } = gram1
   in
   let { gram_subnames       = subnames2;
         gram_lexers         = lexers2;
         gram_lexer_actions  = lexer_actions2;
         gram_lexer_clauses  = lexer_clauses2;
         gram_lexer_start    = starts2;
         gram_parser         = parser2;
         gram_parser_actions = parser_actions2;
         gram_parser_clauses = parser_clauses2;
         gram_iforms         = iforms2
       } = gram2
   in
   let lexers =
      OpnameTable.fold (fun lexers name lexer2 ->
            let lexer =
               try
                  let lexer1 = OpnameTable.find lexers name in
                     Lexer.union lexer1 lexer2
               with
                  Not_found ->
                     lexer2
            in
               OpnameTable.add lexers name lexer) lexers1 lexers2
   in
   let starts = ShapeTable.fold ShapeTable.add starts1 starts2 in
   let subnames = StringSet.union subnames1 subnames2 in
      if !debug_grammar then
         eprintf "@[<v 3>Grammar union: active: %a, %a; subnames:%a@]@." (**)
            pp_print_string_opt gram1.gram_name
            pp_print_string_opt gram2.gram_name
            pp_print_strings subnames;
      { gram_magic          = gram_magic;
        gram_name           = None;
        gram_lexers         = lexers;
        gram_subnames       = subnames;
        gram_parser         = Parser.union parser1 parser2;
        gram_lexer_actions  = ActionTable.fold ActionTable.add lexer_actions1 lexer_actions2;
        gram_parser_actions = ActionTable.fold ActionTable.add parser_actions1 parser_actions2;
        gram_lexer_clauses  = ActionTable.fold ActionTable.add lexer_clauses1 lexer_clauses2;
        gram_parser_clauses = ActionTable.fold ActionTable.add parser_clauses1 parser_clauses2;
        gram_lexer_start    = starts;
        gram_iforms         = ActionTable.fold ActionTable.add iforms1 iforms2;
        gram_iform_table    = None
      }

(*
 * Redex patterns and values for the lexer.
 *)
let mk_mstring_term opname s =
   mk_term (mk_op opname [make_param (MString (Lm_symbol.add s))]) []

let mk_lexer_redex i =
   let rec collect redex i =
      if i = 0 then
         mk_mstring_term lexer_arg_opname "lexeme" :: redex
      else
         collect (mk_mstring_term lexer_arg_opname ("arg" ^ string_of_int i) :: redex) (pred i)
   in
   let args = collect [] i in
      mk_simple_term lexer_arg_opname args

let mk_lexer_term lexeme args =
   let args = List.map (mk_string_term lexer_arg_opname) args in
   let args = mk_string_term lexer_arg_opname lexeme :: args in
      mk_simple_term lexer_arg_opname args

(*
 * Add a lexer token.
 *)
let add_token gram lexer_id id s contractum_opt =
   let { gram_lexers = lexers;
         gram_lexer_actions = actions;
         gram_lexer_clauses = clauses
       } = gram
   in
   let lexer =
      try OpnameTable.find lexers lexer_id with
         Not_found ->
            Lexer.empty
   in
   let arity, lexer = Lexer.add_clause lexer id s in
   let lexers = OpnameTable.add lexers lexer_id lexer in
   let redex = mk_lexer_redex arity in
   let rw_opt =
      match contractum_opt with
         Some contractum ->
            LexerRewrite (term_rewrite Rewrite_sig.Relaxed empty_args_spec [redex] [contractum])
       | None ->
            LexerNone
   in
   let actions = ActionTable.add actions id rw_opt in
   let clause =
      { lexer_regex = s;
        lexer_redex = redex;
        lexer_contractum = contractum_opt
      }
   in
   let clauses = ActionTable.add clauses id clause in
      { gram with gram_name          = None;
                  gram_lexers        = lexers;
                  gram_lexer_actions = actions;
                  gram_lexer_clauses = clauses
      }

(*
 * Add a matched pair.
 *)
let add_token_pair gram lexer_id id s1 s2 contractum_opt =
   (* Make a new lexer for the pair *)
   let lexer_pair = BoolLexer.empty in
   let _, lexer_pair = BoolLexer.add_clause lexer_pair true s1 in
   let _, lexer_pair = BoolLexer.add_clause lexer_pair false s2 in

   (* Now add the initial production to the normal lexer *)
   let { gram_lexers = lexers;
         gram_lexer_actions = actions;
         gram_lexer_clauses = clauses
       } = gram
   in
   let lexer =
      try OpnameTable.find lexers lexer_id with
         Not_found ->
            Lexer.empty
   in
   let _, lexer = Lexer.add_clause lexer id s1 in
   let lexers = OpnameTable.add lexers lexer_id lexer in
   let redex = mk_lexer_redex 2 in
   let rw_opt =
      match contractum_opt with
         Some contractum ->
            LexerRewrite (term_rewrite Rewrite_sig.Relaxed empty_args_spec [redex] [contractum])
       | None ->
            LexerNone
   in
   let actions = ActionTable.add actions id (LexerPair (lexer_pair, rw_opt)) in
   let clause =
      { lexer_regex = s1;
        lexer_redex = redex;
        lexer_contractum = contractum_opt
      }
   in
   let clauses = ActionTable.add clauses id clause in
      { gram with gram_name          = None;
                  gram_lexers        = lexers;
                  gram_lexer_actions = actions;
                  gram_lexer_clauses = clauses
      }

(*
 * Add a parser action.
 *)
let add_production gram id args opt_prec contractum =
   let { gram_parser = parse;
         gram_parser_actions = actions;
         gram_parser_clauses = clauses
       } = gram
   in
   let ops = List.map shape_of_term args in
   let name = shape_of_term contractum in
   let parse = Parser.add_production parse id name ops opt_prec in
   let redex = mk_simple_term parser_arg_opname args in
   let rw = term_rewrite Rewrite_sig.Relaxed empty_args_spec [redex] [contractum] in
   let actions = ActionTable.add actions id rw in
   let clause =
      { parser_prec = opt_prec;
        parser_redex = redex;
        parser_contractum = contractum
      }
   in
   let clauses = ActionTable.add clauses id clause in
      { gram with gram_name           = None;
                  gram_parser         = parse;
                  gram_parser_actions = actions;
                  gram_parser_clauses = clauses
      }

(************************************************************************
 * Precedences.
 *)
type assoc = Lm_parser.assoc =
   LeftAssoc
 | RightAssoc
 | NonAssoc
 | NoneAssoc

type precedence = ParserPrecedence.precedence

let prec_min = Parser.prec_min
let prec_max = Parser.prec_max

let find_prec gram v =
   Parser.find_prec gram.gram_parser v

let create_prec_new gram assoc =
   let parse, pre = Parser.create_prec_gt gram.gram_parser prec_min assoc in
   let gram = { gram with gram_name = None; gram_parser = parse } in
      gram, pre

let create_prec_lt gram v assoc =
   let pre = find_prec gram v in
   let parse, pre = Parser.create_prec_lt gram.gram_parser pre assoc in
   let gram = { gram with gram_name = None; gram_parser = parse } in
      gram, pre

let create_prec_gt gram v assoc =
   let pre = find_prec gram v in
   let parse, pre = Parser.create_prec_gt gram.gram_parser pre assoc in
   let gram = { gram with gram_name = None; gram_parser = parse } in
      gram, pre

let add_prec gram pre v =
   { gram with gram_name = None; gram_parser = Parser.add_prec gram.gram_parser pre v }

(************************************************************************
 * Start symbols.
 *)
let add_start gram v lexer_id =
   { gram with gram_name = None;
               gram_lexer_start = ShapeTable.add gram.gram_lexer_start v lexer_id;
               gram_parser = Parser.add_start gram.gram_parser v
   }

let get_start gram =
   Parser.get_start gram.gram_parser

(************************************************************************
 * Input forms.
 *)

(*
 * Add a new iform.  Clear the cached table.
 *)
let add_iform gram id redex contractum =
   let iform =
      { iform_redex = redex;
        iform_contractum = contractum
      }
   in
      { gram with gram_name = None;
                  gram_iforms = ActionTable.add gram.gram_iforms id iform;
                  gram_iform_table = None
      }

(*
 * For debugging.
 *)
let iform_count gram =
   ActionTable.cardinal gram.gram_iforms

(*
 * Build the Term_match_table.
 *)
let create_iform_table iforms =
   ActionTable.fold (fun table _ iform ->
         let { iform_redex = redex; iform_contractum = contractum } = iform in
         let conv = Conversionals.create_iform "Filter_grammar" false redex contractum in
            Term_match_table.add_item table redex conv) Term_match_table.empty_table iforms

let table_of_iforms gram =
   match gram.gram_iform_table with
      Some table ->
         table
    | None ->
         let table = create_iform_table gram.gram_iforms in
            gram.gram_iform_table <- Some table;
            table

let compile_iforms gram =
   ignore (table_of_iforms gram)

(*
 * The actual rewrite.
 *)
let conv_of_iforms gram =
   let table = table_of_iforms gram in
   let rw t =
      try Term_match_table.lookup table Term_match_table.select_all t with
         Not_found ->
            raise (RefineError ("Conversionals.extract_data", StringTermError ("no reduction for", t)))
   in
      Conversionals.termC rw

(*
 * Apply the iforms.
 *)
let () = Mp_resource.recompute_top ()

(*
 * The primitive so-var terms.
 *)
let perv_opname = mk_opname "Perv" nil_opname
let xsovar_opname = mk_opname "xsovar" perv_opname
let xhypcontext_opname = mk_opname "xhypcontext" perv_opname

let is_xsovar_term t =
   if is_var_dep0_dep0_term xsovar_opname t then
      let _, cvars, args = dest_var_dep0_dep0_term xsovar_opname t in
         is_xlist_term cvars && is_xlist_term args
   else
      false

let dest_xsovar_term t =
   let v, cvars, args = dest_var_dep0_dep0_term xsovar_opname t in
   let cvars = List.map dest_var (dest_xlist cvars) in
   let args = dest_xlist args in
      v, cvars, args

let is_xhypcontext_term t =
   if is_dep0_dep0_term xhypcontext_opname t then
      let cvars, args = dest_dep0_dep0_term xhypcontext_opname t in
         is_xlist_term cvars && is_xlist_term args
   else
      false

let dest_xhypcontext_term t =
   let cvars, args = dest_dep0_dep0_term xhypcontext_opname t in
   let cvars = List.map dest_var (dest_xlist cvars) in
   let args = dest_xlist args in
      cvars, args

(*
 * Also expand quotations.
 * For an xquotation, the string parameter should
 * have the form <:name<...>>.
 *)
let xquotation_opname = mk_opname "xquotation" perv_opname
let is_xquotation_term = is_string_string_term xquotation_opname
let dest_xquotation = dest_string_string_term xquotation_opname

let is_quote_char = function
   '<'
 | '>'
 | '-'
 | '+' ->
      true
 | _ ->
      false

let trim_quotation_name default name =
   let len = String.length name in
   let rec search_left i =
      if i = len then
         i
      else if is_quote_char name.[i] then
         search_left (succ i)
      else
         i
   in
   let rec search_right i =
      if i = len then
         i
      else if is_quote_char name.[i] then
         search_right (succ i)
      else
         i
   in
   let left = search_left 0 in
   let right = search_right left in
      if right = left then
         default
      else
         String.sub name left (right - left)

let unfold_xquotation parse_quotation t =
   let name, quote = dest_xquotation t in
      parse_quotation name quote

let rec sweep_up_unfold_xquotation parse_quotation t =
   map_up (fun t ->
         if is_xquotation_term t then
            let t = unfold_xquotation parse_quotation t in
               sweep_up_unfold_xquotation parse_quotation t
         else
            t) t

(*
 * The so-var iforms are primitive, because hyps are rewritten to contexts.
 *)
let apply_sovar_iforms parse_quotation apply_iforms t =
   let rec apply_so_var_iforms_term t =
      if is_var_term t then
         t
      else if is_so_var_term t then
         let v, cvars, args = dest_so_var t in
            mk_so_var_term v cvars (apply_so_var_iforms_term_list args)
      else if is_context_term t then
         let v, arg, cvars, args = dest_context t in
            mk_context_term v (apply_so_var_iforms_term arg) cvars (apply_so_var_iforms_term_list args)
      else if is_sequent_term t then
         let { sequent_args = arg;
               sequent_hyps = hyps;
               sequent_concl = concl
             } = explode_sequent t
         in
         let arg = apply_so_var_iforms_term arg in
         let concl = apply_so_var_iforms_term concl in
         let hyps =
            SeqHyp.map (fun hyp ->
                  match hyp with
                     Hypothesis (v, t) ->
                        if is_xhypcontext_term t then
                           let cvars, args = dest_xhypcontext_term t in
                           let args = apply_so_var_iforms_term_list args in
                              Context (v, cvars, args)
                        else
                           Hypothesis (v, apply_so_var_iforms_term t)
                   | Context (v, cvars, args) ->
                        Context (v, cvars, apply_so_var_iforms_term_list args)) hyps
         in
         let seq =
            { sequent_args = arg;
              sequent_hyps = hyps;
              sequent_concl = concl
            }
         in
            mk_sequent_term seq
      else if is_xsovar_term t then
         let v, cvars, args = dest_xsovar_term t in
         let args = apply_so_var_iforms_term_list args in
            mk_so_var_term v cvars args
      else if is_xquotation_term t then
         let t = unfold_xquotation parse_quotation t in
            apply_so_var_iforms_term (apply_iforms t)
      else
         let { term_op = op; term_terms = bterms } = dest_term t in
         let bterms = apply_so_var_iforms_bterm_list bterms in
            mk_term op bterms

   and apply_so_var_iforms_term_list terms =
      List.map apply_so_var_iforms_term terms

   and apply_so_var_iforms_bterm bterm =
      let { bvars = bvars; bterm = t } = dest_bterm bterm in
         mk_bterm bvars (apply_so_var_iforms_term t)

   and apply_so_var_iforms_bterm_list bterms =
      List.map apply_so_var_iforms_bterm bterms
   in
      apply_so_var_iforms_term (apply_iforms t)

(*
 * Now actually apply the input forms.
 *)
let apply_iforms parse_quotation gram t =
   let conv = conv_of_iforms gram in
   let conv = Conversionals.repeatC (Conversionals.higherC conv) in
   let book = Mp_resource.find Mp_resource.top_bookmark in
      apply_sovar_iforms parse_quotation (Conversionals.apply_rewrite book conv) t

let apply_iforms_mterm parse_quotation gram mt args =
   let conv = conv_of_iforms gram in
   let conv = Conversionals.repeatC (Conversionals.higherC conv) in
   let book = Mp_resource.find Mp_resource.top_bookmark in
   let apply_iforms t =
      Conversionals.apply_rewrite book conv t
   in
   let apply_term t =
      apply_sovar_iforms parse_quotation apply_iforms t
   in
   let rec apply mt =
      match mt with
         MetaTheorem t ->
            MetaTheorem (apply_term t)
       | MetaImplies (mt1, mt2) ->
            MetaImplies (apply mt1, apply mt2)
       | MetaIff (mt1, mt2) ->
            MetaIff (apply mt1, apply mt2)
       | MetaFunction (t, mt1, mt2) ->
            MetaFunction (apply_term t, apply mt1, apply mt2)
       | MetaLabeled (l, mt) ->
            MetaLabeled (l, apply mt)
   in
      apply mt, List.map apply_term args

(************************************************************************
 * Utilities.
 *)
let compile gram =
   let { gram_lexers = lexers;
         gram_parser = parse
       } = gram
   in
      OpnameTable.iter (fun _ lexer -> Lexer.compile lexer) lexers;
      Parser.compile parse;
      compile_iforms gram

let prepare_to_marshal gram name =
   let { gram_name     = gram_name;
         gram_subnames = subnames;
         gram_lexers   = lexers;
         gram_parser   = parse
       } = gram
   in
      match gram_name with
         Some _ ->
            (* If it is already named, don't rename it *)
            ()
       | None ->
            OpnameTable.iter (fun _ lexer -> Lexer.compile lexer) lexers;
            Parser.compile parse;
            gram.gram_iform_table <- None;
            gram.gram_name <- Some name;
            gram.gram_subnames <- StringSet.add subnames name

let unmarshal gram =
   if gram.gram_magic <> gram_magic then begin
      eprintf "! A grammar that was loaded from a file is out-of-date.\n";
      eprintf "! This is probably not a problem, but if you have trouble\n";
      eprintf "! you should export your work and recompile MetaPRL.@.";
      empty
   end
   else
      gram

let is_modified gram =
   gram.gram_name = None && not (is_empty gram)

let pp_print_grammar buf gram =
   let { gram_lexers = lexers;
         gram_parser = parse
       } = gram
   in
      debug_grammar := true;
      fprintf buf "@[<v 0>";
      OpnameTable.iter (fun name lexer ->
            fprintf buf "@[<v 3>Lexer %s@ %a@]" (string_of_opname name) Lexer.pp_print_lexer lexer) lexers;
      fprintf buf "@ %a@]" Parser.pp_print_parser parse

(************************************************************************
 * Actual parsing.
 *)

(*
 * Parse the input.
 *)
let parse parse_quotation gram start loc s =
   let { gram_lexers         = lexers;
         gram_lexer_actions  = lexer_actions;
         gram_lexer_start    = starts;
         gram_parser         = parse;
         gram_parser_actions = parser_actions
       } = gram
   in

   (* Input channel *)
   let { Lexing.pos_fname = filename;
         Lexing.pos_lnum  = line;
         Lexing.pos_bol   = char
       } = loc
   in
(*
   let input = Lm_channel.of_loc_string filename line char s in
 *)
   let input = Lm_channel.of_loc_string filename 1 0 s in

   (* Get the lexer *)
   let lexer =
      try OpnameTable.find lexers (ShapeTable.find starts start) with
         Not_found ->
            raise (Failure ("unknown start symbol: " ^ string_of_shape start))
   in

   (* Rewrite a lexeme; handle quotations here *)
   let rewrite_lexeme rw loc lexeme args =
      let arg = mk_lexer_term lexeme args in
      let tok =
         match apply_rewrite rw empty_args arg [] with
            [tok] ->
               tok
          | _ ->
               raise (Invalid_argument "lexer_fun")
      in
      let tok = sweep_up_unfold_xquotation parse_quotation tok in
      let shape = shape_of_term tok in
         if !debug_grammar then
            eprintf "Token: %a@." pp_print_shape shape;
         shape, loc, (), tok
   in

   (* Process a lexeme *)
   let rec lexer_action rw_opt loc lexeme args =
      match rw_opt with
         LexerRewrite rw ->
            rewrite_lexeme rw loc lexeme args
       | LexerPair (lexer_bool, rw_opt) ->
            lexer_match lexer_bool rw_opt loc (Buffer.create 64) lexeme 0
       | LexerNone ->
            lexer_fun ()

   (* Lex a matched pair *)
   and lexer_match lexer rw_opt loc buf name level =
      let text, key = BoolLexer.search lexer input in
         Buffer.add_string buf text;

         match key with
            Some (false, arg, _) ->
               (* Terminator *)
               if level = 0 then
                  let name = trim_quotation_name "term" name in
                  let arg = trim_quotation_name name arg in
                     lexer_action rw_opt loc (Buffer.contents buf) [name; arg]
               else begin
                  Buffer.add_string buf arg;
                  lexer_match lexer rw_opt loc buf name (pred level)
               end
          | Some (true, arg, _) ->
               Buffer.add_string buf arg;
               lexer_match lexer rw_opt loc buf name (succ level)
          | None ->
               (* Unexpected EOF *)
               raise (Failure ("unexpected end-of-file in quotation " ^ name))

   (* General lexing *)
   and lexer_fun () =
      let action, loc, lexeme, args = Lexer.lex lexer input in
      let () =
         if !debug_grammar then
            eprintf "Lexeme: \"%s\"@." (String.escaped lexeme)
      in
      let rw_opt = ActionTable.find lexer_actions action in
         lexer_action rw_opt loc lexeme args
   in

   (* Semantic action evaluator *)
   let eval_fun () action loc args =
      let rw = ActionTable.find parser_actions action in
      let arg = mk_simple_term parser_arg_opname args in
      let result =
         match apply_rewrite rw empty_args arg [] with
            [tok] ->
               tok
          | _ ->
               raise (Invalid_argument "eval_fun")
      in
         (), result
   in
   let _, result = Parser.parse parse start lexer_fun eval_fun () in
      result

(************************************************************************
 * Lazy version.
 *)


let info_gram gram =
   { gram_info = GramInfo gram }

(*
 * Flatten the grammar to a single one.
 *)
let flatten debug grams =
   let subs =
      StringTable.fold (fun sub name gram ->
            let subnames = StringSet.remove gram.gram_subnames name in
               StringSet.union sub subnames) StringSet.empty grams
   in
   let roots = StringSet.fold StringTable.remove grams subs in
   let roots =
      StringTable.fold (fun roots _ gram ->
            gram :: roots) [] roots
   in
      match roots with
         [] ->
            raise (Invalid_argument "Filter_grammar.flatten: cyclic grammar dependencies")
       | [gram] ->
            gram
       | gram :: rest ->
            if !debug_parsetiming then
               eprintf "Flatten: %s: taking union@." debug;
            List.fold_left union gram rest

let flatten debug info =
   match info.gram_info with
      GramCollection grams ->
         let gram = flatten debug grams in
            info.gram_info <- GramInfo gram;
            gram
    | GramInfo gram ->
         gram

(*
 * Get the collection.
 *)
let unflatten info =
   match info.gram_info with
      GramCollection grams ->
         grams
    | GramInfo gram ->
         let name =
            match gram.gram_name with
               Some name ->
                  name
             | None ->
                  raise (Invalid_argument "Filter_grammar.union: unnamed grammar")
         in
            StringTable.add StringTable.empty name gram

(*
 * Wrap all of the functions.
 *)
let empty = info_gram empty

let add_token info lexer_id id regex term =
   info_gram (add_token (flatten "add_token" info) lexer_id id regex term)

let add_token_pair info lexer_id id regex1 regex2 term =
   info_gram (add_token_pair (flatten "add_token_pair" info) lexer_id id regex1 regex2 term)

let add_production info id terms shape term =
   info_gram (add_production (flatten "add_production" info) id terms shape term)

let hash_grammar info =
   hash_grammar (flatten "hash_grammar" info)

let find_prec info shape =
   find_prec (flatten "find_prec" info) shape

let create_prec_new info assoc =
   let gram, pre = create_prec_new (flatten "create_prec_new" info) assoc in
      info_gram gram, pre

let create_prec_lt info shape assoc =
   let gram, pre = create_prec_lt (flatten "create_prec_lt" info) shape assoc in
      info_gram gram, pre

let create_prec_gt info shape assoc =
   let gram, pre = create_prec_gt (flatten "create_prec_gt" info) shape assoc in
      info_gram gram, pre

let add_prec info pre shape =
   info_gram (add_prec (flatten "add_prec" info) pre shape)

let add_start info shape opname =
   info_gram (add_start (flatten "add_start" info) shape opname)

let get_start info =
   match info.gram_info with
      GramCollection grams ->
         StringTable.fold (fun shapes _ gram ->
               List.fold_left ShapeSet.add shapes (get_start gram)) ShapeSet.empty grams
    | GramInfo gram ->
         List.fold_left ShapeSet.add ShapeSet.empty (get_start gram)

let add_iform info id redex contractum =
   info_gram (add_iform (flatten "add_iform" info) id redex contractum)

let iform_count info =
   iform_count (flatten "iform_count" info)

let compile info =
   compile (flatten "compile" info)

let prepare_to_marshal info name =
   prepare_to_marshal (flatten "prepare_to_marshal" info) name

let unmarshal info =
   info_gram (unmarshal (flatten "unmarshal" info))

let is_modified info =
   is_modified (flatten "is_modified" info)

let pp_print_grammar buf info =
   pp_print_grammar buf (flatten "pp_print_grammar" info)

let parse quote info shape pos s =
   parse quote (flatten "parse" info) shape pos s

let apply_iforms quote info t =
   apply_iforms quote (flatten "apply_iforms" info) t

let apply_iforms_mterm quote info mt =
   apply_iforms_mterm quote (flatten "apply_iforms_mterm" info) mt

(*
 * A grammar is empty if all the subgrammars are empty.
 *)
let is_empty info =
   match info.gram_info with
      GramCollection grams ->
         StringTable.forall (fun _ gram ->
               is_empty gram) grams
    | GramInfo gram ->
         is_empty gram

(*
 * Take the union by building a table containing the two grammars.
 * The grammars must be named.
 *)
let union info1 info2 =
   let grams1 = unflatten info1 in
   let grams2 = unflatten info2 in
      { gram_info = GramCollection (StringTable.fold StringTable.add grams1 grams2) }

(************************************************************************
 * Imperative version.
 *)
type state =
   { mutable state_grammar : t option;
     mutable state_starts  : shape StringTable.t
   }

let state =
   { state_grammar = None;
     state_starts  = StringTable.empty
   }

let set_grammar gram =
   state.state_grammar <- Some gram

let set_start s shape =
   state.state_starts <- StringTable.add state.state_starts s shape

let term_of_string parse_quotation name loc s =
   match state.state_grammar with
      Some gram ->
         let start = StringTable.find state.state_starts name in
            parse parse_quotation gram start loc s
    | None ->
         raise (Failure "grammar is not initialized")

let apply_iforms parse_quotation t =
   match state.state_grammar with
      Some gram ->
         apply_iforms parse_quotation gram t
    | None ->
         raise (Failure "grammar is not initialized")

let apply_iforms_mterm parse_quotation mt args =
   match state.state_grammar with
      Some gram ->
         apply_iforms_mterm parse_quotation gram mt args
    | None ->
         raise (Failure "grammar is not initialized")

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
