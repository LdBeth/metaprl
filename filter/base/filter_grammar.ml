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
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermShape
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError

open Tactic_type

open Term_hash_code

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

module Action =
struct
   type action = id

   let pp_print_action = pp_print_symbol

   module ActionSet = SymbolSet;;

   let choose = max
end;;

module ActionTable = SymbolTable;;

module Lexer = MakeLexer (Lm_channel.LexerInput) (Action);;

(*
 * CFG variables are symbols.
 *)
module ParserArg =
struct
   type symbol = shape

   let eof_term = mk_simple_term (mk_opname "<eof>" nil_opname) []
   let eof = shape_of_term eof_term

   let to_string = string_of_shape

   let pp_print_symbol buf op =
      pp_print_string buf (to_string op)

   module SymbolCompare =
   struct
      type t = symbol
      let compare = Pervasives.compare
   end;;

   module SymbolSet = Lm_set.LmMake (SymbolCompare);;
   module SymbolTable = Lm_map.LmMake (SymbolCompare);;
   module SymbolMTable = Lm_map.LmMakeList (SymbolCompare);;

   include Action
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
type lexer_action = rewrite_rule

type lexer_clause =
   { lexer_regex      : string;
     lexer_redex      : term;
     lexer_contractum : term option
   }

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
 *)
type t =
   { gram_name                : string option;
     gram_lexer               : Lexer.t;
     gram_lexer_actions       : lexer_action option ActionTable.t;
     gram_lexer_clauses       : lexer_clause ActionTable.t;
     gram_parser              : Parser.t;
     gram_parser_actions      : parser_action ActionTable.t;
     gram_parser_clauses      : parser_clause ActionTable.t;
     gram_iforms              : iform ActionTable.t;
     mutable gram_iform_table : TacticTypes.conv Term_match_table.term_table option
   }

(* %%MAGICEND%% *)

let empty =
   { gram_name           = None;
     gram_lexer          = Lexer.empty;
     gram_lexer_actions  = ActionTable.empty;
     gram_lexer_clauses  = ActionTable.empty;
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

let union gram1 gram2 =
   if !debug_grammar then
      eprintf "Grammar union: %a, %a@." pp_print_string_opt gram1.gram_name pp_print_string_opt gram2.gram_name;
   if is_empty gram1 then
      gram2
   else if is_empty gram2 || gram1.gram_name <> None && gram2.gram_name = gram1.gram_name then
      gram1
   else
      let { gram_lexer          = lexer1;
            gram_lexer_actions  = lexer_actions1;
            gram_lexer_clauses  = lexer_clauses1;
            gram_parser         = parser1;
            gram_parser_actions = parser_actions1;
            gram_parser_clauses = parser_clauses1;
            gram_iforms         = iforms1
          } = gram1
      in
      let { gram_lexer          = lexer2;
            gram_lexer_actions  = lexer_actions2;
            gram_lexer_clauses  = lexer_clauses2;
            gram_parser         = parser2;
            gram_parser_actions = parser_actions2;
            gram_parser_clauses = parser_clauses2;
            gram_iforms         = iforms2
          } = gram2
      in
         if !debug_grammar then
            eprintf "Grammar union: active: %a, %a@." pp_print_string_opt gram1.gram_name pp_print_string_opt gram2.gram_name;
         { gram_name           = None;
           gram_lexer          = Lexer.union lexer1 lexer2;
           gram_parser         = Parser.union parser1 parser2;
           gram_lexer_actions  = ActionTable.fold ActionTable.add lexer_actions1 lexer_actions2;
           gram_parser_actions = ActionTable.fold ActionTable.add parser_actions1 parser_actions2;
           gram_lexer_clauses  = ActionTable.fold ActionTable.add lexer_clauses1 lexer_clauses2;
           gram_parser_clauses = ActionTable.fold ActionTable.add parser_clauses1 parser_clauses2;
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
let add_token gram id s contractum_opt =
   let { gram_lexer = lexer;
         gram_lexer_actions = actions;
         gram_lexer_clauses = clauses
       } = gram
   in
   let arity, lexer = Lexer.add_clause lexer id s in
   let redex = mk_lexer_redex arity in
   let rw_opt =
      match contractum_opt with
         Some contractum ->
            Some (term_rewrite Rewrite_sig.Relaxed empty_args_spec [redex] [contractum])
       | None ->
            None
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
                  gram_lexer         = lexer;
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
let add_start gram v =
   { gram with gram_name = None; gram_parser = Parser.add_start gram.gram_parser v }

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

let apply_iforms gram t =
   let conv = conv_of_iforms gram in
   let conv = Conversionals.repeatC (Conversionals.higherC conv) in
   let book = Mp_resource.find Mp_resource.top_bookmark in
      Conversionals.apply_rewrite book conv t

let apply_iforms_mterm gram mt args =
   let conv = conv_of_iforms gram in
   let conv = Conversionals.repeatC (Conversionals.higherC conv) in
   let book = Mp_resource.find Mp_resource.top_bookmark in
   let apply_term t =
      Conversionals.apply_rewrite book conv t
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
   let { gram_lexer = lexer;
         gram_parser = parse
       } = gram
   in
      Lexer.compile lexer;
      Parser.compile parse;
      compile_iforms gram

let prepare_to_marshal gram name =
   let { gram_lexer = lexer;
         gram_parser = parse
       } = gram
   in
      Lexer.compile lexer;
      Parser.compile parse;
      gram.gram_iform_table <- None;
      { gram with gram_name = Some name }

let is_modified gram =
   gram.gram_name = None && not (is_empty gram)

let pp_print_grammar buf gram =
   let { gram_lexer = lexer;
         gram_parser = parse
       } = gram
   in
      debug_grammar := true;
      fprintf buf "@[<v 0>%a@ %a@]" (**)
         Lexer.pp_print_lexer lexer
         Parser.pp_print_parser parse

(************************************************************************
 * Actual parsing.
 *)
let parse gram start loc s =
   let { gram_lexer          = lexer;
         gram_lexer_actions  = lexer_actions;
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
   let input = Lm_channel.of_loc_string filename line char s in

   (* Lexer *)
   let rec lexer_fun () =
      let action, loc, lexeme, args = Lexer.lex lexer input in
      let () =
         if !debug_grammar then
            eprintf "Lexeme: \"%s\"@." (String.escaped lexeme)
      in
      let rw_opt = ActionTable.find lexer_actions action in
         match rw_opt with
            Some rw ->
               let arg = mk_lexer_term lexeme args in
               let tok =
                  match apply_rewrite rw empty_args arg [] with
                     [tok] ->
                        tok
                   | _ ->
                        raise (Invalid_argument "lexer_fun")
               in
               let shape = shape_of_term tok in
                  if !debug_grammar then
                     eprintf "Token: %a@." pp_print_shape shape;
                  shape, loc, (), tok
          | None ->
               lexer_fun ()
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
      apply_iforms gram result

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

let term_of_string name loc s =
   match state.state_grammar with
      Some gram ->
         let start = StringTable.find state.state_starts name in
            parse gram start loc s
    | None ->
         raise (Failure "grammar is not initialized")

let apply_iforms t =
   match state.state_grammar with
      Some gram ->
         apply_iforms gram t
    | None ->
         raise (Failure "grammar is not initialized")

let apply_iforms_mterm mt args =
   match state.state_grammar with
      Some gram ->
         apply_iforms_mterm gram mt args
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
