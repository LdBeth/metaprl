(*
 * @begin[doc]
 * @module[Base_parser]
 *
 * The @hrefmodule[Base_parser] module implements the base resource
 * for parsing and lexing.
 *
 * A lexer entry has a string and a term.
 * For example, @code{("[0-9]+", << number[$0$:n] >>)}
 *
 * @docoff
 * @end[doc]
 *
 * Define a resource for parsing and lexing.
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

(*
 * @begin[doc]
 * @parents
 * @end[doc]
 *)
extends Perv
extends Nuprl_font
(* @docoff *)

open Lm_debug
open Lm_symbol
open Lm_printf
open Lm_lexer
open Lm_parser
open Lm_int_set

open Refiner.Refiner.TermType

open Mp_resource

(*
 * Terms.
 *)
declare TokEof

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Base_parser%t"

(************************************************************************
 * Lexer.
 *)
module LexAction =
struct
   type action = int

   let pp_print_action = pp_print_int
end;;

module Lexer = MakeLexer (Lm_channel.LexerInput) (LexAction);;

type lexer_item = string * (string array -> symbol * term)

type lexer =
   { lexer_info    : Lexer.t;
     lexer_actions : (string array -> symbol * term) IntTable.t
   }

let lexer_empty =
   { lexer_info    = Lexer.empty;
     lexer_actions = IntTable.empty
   }

(*
 * Add a clause to the lexer.
 *)
let lexer_improve lexer (regex, t) =
   let { lexer_info = info;
         lexer_actions = actions
       } = lexer
   in
   let index = Lm_symbol.new_number () in
   let info = Lexer.add_clause info index regex in
   let actions = IntTable.add actions index t in
      { lexer_info = info;
        lexer_actions = actions
      }

(*
 * Not much to do during extraction.
 *)
let lexer_extract lexer =
   lexer

(*
 * The lexer resource.
 *)
let eof_sym = Lm_symbol.add "eof"

let lexer_init =
   ["\\'", (fun argv -> eof_sym, <:con< TokEof >>)]

let default_lexer = List.fold_left lexer_improve lexer_empty lexer_init

let lexer_collection =
   Functional (**)
      { fp_empty = default_lexer;
        fp_add   = lexer_improve;
        fp_retr  = lexer_extract
      }

let resource (lexer_item, lexer) lexer =
   lexer_collection

(************************************************************************
 * Parser.
 *)

(*
 * Precedences are based on MetaPRL precedences,
 * rather than the default Parser precedences.
 *)
type assoc = Lm_parser.assoc =
   LeftAssoc
 | RightAssoc
 | NonAssoc

module PrecedenceArg =
struct
   type precedence = Precedence.precedence

   (* The table just maps the associativity, not include in the Precedence module *)
   module PrecCompare =
   struct
      type t = precedence
      let compare = Pervasives.compare
   end;;

   module PrecTable = Lm_map.LmMake (PrecCompare);;

   type t = assoc PrecTable.t

   let prec_min = Precedence.min_prec
   let prec_max = Precedence.max_prec

   let empty    =
      let table = PrecTable.empty in
      let table = PrecTable.add table prec_min NonAssoc in
      let table = PrecTable.add table prec_max NonAssoc in
         table

   let create_prec_lt table pre assoc =
      let pre' = Precedence.new_prec () in
      let () = Precedence.add_lt pre' pre in
      let table = PrecTable.add table pre' assoc in
         table, pre'

   let create_prec_gt table pre assoc =
      let pre' = Precedence.new_prec () in
      let () = Precedence.add_lt pre pre' in
      let table = PrecTable.add table pre' assoc in
         table, pre'

   let pp_print_prec table buf pre =
      let assoc = PrecTable.find table pre in
         fprintf buf "%a <precedence>" pp_print_assoc assoc

   let add_assoc table pre assoc =
      PrecTable.filter_add table pre (fun assoc' ->
            match assoc' with
               Some assoc' ->
                  if assoc' <> assoc then
                     raise (Failure "Base_parser.add_assoc: associativities do not match");
                  assoc'
             | None ->
                  assoc)

   let assoc table pre =
      PrecTable.find table pre

   let compare table pre1 pre2 =
      match Precedence.get_prec pre1 pre2 with
         Precedence.NoRelation
       | Precedence.EQRelation ->
            0
       | Precedence.LTRelation ->
            -1
       | Precedence.GTRelation ->
            1
end

(*
 * CFG variables are symbols.
 *)
module ParserArg =
struct
   type symbol = Lm_symbol.symbol

   let to_string = Lm_symbol.to_string
   let pp_print_symbol = Lm_symbol.pp_print_symbol

   module SymbolSet = Lm_symbol.SymbolSet;;
   module SymbolTable = Lm_symbol.SymbolTable;;

   type action = int

   let pp_print_action = pp_print_int
end;;

(*
 * The actual parser.
 *)
module Parser = Lm_parser.MakeParser (ParserArg) (PrecedenceArg);;

(*
 * The resource definition.
 *)
type parser_item =
   ParseStart      of symbol list
 | ParsePrec       of assoc * Precedence.precedence * symbol list
 | ParseProduction of symbol * symbol list * symbol option * (term array -> term)

type parser =
   { parser_info    : Parser.t;
     parser_actions : (term array -> term) IntTable.t
   }

let parser_empty =
   { parser_info    = Parser.empty;
     parser_actions = IntTable.empty
   }

(*
 * Add a clause to the lexer.
 *)
let parser_improve parser item =
   let { parser_info = info;
         parser_actions = actions
       } = parser
   in
      match item with
         ParsePrec (assoc, pre, syms) ->
            let info = Parser.add_assoc info pre assoc in
            let info =
               List.fold_left (fun info v ->
                     Parser.add_prec info pre v) info syms
            in
               { parser_info = info; parser_actions = actions }
       | ParseStart syms ->
            let info = List.fold_left Parser.add_start info syms in
               { parser_info = info; parser_actions = actions }
       | ParseProduction (name, vars, prec_option, t) ->
            let index = Lm_symbol.new_number () in
            let info = Parser.add_production info index name vars prec_option in
            let actions = IntTable.add actions index t in
               { parser_info = info;
                 parser_actions = actions
               }

(*
 * Not much to do during extraction.
 *)
let parser_extract parser =
   parser

(*
 * The parser resource.
 *)
let parser_init =
   []

let default_parser = List.fold_left parser_improve parser_empty parser_init

let parser_collection =
   Functional (**)
      { fp_empty = default_parser;
        fp_add   = parser_improve;
        fp_retr  = parser_extract
      }

let resource (parser_item, parser) parser =
   parser_collection

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
