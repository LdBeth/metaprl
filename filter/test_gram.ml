(*
 * Test a term.
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

let rec print_list () = function
   [h] ->
      h
 | h::t ->
      h ^ ";" ^ (print_list () t)
 | [] ->
      ""

let gram = Grammar.create Plexer.make
let term_eoi = Grammar.Entry.create gram "term"
let term = Grammar.Entry.create gram "term"

EXTEND
   GLOBAL: term_eoi term;

   term_eoi: [[ t = term; EOI -> t ]];

   term:
      [[ t = aterm ->
          t
       ]];

   aterm:
      [[ t = noncommaterm ->
          t
        | t1 = noncommaterm; sl_comma; t2 = noncommaterm ->
          t1 ^ "," ^ t2
       ]];

   noncommaterm:
      [[ t = nonwordterm ->
          t
        | t = wordterm ->
          t
        | t = wordterm; (params, bterms) = termsuffix ->
          sprintf "%s[%a][%a]" t print_list params print_list bterms
       ]];

   wordterm:
      [[ o = opname ->
          o
       ]];

   nonwordterm:
      [[ v = varterm ->
          v
        | sl_wild_card ->
          "_"
        | i = sl_number ->
          string_of_int i
       ]];

   termsuffix:
      [[ p = params ->
          p, []
        | p = params; sl_open_curly; bterms = btermslist; sl_close_curly ->
          p, bterms
        | sl_open_curly; bterms = btermslist; sl_close_curly ->
          [], bterms
       ]];

   varterm:
      [[ sl_single_quote; v = sl_word ->
          v
       ]];

   opname:
      [[ w = sl_word ->
          w
        | op = opname; sl_exclamation; w = sl_word ->
          op ^ "!" ^ w
       ]];

   params:
      [[ sl_open_brack; params = OPT paramlist; sl_close_brack ->
          match params with
             Some params' -> params'
           | None -> []
       ]];

   paramlist:
      [[ p = param ->
          [p]
        | p = param; sl_comma; l = paramlist ->
          p :: l
       ]];

   param:
      [[ w = sl_word ->
          w
       ]];

   btermslist:
      [[ l = OPT btermlist ->
          match l with
             Some l' -> l'
           | None -> []
       ]];

   btermlist:
      [[ t = bterm ->
          [t]
        | l = btermlist; sl_semi_colon; t = bterm ->
          l @ [t]
       ]];

   bterm:
      [[ w = sl_word ->
         w
       ]];

   sl_word:
      [[  s = LIDENT -> s
        | s = UIDENT -> s
       ]];

   sl_number:
      [[ n = INT ->
          int_of_string n
       ]];

   sl_open_brack:
      [[ "[" -> () ]];

   sl_close_brack:
      [[ "]" -> () ]];

   sl_open_curly:
      [[ "{" -> () ]];

   sl_close_curly:
      [[ "}" -> () ]];

   sl_comma:
      [[ "," -> () ]];

   sl_semi_colon:
      [[ ";" -> () ]];

   sl_exclamation:
      [[ "!" -> () ]];

   sl_wild_card:
      [[ "_" -> () ]];

   sl_single_quote:
      [[ "'" -> () ]];
END

let term_exp s =
   let cs = Gstream.of_string s in
   let s = Grammar.Entry.parse term_eoi cs in
      sprintf "\"%s\"" (String.escaped s)

let _ = Quotation.add "term" term_exp
let _ = Quotation.default := "term"

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
