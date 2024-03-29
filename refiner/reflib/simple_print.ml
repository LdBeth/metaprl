(*
 * Pretty printer for terms.
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
 *
 *)
open Lm_debug
open Lm_symbol
open Lm_printf
open Lm_printf_rbuffer
open Lm_rformat
open Lm_rformat_text

open Opname
open Term_sig
open Refiner_sig

(*
 * Show the file loading.
 *)
let () =
   show_loading "Loading Simple_print"

let debug_simple_print =
   create_debug (**)
      { debug_name = "simple_print";
        debug_description = "show simple printing operations";
        debug_value = false
      }

(*
 * Print "empty" vars as "_"
 *)
let dstring_of_var v =
   if Lm_symbol.to_string v = "" then "_" else Lm_symbol.string_of_symbol v

let format_quoted_var buf v =
   format_string buf "'";
   format_quoted_string buf (dstring_of_var v)

let format_var buf v =
   format_string buf (dstring_of_var v)

let format_loc buf (loc : MLast.loc) =
   let fname = Ploc.file_name loc in
   if (fname <> "") && (fname <> "-") then begin
      format_string buf "File \"";
      format_string buf fname;
      format_string buf "\", line ";
   end
   else
      format_string buf "Line ";
   let line = Ploc.line_nb loc in
   let lpos = Ploc.bol_pos loc in
   let bpos = Ploc.first_pos loc in
   let epos = Ploc.last_pos loc in
   format_int buf line;
   format_string buf ", ";
   format_string buf "characters ";
   format_int buf (bpos - lpos + 1);
   format_string buf "-";
   format_int buf (epos - lpos)

let string_of_loc loc =
   let buf = new_buffer () in
      format_loc buf loc;
      print_text_string 120 buf

module MakeSimplePrint (Refiner : RefinerSig) =
struct
   open Refiner
   open Refiner.Term
   open Refiner.TermType
   open Refiner.TermAddr
   open Refiner.TermMan
   open Refiner.TermShape

   type term = TermType.term
   type level_exp = TermType.level_exp
   type param = TermType.param
   type bound_term = TermType.bound_term
   type meta_term = TermType.meta_term
   type address = TermAddr.address
   type buffer = Lm_rformat.buffer

   (************************************************************************
    * PRINTERS                                                             *
    ************************************************************************)

   (* Level expression *)
   let format_level_exp buf l =
      if !debug_simple_print then
         eprintf "Simple_print.format_level_exp%t" eflush;
      let rec format_quotes = function
         0 ->
            ()
       | i ->
            format_char buf '\'';
            format_quotes (i - 1)
      in
      let format_var lv =
         match dest_level_var lv with
            { le_var = v; le_offset = o } ->
               if o < 3 then
                  begin
                     format_quoted_var buf v;
                     format_quotes o
                  end
               else
                  begin
                     format_quoted_var buf v;
                     format_char buf ' ';
                     format_int buf o
                  end
      in
         match dest_level l with
            { le_const = c; le_vars = [] } ->
               format_int buf c
          | { le_const = 0; le_vars = [v] } ->
               (match dest_level_var v with
                   { le_var = v; le_offset = o } ->
                      if o < 3 then
                         begin
                            format_quoted_var buf v;
                            format_quotes o
                         end
                      else
                         begin
                            format_string buf "{";
                            format_quoted_var buf v;
                            format_int buf o;
                            format_string buf "}"
                         end)

          | { le_const = c; le_vars = vars } ->
               let rec maxaux = function
                  [] -> 0
                | h::t ->
                     match dest_level_var h with
                        { le_var = _; le_offset = i } -> max i (maxaux t)
               in
               let maxoff = maxaux vars in
               let rec format_vars = function
                  [] -> ()
                | [h] -> format_var h
                | h::t ->
                     format_var h;
                     format_string buf " | ";
                     format_vars t
               in
                  format_char buf '{';
                  if maxoff < c then
                     begin
                        format_int buf c;
                        format_string buf " | "
                     end;
                  format_vars vars;
                  format_char buf '}'

   (*
    * Operator name.
    *)
   let string_of_opname opname =
      if !debug_simple_print then
         eprintf "Simple_print.string_of_opname%t" eflush;
      let rec aux v = function
         [] -> v
       | str::opname' ->
            let str' =
               if v = "" then
                  str
               else
                  str ^ "!" ^ v
            in
               aux str' opname'
      in
         aux "" (dest_opname opname)

    let rec format_list buf format = function
         [h] ->
            format buf h
       | h::t ->
            format buf h;
            format_string buf "; ";
            format_list buf format t
       | [] ->
            ()

   (* General parameter *)
   let rec format_param buf p =
      if !debug_simple_print then
         eprintf "Simple_print.format_param%t" eflush;
      match dest_param p with
         Number n -> format_num buf n; format_string buf ":n"
       | String s -> format_char buf '"'; format_string buf (String.escaped s); format_char buf '"'; format_string buf ":s"
       | Token t -> format_char buf '"'; format_string buf (String.escaped (string_of_opname t)); format_char buf '"'; format_string buf ":t"
       | Shape sh -> format_char buf '"'; format_string buf (String.escaped (string_of_shape sh)); format_char buf '"'; format_string buf ":sh"
       | Operator op ->
            format_char buf '(';
            format_string buf (String.escaped (string_of_opname op.opparam_name));
            format_paramlist buf op.opparam_params;
            format_char buf '{';
            format_list buf format_int op.opparam_arities;
            format_string buf "}):op"
       | Quote -> format_string buf "@"
       | MNumber v -> format_var buf v; format_string buf ":n"
       | MString v -> format_var buf v; format_string buf ":s"
       | MToken v -> format_var buf v; format_string buf ":t"
       | MShape v -> format_var buf v; format_string buf ":sh"
       | MOperator v -> format_var buf v; format_string buf ":op"
       | MLevel l -> format_level_exp buf l; format_string buf ":l"
       | Var v -> format_var buf v; format_string buf ":v"
       | ObId a -> format_string buf "<object-id>"
       | ParamList l ->
               format_string buf "[";
               format_list buf format_param l;
               format_string buf "]"

   (* List of params *)
   and format_paramlist buf = function
      [] -> ()
    | h::[] ->
         format_param buf h
    | h::t ->
         format_param buf h;
         format_char buf ',';
         format_space buf;
         format_paramlist buf t

   (* Optional empty params *)
   let format_params buf = function
      [] -> ()
    | _::_ as params ->
         (* format_space buf; *)
         format_char buf '[';
         format_pushm buf 1;
         format_paramlist buf params;
         format_char buf ']';
         format_popm buf

   (* Bound contexts list *)
   let rec format_contexts buf conts =
      format_pushm buf 1;
      format_string buf "<|";
      format_context_list buf conts;
      format_string buf "|>";
      format_popm buf

   and format_context_list buf = function
      [] ->
         ()
    | [v] ->
         format_var buf v
    | v :: vs ->
         format_var buf v;
         format_string buf ";";
         format_space buf;
         format_context_list buf vs

   (* Print a single bterm *)
   let rec format_bterm buf bterm =
      if !debug_simple_print then
         eprintf "Simple_print.format_bterm%t" eflush;
      match dest_bterm bterm with
         { bvars = []; bterm = term } ->
            format_term buf term
       | { bvars = vars; bterm = term } ->
            let rec format_bvars = function
               [] -> ()
             | [h] -> format_quoted_var buf h
             | h::t ->
                  format_quoted_var buf h;
                  format_string buf ", ";
                  format_bvars t
            in
               format_bvars vars;
               format_string buf ". ";
               format_term buf term

   (* Nonempty list *)
   and format_btermlist buf = function
      [] -> ()
    | [h] ->
         format_bterm buf h
    | h::t ->
         format_bterm buf h;
         format_char buf ';';
         format_space buf;
         format_btermlist buf t

   (* Optional empty bterm list *)
   and format_bterms buf = function
      [] -> ()
    | _::_ as bterms ->
         (* format_space buf; *)
         format_char buf '{';
         format_pushm buf 1;
         format_btermlist buf bterms;
         format_char buf '}';
         format_popm buf

   (*
    * Top level print function.
    *)
   and format_term buf term =
      if !debug_simple_print then
         eprintf "Simple_print.format_term%t" eflush;
      if is_var_term term then
         format_quoted_var buf (dest_var term)
      else if is_so_var_term term then
         begin
            if !debug_simple_print then
               eprintf "Simple_print.format_term: got a variable%t" eflush;
            let v, conts, subterms = dest_so_var term in
               if !debug_simple_print then
                  eprintf "Simple_print.format_term: var: %a%t" output_symbol v eflush;
               format_quoted_var buf v;
               if conts <> [v] then
                  format_contexts buf conts;
               format_terms buf subterms
         end
      else if is_context_term term then
         begin
            let v, t, conts, subterms = dest_context term in
               format_quoted_var buf v;
               if conts <> [v] then
                  format_contexts buf conts;
               format_pushm buf 2;
               format_string buf "[[";
               format_term buf t;
               format_string buf "]]";
               format_popm buf;
               format_terms buf subterms
         end
      else if is_sequent_term term then
         begin
            if !debug_simple_print then
               eprintf "Simple_print.format_term: got a sequent%t" eflush;
            let seq = explode_sequent term in
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec format_hyps i =
               if i <> len then
                  begin
                     format_char buf '\n';
                     format_int buf (i + 1);
                     format_char buf '.';
                     format_pushm buf 1;
                     begin
                        match SeqHyp.get hyps i with
                           Hypothesis (v, t) ->
                              if Lm_symbol.to_string v <> "" then
                                 begin
                                    format_string buf (dstring_of_var v);
                                    format_string buf " :";
                                    format_pushm buf 1;
                                 end
                              else
                                 format_pushm buf 0;
                              format_term buf t;
                              format_popm buf
                         | Context (v, conts, ts) ->
                              format_string buf ("<" ^ (dstring_of_var v));
                              if conts <> [v] then
                                 format_contexts buf conts;
                              format_terms buf ts;
                              format_string buf (">");
                     end;
                     format_popm buf;
                     format_hyps (succ i)
                  end
            in
               format_string buf "Arg: ";
               format_pushm buf 2;
               format_term buf seq.sequent_args;
               format_popm buf;
               format_string buf "\nHyps:";
               format_pushm buf 1;
               format_hyps 0;
               format_popm buf;
               format_string buf "\nGoal:";
               format_pushm buf 1;
               format_term buf seq.sequent_concl;
               format_popm buf
         end
      else
         begin
         (* Standard term *)
            if !debug_simple_print then
               eprintf "Simple_print.format_term: regular term%t" eflush;
            let { term_op = op; term_terms = bterms } = dest_term term in
            let { op_name = name; op_params = params } = dest_op op in
               if !debug_simple_print then
                  eprintf "Simple_print.format_term: destructed term%t" eflush;
               format_pushm buf 4;
               format_quoted_string buf (string_of_opname name);
               format_params buf params;
               format_bterms buf bterms;
               format_popm buf
         end

   (*
    * List of terms.
    *)
   and format_terms buf = function
      [] -> ()
    | _::_ as subterms ->
         format_pushm buf 1;
         format_string buf "[";
         format_termlist buf subterms;
         format_string buf "]";
         format_popm buf

   and format_termlist buf = function
      [] -> ()
    | [h] -> format_term buf h
    | h::t ->
         format_term buf h;
         format_string buf ";";
         format_space buf;
         format_termlist buf t

   (*
    * MetaTerms.
    *)
   let rec format_mterm buf = function
      MetaTheorem t ->
         format_term buf t
    | MetaImplies (a, b) ->
         format_szone buf;
         format_pushm buf 0;
         format_mterm buf a;
         format_string buf " -->";
         format_hspace buf;
         format_mterm buf b;
         format_popm buf;
         format_ezone buf
    | MetaFunction (v, a, b) ->
         format_szone buf;
         format_pushm buf 0;
         format_term buf v;
         format_string buf " : ";
         format_mterm buf a;
         format_string buf " -->";
         format_hspace buf;
         format_mterm buf b;
         format_popm buf;
         format_ezone buf
    | MetaIff (a, b) ->
         format_szone buf;
         format_pushm buf 0;
         format_mterm buf a;
         format_string buf " <-->";
         format_hspace buf;
         format_mterm buf b;
         format_popm buf;
         format_ezone buf
    | MetaLabeled (l, t) ->
         format_szone buf;
         format_pushm buf 0;
         format_string buf ( " [\"" ^ l ^"\"]" );
         format_mterm buf t;
         format_popm buf;
         format_ezone buf

   (************************************************************************
    * INTERFACE                                                            *
    ************************************************************************)

   (* Level_Exps *)
   let format_simple_level_exp = format_level_exp

   let print_simple_level_exp_fp out p =
      let buf = new_buffer () in
         format_level_exp buf p;
         output_rbuffer out buf

   let print_simple_level_exp = print_simple_level_exp_fp stdout

   let prerr_simple_level_exp = print_simple_level_exp_fp stderr

   let string_of_level_exp p =
      let buf = new_buffer () in
         format_level_exp buf p;
         print_text_string default_width buf

   (* Params *)
   let format_simple_param = format_param

   let print_simple_param_fp out p =
      let buf = new_buffer () in
         format_param buf p;
         output_rbuffer out buf

   let print_simple_param = print_simple_param_fp stdout

   let prerr_simple_param = print_simple_param_fp stderr

   let string_of_param p =
      let buf = new_buffer () in
         format_param buf p;
         output_rbuffer stdstr buf;
         flush_stdstr ()

   (* Terms *)
   let format_simple_term = format_term

   let print_simple_term_fp out term =
      let buf = new_buffer () in
         format_term buf term;
         output_rbuffer out buf

   let print_simple_term = print_simple_term_fp stdout

   let prerr_simple_term = print_simple_term_fp stderr

   let string_of_term term =
      let buf = new_buffer () in
         format_term buf term;
         output_rbuffer stdstr buf;
         flush_stdstr ()

   let short_string_of_term term =
      line_format default_width ( fun buf -> format_term buf term)

   (* Terms *)
   let format_simple_bterm buf = format_bterm buf

   let print_simple_bterm_fp out term =
      let buf = new_buffer () in
         format_bterm buf term;
         output_rbuffer out buf

   let print_simple_bterm = print_simple_bterm_fp stdout

   let prerr_simple_bterm = print_simple_bterm_fp stderr

   let string_of_bterm term =
      let buf = new_buffer () in
         format_bterm buf term;
         output_rbuffer stdstr buf;
         flush_stdstr ()

   (*
    * MetaTerms.
    *)
   let format_simple_mterm = format_mterm

   let print_simple_mterm_fp out mterm =
      let buf = new_buffer () in
         format_mterm buf mterm;
         output_rbuffer out buf

   let print_simple_mterm = print_simple_mterm_fp stdout

   let prerr_simple_mterm = print_simple_mterm_fp stderr

   let string_of_mterm mterm =
      let buf = new_buffer () in
         format_mterm buf mterm;
         print_text_string default_width buf

   (*
    * Addresses.
    *)
   let print_simple_address_fp out address =
      output_string out (string_of_address address)

   let print_simple_address = print_simple_address_fp stdout
   let prerr_simple_address = print_simple_address_fp stderr

   (*
    * Install simple printer as default printer.
    *)
   let () = install_debug_printer print_simple_term_fp
end

module SimplePrint = MakeSimplePrint (Refiner.Refiner)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

