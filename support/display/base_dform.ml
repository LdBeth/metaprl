(*
 * @begin[doc]
 * @module[Dform]
 *
 * The @hrefmodule[Dform] module implements basic display forms for
 * variables and sequents.
 *
 * @docoff
 * @end[doc]
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@cs.cornell.edu}
 *
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

open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMan
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Dform
open Lm_rformat

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Base_dform%t"

let debug_dform = load_debug "dform"

(* @terms *)
declare df_var[var:v]
declare df_free_fo_var[var:v]
declare df_so_var[var:v]{'conts;'termlist}
declare df_context_var[name:v]
declare "sequent"{'arg; 'seq}
declare bvar{'v}
declare " "
declare "^"
declare "_"
declare "{"
declare "}"
declare "$"
declare "["
declare "]"
declare ";"
declare "\\"

(*
 * List helper operations.
 *)
declare df_length{'l}
declare df_last{'l}
declare df_concat{'sep;'l}
declare df_rev_concat{'sep;'l}
declare df_down{'l}

(* same as "szone 'e ezone" *)
declare szone{'e}
(* @docoff *)

(*
 * @begin[doc]
 * Variables are terms with the opname @tt[var], and a single @emph[var]
 * parameter.  @emph{Second-order} variables also have (unbound) subterms
 * that correspond to the free variables in the term being represented (in
 * a redex), or terms to be substituted for those variables (in a contractum).
 *
 * Display for mechanism would convert the variable term into a @tt[df_so_var]
 * term to avoid having to deal with argument lists of arbitrary length.
 *
 * The @tt[tex] mode display form for @tt[df_so_var] uses some heuristics to split
 * the variable name into the name and the subscript part and is omitted from the
 * documentation.
 * @end[doc]
 *)
declare var_list{'t}
declare df_bconts{'conts}

dform var_prl_df : df_so_var[v:v]{cons{df_context_var[v:v]; nil}; nil} =
   df_var[v:v]

dform var_prl_df : mode[prl] :: df_var[v:v] =
   slot[v:v]

dform var_src_df : mode[src] :: df_var[v:v] =
   `"'" slot[v:v]

dform free_var_src_df : mode[src] :: df_free_fo_var[v:v] =
   `"!" slot[v:v]

dform free_var_df : except_mode[src] :: df_free_fo_var[v:v] =
   `"!" df_var[v:v]

dform so_var1 : df_so_var[v:v]{cons{df_context_var[v:v];nil};'t} =
   szone df_var[v:v] `"[" pushm[0] var_list{'t} popm `"]" ezone

dform so_var2 : df_so_var[v:v]{'conts;nil} =
   szone df_var[v:v] df_bconts{'conts} `"[]" ezone

dform so_var3 : df_so_var[v:v]{'conts;'t} =
   szone df_var[v:v] df_bconts{'conts} `"[" pushm[0] var_list{'t} popm `"]" ezone

dform conts_left_df : mode[src] :: mode[prl] :: mode[html] :: df_bconts{'conts} =
   `"<|" df_concat{slot[";"]; 'conts} `"|>"

dform conts_left_df : mode[tex] :: df_bconts{'conts} =
   izone `"{}_{" ezone
   <<mathmacro["left<"]>> `"|" df_concat{slot[";"]; 'conts} `"|" <<mathmacro["right>"]>>
   izone `"}" ezone

dform var_list_df1 : var_list{cons{'a;'b}} =
   'a `";" hspace var_list{'b}

dform var_list_df2 : var_list{cons{'a;nil}} =
   'a

(* @docoff *)

let split_digits s =
   let rec aux i =
      if (i=0) then 0 else
         let i' = pred i in
         if Lm_ctype.is_digit(s.[i']) then aux i' else i
   in
      let len = String.length s in
      let i = aux len in
      String.sub s 0 i, String.sub s i (len-i)

let split_var v =
   match Lm_string_util.split "_" v with
      [] ->
         raise (Invalid_argument "var_*_df: string has an empty name")
    | h::tl ->
         if List.for_all (Lm_string_util.for_all Lm_ctype.is_digit) tl then
            let hn, hd = split_digits h in
               if hn <> "" && hd <> "" then
                  hn, hd::tl
               else
                  h, tl
         else
               h, tl

let print_html_var format_term buf header_fun v =
   let h, tl = split_var v in
      format_izone buf;
      format_string buf "<span class=\"var\">";
      format_ezone buf;
      format_term buf NOParens (header_fun h);
      format_izone buf;
      if tl <> [] then
         begin
            format_string buf "<sub>";
            format_ezone buf;
            format_string buf (String.concat "," tl);
            format_izone buf;
            format_string buf "</sub>";
         end;
      format_string buf "</span>";
      format_ezone buf

let print_tex_var format_term buf header_fun v =
   let h, tl = split_var v in
      format_izone buf;
      format_string buf "\\ensuremath{";
      format_ezone buf;
      format_term buf NOParens (header_fun h);
      format_izone buf;
      if tl <> [] then begin
         format_string buf "_{";
         format_ezone buf;
         format_string buf (String.concat "," tl);
         format_izone buf;
      format_string buf "}";
      end;
      format_string buf "}";
      format_ezone buf

let mk_slot s = <:con< slot[$s$:s] >>
let mk_mathit s = <:con< math_it[$s$:s] >>

ml_dform var_html_df : mode[html] :: df_var[v:v] format_term buf = fun _ ->
   print_html_var format_term buf mk_slot (string_of_symbol v)

ml_dform var_tex_df : mode[tex] :: df_var[v:v] format_term buf = fun _ ->
   print_tex_var format_term buf mk_mathit (string_of_symbol v)

dform cvar_src_df : mode[src] :: df_context_var[v:v] =
   slot[v:v]

ml_dform cvar_prl_df : mode[prl] :: df_context_var[v:v] format_term buf = fun
   term ->
      let v = string_of_symbol v in
      if v = "" then raise (Invalid_argument "var_prl_df");
      begin match v.[0] with
         'H' -> format_term buf NOParens <<Gamma>>
       | 'J' -> format_term buf NOParens <<Delta>>
       | 'K' -> format_term buf NOParens <<Sigma>>
       |  c  -> format_char buf c
      end;
      format_string buf (String.sub v 1 (String.length v - 1))

let context_term = function
   "H" -> <<Gamma>>
 | "J" -> <<Delta>>
 | "K" -> <<Sigma>>
 |  v  -> mk_mathit v

ml_dform cvar_html_df : mode[html] :: df_context_var[v:v] format_term buf = fun
   term ->
      print_html_var format_term buf context_term (string_of_symbol v)

ml_dform cvar_tex_df : mode[tex] :: df_context_var[v:v] format_term buf = fun
   term ->
      print_tex_var format_term buf context_term (string_of_symbol v)

ml_dform bvar_df : mode[src] :: bvar{'v} format_term buf = fun
   term ->
      if is_var_term v then
         format_string buf (string_of_symbol (dest_var v))
      else
         format_term buf LTParens v

dform bvar_df : except_mode[src] :: bvar{'v} = 'v

(*
 * Rewriting.
 *)
dform rewrite_df2 : "rewrite"{'redex; 'contractum} =
   szone pushm[0] szone slot{'redex} ezone hspace longleftrightarrow hspace szone slot{'contractum} ezone popm ezone

let rec fmt_term_lst format_term buf = function
   [] ->
      raise(Invalid_argument("fmt_term_lst"))
 | [t] ->
      format_term buf NOParens t
 | t::tl ->
      format_term buf NOParens t;
      format_string buf "; ";
      fmt_term_lst format_term buf tl

let format_term_list format_term buf = function
   [] -> ()
 | ts ->
      format_szone buf;
      format_string buf "[";
      format_pushm buf 0;
      fmt_term_lst format_term buf ts;
      format_popm buf;
      format_string buf "]";
      format_ezone buf

let make_cont v = <:con< df_context_var[$v$:v] >>

let format_bconts format_term buf v = function
   [v'] when Lm_symbol.eq v v' -> ()
 | conts -> format_term buf NOParens <:con< df_bconts{$mk_xlist_term (List.map make_cont conts)$} >>

(*
 * For sequents.
 * In the "format" function,
 *    i is the hyp number, if it is known
 *    cflag is true if the last term was a conclusion
 *    t is the term to be printed.
 *)
ml_dform sequent_src_df : mode["src"] :: sequent ('ext) { <H> >- 'concl } format_term buf =
   let rec format_goal goals i len =
      if i <> len then
         begin
            format_string buf (if i = 0 then " >-" else ";");
            format_space buf;
            format_term buf NOParens (SeqGoal.get goals i);
            format_goal goals (succ i) len
         end
   in
   let rec format_hyp hyps i len =
      if i <> len then
         let _ =
            if i <> 0 then
               format_string buf ";"
         in
         let _ =
            match SeqHyp.get hyps i with
               Hypothesis (v, a) ->
                  format_space buf;
                  if Lm_symbol.to_string v <> "" then begin
                     format_string buf (string_of_symbol v);
                     format_string buf ": ";
                  end;
                  format_term buf NOParens a
             | Context (v, conts, values) ->
                  format_space buf;
                  format_string buf ("<" ^ string_of_symbol v);
                  format_bconts format_term buf v conts;
                  format_term_list format_term buf values;
                  format_string buf ">"
         in
            format_hyp hyps (succ i) len
   in
   let format term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
         format_szone buf;
         format_pushm buf 0;
         format_string buf "sequent (";
         format_term buf NOParens arg;
         format_string buf ") {";
         format_hyp hyps 0 (SeqHyp.length hyps);
         format_goal goals 0 (SeqGoal.length goals);
         format_string buf " }";
         format_popm buf;
         format_ezone buf
   in
      format

let format_context format_term buf v conts values =
   format_term buf NOParens <:con< df_context_var[$v$:v] >>;
   format_bconts format_term buf v conts;
   format_term_list format_term buf values

(*
 * @begin[doc]
 * The refiner uses a special representation for sequents that requires the
 * display form to be implemented in ML.
 * @end[doc]
 *)
ml_dform sequent_prl_df : mode["prl"] :: sequent ('ext) { <H> >- 'concl } format_term buf =
   let rec format_hyp hyps i len =
      if i <> len then
         let lead = (string_of_int (succ i)) ^ ". " in
         let _ =
            format_term buf NOParens <<pushfont["bf"]>>;
            if i = 0 then
               format_hbreak buf lead ""
            else
               format_hbreak buf lead "; ";
            format_term buf NOParens <<popfont["bf"]>>;
            match SeqHyp.get hyps i with
               Context (v, conts, values) ->
                  (* This is a context hypothesis *)
                  format_string buf "<"; (* note: U27E8 is much nicer, but not all fonts have it *)
                  format_context format_term buf v conts values;
                  format_string buf ">"; (* note: U27E9 is much nicer, but not all fonts have it *)
             | Hypothesis (v, a) ->
                  format_szone buf;
                  format_pushm buf 0;
                  if Lm_symbol.to_string v <> "" then begin
                     format_term buf NOParens (mk_var_term v);
                     format_string buf ":";
                     format_space buf;
                  end;
                  format_term buf NOParens a;
                  format_popm buf;
                  format_ezone buf
         in
            format_hyp hyps (succ i) len
   in
   let rec format_goal goals i len =
      if i < len then
         let a = SeqGoal.get goals i in
            if i > 0 then format_hbreak buf "; " "";
            format_term buf NOParens a;
            format_goal goals (succ i) len
   in
   let format term =
      let { sequent_args = args;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
         format_szone buf;
         format_pushm buf 0;
         let hlen = SeqHyp.length hyps in
         if (hlen>0) then begin
            format_hyp hyps 0 hlen;
            format_hbreak buf "" " ";
         end;
         format_term buf NOParens <<bf{vdash}>>;
         format_term buf NOParens args;
         format_string buf " ";
         format_pushm buf 0;
         format_goal goals 0 (SeqGoal.length goals);
         format_popm buf;
         format_popm buf;
         format_ezone buf
   in
      format

ml_dform sequent_html_df : mode["html"] :: sequent ('ext) { <H> >- 'concl } format_term buf =
   let rec format_hyp hyps i len =
      if i <> len then
         let lead = (string_of_int (succ i)) ^ ". " in
         let _ =
            if i = 0 then
               format_hbreak buf lead ""
            else
               format_hbreak buf lead "; ";
            match SeqHyp.get hyps i with
               Context (v, conts, values) ->
                  (* This is a context hypothesis *)
                  format_string buf "<";
                  format_context format_term buf v conts values;
                  format_string buf ">"
             | Hypothesis (v, a) ->
                  format_szone buf;
                  if Lm_symbol.to_string v <> "" then begin
                     format_term buf NOParens (mk_var_term v);
                     format_string buf ":";
                     format_space buf;
                  end;
                  format_term buf NOParens a;
                  format_ezone buf
         in
            format_hyp hyps (succ i) len
   in
   let rec format_goal goals i len =
      if i <> len then
         let a = SeqGoal.get goals i in
            if i > 0 then
               format_hbreak buf "; " "  ";
            format_term buf NOParens a;
            format_goal goals (succ i) len
   in
   let format term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
         format_szone buf;
         format_pushm buf 0;
         let hlen = SeqHyp.length hyps in
         if (hlen>0) then begin
            format_hyp hyps 0 hlen;
            format_hspace buf
         end;
         format_term buf NOParens <<Nuprl_font!vdash>>;
         format_term buf NOParens arg;
         format_string buf " ";
         format_pushm buf 0;
         format_goal goals 0 (SeqGoal.length goals);
         format_popm buf;
         format_popm buf;
         format_ezone buf
   in
      format

ml_dform sequent_tex_df : mode["tex"] :: sequent ('ext) { <H> >- 'concl } format_term buf =
   let rec format_hyp hyps i len =
      if i <> len then
         let lead = (string_of_int (succ i)) ^ ". " in
         let _ =
            if i = 0 then
               format_hbreak buf lead ""
            else
               format_hbreak buf lead "; ";
            match SeqHyp.get hyps i with
               Context (v, conts, values) ->
                  format_term buf NOParens <<mathmacro["left<"]>>;
                  format_context format_term buf v conts values;
                  format_term buf NOParens <<mathmacro["right>"]>>
             | Hypothesis (v, a) ->
                  format_szone buf;
                  if Lm_symbol.to_string v <> "" then begin
                     format_term buf NOParens (mk_var_term v);
                     format_string buf ":";
                     format_space buf;
                  end;
                  format_term buf NOParens a;
                  format_ezone buf
         in
            format_hyp hyps (succ i) len
   in
   let rec format_goal goals i len =
      if i <> len then
         let a = SeqGoal.get goals i in
            if i > 0 then
               format_hbreak buf "; " "";
            format_term buf NOParens a;
            format_goal goals (succ i) len
   in
   let format term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
         format_szone buf;
         format_pushm buf 0;
         let hlen = SeqHyp.length hyps in
         if (hlen>0) then begin
            format_hyp hyps 0 hlen;
            format_hspace buf;
         end;
         format_term buf NOParens <<mathmacro["vdash"]>>;
         format_term buf NOParens arg;
         format_izone buf;
         format_string buf "\\,";
         format_ezone buf;
         format_pushm buf 0;
         format_space buf;
         format_goal goals 0 (SeqGoal.length goals);
         format_popm buf;
         format_popm buf;
         format_ezone buf
   in
      format

(*
 * This is a convenient way to print a number.
 *)
ml_dform df_length_df : internal :: df_length{'l} format_term buf = fun term ->
   try
      format_int buf (List.length (dest_xlist (one_subterm term)))
   with
      RefineError _ ->
         format_string buf "???"

(*
 * This is a convenient way to print a number.
 * This changes to the numbered directory.
 *)
ml_dform df_down_df : internal :: mode[html] :: df_down{'l} format_term buf = fun term ->
   try
      let length = List.length (dest_xlist (one_subterm term)) in
         format_izone buf;
         format_string buf "<a href=\"";
         format_int buf length;
         format_string buf "/\">";
         format_ezone buf;
         format_int buf length;
         format_izone buf;
         format_string buf "</a>";
         format_ezone buf
   with
      RefineError _ ->
         format_string buf "???"

ml_dform df_down_df : internal :: except_mode[html] :: df_down{'l} format_term buf = fun term ->
   try
      let length = List.length (dest_xlist (one_subterm term)) in
         format_int buf length
   with
      RefineError _ ->
         format_string buf "???"

(*
 * Get the last item in a list.
 *)
dform df_last_df1 : internal :: df_last{cons{'a; cons{'b; 'c}}} =
   df_last{cons{'b; 'c}}

dform df_last_df2 : internal :: df_last{cons{'a; nil}} =
   'a

(*
 * List concatenation
 *)
dform df_concat_cons : internal :: df_concat{'sep; cons{'hd; 'tl}} =
   slot{'hd} slot{'sep} df_concat{'sep;'tl}

dform df_concat_nil : internal :: df_concat{'sep; cons{'hd; nil}} =
   slot{'hd}

dform df_concat_nil2 : internal :: df_concat{'sep; nil} =
   `""

(*
 * List rev_concatenation
 *)
dform df_rev_concat_cons : internal :: df_rev_concat{'sep; cons{'hd; 'tl}} =
   df_rev_concat{'sep; 'tl} slot{'sep} slot{'hd}

dform df_rev_concat_nil : internal :: df_rev_concat{'sep; cons{'hd; nil}} =
   slot{'hd}

dform df_rev_concat_nil2 : internal :: df_rev_concat{'sep; nil} =
   `""

(*
 * Perv!bind
 *)
dform bind_df : except_mode[src] :: bind{x. 'T} =
   tt["bind"] `"(" slot[x] `"." slot{'T} `")"

(************************************************************************
 * COMMANDS                                                             *
 ************************************************************************)

dform space_df : internal :: " " = `" "
dform hat_df : internal :: "^" = `"^"
dform underscore_df : internal :: "_" = `"_"
dform left_curly_df : internal :: "{" = `"{"
dform right_curly_df : internal :: "}" = `"}"
dform dollar_df : internal :: "$" = `"$"
dform left_brack_df : internal :: "[" = `"["
dform right_brack_df : internal :: "]" = `"]"
dform semicolon_df : internal :: ";" = `";"
dform newline_df : internal :: "\\" = \newline

dform szone_df : internal :: szone{'e} =
   szone slot{'e} ezone

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
