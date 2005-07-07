(*
 * @module[Mpfont]
 *
 * The @hrefmodule[Mpfont] module defines display forms for
 * the fonts used by the @MetaPRL editor.
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
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2005 MetaPRL Group
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
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * Modified By: Alexei Kopylov @email{kopylov@cs.cornell.edu}
 * Modified By: Xin Yu @email{xiny@cs.caltech.edu}
 *
 * @end[license]
 *)

open Lm_debug

open Lm_ctype
open Refiner.Refiner.TermOp
open Lm_rformat

extends Perv

(************************************************************************
 * INTERNAL TERMS                                                       *
 ************************************************************************)

(*
 * Fonts.
 * Each of these can be used with a string, with a term,
 * or unbalanced.
 *
 * For example, all of these are the same:
 *    bf["["]
 *    bf{"["}
 *    bf_begin "[" bf_end
 *)
declare info{'t : Dform} : Dform
declare keyword{'t : Dform} : Dform
declare keyword_begin : Dform
declare keyword_end : Dform
declare bf_begin : Dform
declare bf_end : Dform
declare it_begin : Dform
declare it_end : Dform
declare em_begin : Dform
declare em_end : Dform
declare tt_begin : Dform
declare tt_end : Dform
declare small_begin : Dform
declare small_end : Dform

(************************************************************************
 * DISPLAY CONTROL                                                      *
 ************************************************************************)

(*
 * Pagebreak works only in prl mode.
 *)
dform pagebreak_df1 : except_mode[prl] :: pagebreak = `""

dform pagebreak_df2 : mode[prl] :: pagebreak =
   `"\012"

(************************************************************************
 * HTML                                                                 *
 ************************************************************************)

(*
 * Print the string in invis mode.
 *)
dform html_df1 : mode[html] :: html[content:s] =
   izone slot[content:s] ezone

dform html_df2 : except_mode[html] :: html[content:s] =
   `""

(*
 * Change directory.
 *)
dform cd_begin_df1 : except_mode[html] :: cd_begin[name:s] =
   `""

dform cd_begin_df2_java : mode[java] :: cd_begin[name:s] =
   izone `"<a href=\"http://cd.metaprl.local/" slot[name:s] `"\">" ezone

dform cd_begin_df2_html : mode[html] :: cd_begin[name:s] =
   izone `"<a href=\"" slot[name:s] `"/\">" ezone

dform cd_end_df1 : except_mode[html] :: cd_end =
   `""

dform cd_end_df2 : mode[html] :: cd_end =
   izone `"</a>" ezone

(*
 * Anchor. XXX BUG (nogin): the anchor name is invisible, but need to be quoted.
 * Currently, there is no way (?) to specify this naturally.
 *)
dform html_anchor_df : mode[html] :: html_anchor[name:s]{'body} =
   izone `"<a name=\"" ezone slot[name:s] izone `"\">" ezone 'body izone `"</a>" ezone

dform html_head_df : mode[html] :: html_head[l:n]{'body} =
   izone `"<h" slot[l:n] `">" ezone 'body izone `"</h" slot[l:n] `">" ezone

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

dform text_df : text{'e} =
   'e

dform info_begin_df : mode[html] :: info_begin =
   html["<span class=\"info\">"]

dform info_end_df : mode[html] :: info_end =
   html["</span>"]

dform info_df1 : info[text:s] =
   info_begin slot[text:s] info_end

dform info_df2 : info{'t} =
   info_begin 't info_end

dform keyword_begin_df : mode[html] :: keyword_begin =
   html["<span class=\"keyword\">"]

dform keyword_end_df : mode[html] :: keyword_end =
   html["</span>"]

dform info_begin_df_all : mode [src] :: info_begin = `""
dform info_end_df_all : mode[src] :: info_end = `""

dform keyword_begin_df_all : except_mode[html] :: except_mode[tex] :: keyword_begin =
   bf_begin

dform keyword_end_df_all : except_mode[html] :: except_mode[tex] :: keyword_end =
   bf_end

dform keyword_df1 : except_mode[tex] :: keyword[text:s] =
   keyword_begin slot[text:s] keyword_end

dform keyword_df2 : keyword{'t} =
   keyword_begin 't keyword_end

dform bf_begin_df : mode[html] :: bf_begin =
   html["<b>"]

dform bf_end_df : mode[html] :: bf_end =
   html["</b>"]

dform bf_begin_df : mode[prl] :: bf_begin =
   pushfont["bf"]

dform bf_end_df : mode[prl] :: bf_end =
   popfont

dform bf_df1 : bf[text:s] =
   bf_begin slot[text:s] bf_end

dform bf_df2 : bf{'t} =
   bf_begin 't bf_end

dform monospaced_begin_df : mode[html] :: monospaced_begin =
   html["<span class=\"monospaced\">"]

dform monospaced_end_df : mode[html] :: monospaced_end =
   html["</span>"]

dform monospaced_begin_df : mode[prl] :: monospaced_begin =
   pushfont["monospaced"]

dform monospaced_end_df : mode[prl] :: monospaced_end =
   popfont

dform monospaced_df1 : monospaced[text:s] =
   monospaced_begin slot[text:s] monospaced_end

dform monospaced_df2 : monospaced{'t} =
   monospaced_begin 't monospaced_end

dform it_begin_df : mode[html] :: it_begin =
   html["<i>"]

dform it_end_df : mode[html] :: it_end =
   html["</i>"]

dform it_begin_df : mode[prl] :: it_begin =
   pushfont["it"]

dform it_end_df : mode[prl] :: it_end =
   popfont

dform it_df1 : it[text:s] =
   it_begin slot[text:s] it_end

dform it_df2 : it{'t} =
   it_begin 't it_end

dform i_df1 : i[text:s] =
   it[text:s]

dform i_df2 : i{'t} =
   it{'t}

dform math_it_df2 : except_mode[tex] :: math_it{'t} =
   it{'t}

dform math_it_df2 : except_mode[tex] :: math_it[text:s] =
   it[text:s]

dform em_begin_df : mode[html] :: em_begin =
   html["<em>"]

dform em_end_df : mode[html] :: em_end =
   html["</em>"]

dform em_begin_df : mode[prl] :: em_begin =
   `""

dform em_end_df : mode[prl] :: em_end =
   `""

dform em_df1 : em[text:s] =
   em_begin slot[text:s] em_end

dform em_df2 : em{'t} =
   em_begin 't em_end

dform emph_df1 : emph{'t} =
   em_begin 't em_end

dform tt_begin_df : mode[html] :: tt_begin =
   html["<tt>"]

dform tt_end_df : mode[html] :: tt_end =
   html["</tt>"]

dform tt_begin_df : mode[prl] :: tt_begin =
   `""

dform tt_end_df : mode[prl] :: tt_end =
   `""

dform tt_df1 : tt[text:s] =
   tt_begin slot[text:s] tt_end

dform tt_df2 : tt{'t} =
   tt_begin 't tt_end

dform url_df1 : mode[html] :: url[url:s] =
   izone `"<a href=\"" slot[url:s] `"\"><tt>" ezone slot[url:s] izone `"</tt></a>" ezone

dform url_df2 : mode[tex] :: url[url:s] =
   izone `"\\url{" ezone slot[url:s] izone `"}" ezone

dform url_df3 : mode[prl] :: mode [src] :: url[url:s] =
   `"@url[" slot[url:s] `"]"

dform sub_begin_df : mode[html] :: sub_begin =
   html["<sub>"]

dform sub_end_df : mode[html] :: sub_end =
   html["</sub>"]

dform sub_df1 : sub[text:s] =
   sub_begin slot[text:s] sub_end

dform sub_df2 : sub{'t} =
   sub_begin 't sub_end

dform sub_df3 : sub[i:l] =
   sub_begin slot[i:l] sub_end

dform sup_begin_df : mode[html] :: sup_begin =
   html["<sup>"]

dform sup_end_df : mode[html] :: sup_end =
   html["</sup>"]

dform sup_df1 : sup[text:s] =
   sup_begin slot[text:s] sup_end

dform sup_df2 : sup{'t} =
   sup_begin 't sup_end

dform sub_begin_df : mode[prl] :: sub_begin =
   `"_"

dform sub_end_df : mode[prl] :: sub_end =
   `""

dform sup_begin_df : mode[prl] :: sup_begin =
   `"^"

dform sup_end_df : mode[prl] :: sup_end =
   `""

dform small_begin_df : mode[html] :: small_begin =
   html["<small>"]

dform small_end_df : mode[html] :: small_end =
   html["</small>"]

dform small_df1 : small[text:s] =
   small_begin slot[text:s] small_end

dform small_df2 : small{'t} =
   small_begin 't small_end

dform underline_df1 : underline{'t} =
   underline_begin 't underline_end

dform underline_begin_prl : mode[prl] :: underline_begin = pushfont["ul"]

dform underline_end_prl : mode[prl] :: underline_end = popfont

dform underline_begin_html : mode[html] :: underline_begin = html["<ul>"]

dform underline_end_html : mode[html] :: underline_end = html["</ul>"]

dform underline_begin_tex : mode[tex] :: underline_begin =
   izone `"\\underline{" ezone

dform underline_end_tex : mode[tex] :: underline_end =
   izone `"}" ezone

(************************************************************************
 * TEX HELPERS                                                          *
 ************************************************************************)

dform ensuremath_df0 : except_mode[tex] :: ensuremath{'t} =
   't

dform ensuremath_df1 : ensuremath[text:s] =
   ensuremath{slot[text:s]}

dform ensuremath_df2 : mode[tex] :: ensuremath{'t} =
   tzone["ensuremath"] slot{'t} ezone

dform ensuremath_xcons_df : mode[tex] :: xcons{ensuremath{'t1}; ensuremath{'t2}} =
   ensuremath{xcons{'t1;'t2}}

dform mathBB_df : mode[tex] :: mathBB[text:s] =
   izone `"\\mathbb{" ezone slot[text:s] izone `"}" ezone

dform mathmacro_df : mode[tex] :: mathmacro[text:s] =
   izone `"\\" slot[text:s] `" " ezone

dform info_begin_df : mode[tex] :: info_begin =
   izone `"\\textbf{" ezone

dform info_end_df : mode[tex] :: info_end =
   izone `"}" ezone

dform info_begin_df : mode[prl] :: info_begin =
   pushfont["bf"]

dform info_end_df : mode[prl] :: info_end =
   popfont

let not_alnum c = not (is_alnum c)

ml_dform keyword_tex_df : mode[tex] :: keyword[text:s] format_term buf =
   fun term ->
     let text = dest_string_param term in
     let font = if Lm_string_util.for_all not_alnum text then "tt" else "bf" in
        format_izone buf;
        format_string buf ("\\mbox{\\" ^ font ^ " ");
        format_ezone buf;
        format_string buf text;
        format_izone buf;
        format_string buf "}";
        format_ezone buf

dform keyword_begin_df : mode[tex] :: keyword_begin =
   izone `"\\textbf{" ezone

dform keyword_end_df : mode[tex] :: keyword_end =
   izone `"}" ezone

dform bf_begin_df : mode[tex] :: bf_begin =
   tzone["textbf"]

dform bf_end_df : mode[tex] :: bf_end =
   ezone

dform it_begin_df : mode[tex] :: it_begin =
   tzone["textit"]

dform it_end_df : mode[tex] :: it_end =
   ezone

dform math_it_df1 : mode[tex] :: math_it{'t} =
   izone `"\\mathit{" ezone slot{'t} izone `"}" ezone

dform math_it_df1 : mode[tex] :: math_it[text:s] =
   izone `"\\mathit{" ezone slot[text:s] izone `"}" ezone

dform em_begin_df : mode[tex] :: em_begin =
   tzone["emph"]

dform em_end_df : mode[tex] :: em_end =
   ezone

dform tt_begin_df : mode[tex] :: tt_begin =
   tzone["texttt"]

dform tt_end_df : mode[tex] :: tt_end =
   ezone

dform sub_begin_df : mode[tex] :: sub_begin =
   izone `"{}_{" ezone

dform sub_end_df : mode[tex] :: sub_end =
   izone `"}" ezone

dform sup_begin_df : mode[tex] :: sup_begin =
   izone `"{}^{" ezone

dform sup_end_df : mode[tex] :: sup_end =
   izone `"}" ezone

dform small_begin_df : mode[tex] :: small_begin =
   izone `"{\\scriptsize " ezone

dform small_end_df : mode[tex] :: small_end =
   izone `"}" ezone

