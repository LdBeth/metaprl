(*
 * @begin[doc]
 * @module[Nuprl_font]
 *
 * The @hrefmodule[Nuprl_font] module defines display forms for the
 * special characters used by the @MetaPRL editor.
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
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * Modified By: Alexei Kopylov @email{kopylov@cs.cornell.edu}
 * Modified By: Xin Yu @email{xiny@cs.caltech.edu}
 *
 * @end[license]
 *)

open Lm_debug
open Lm_printf

open Lm_ctype
open Refiner.Refiner.TermOp
open Lm_rformat

extends Perv

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Nuprl_font%t"

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

(*
 * Display control.
 *)
declare pagebreak

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
declare info[name:s]
declare info{'t}
declare info_begin
declare info_end
declare keyword[name:s]
declare keyword{'t}
declare keyword_begin
declare keyword_end
declare bf[name:s]
declare bf{'t}
declare bf_begin
declare bf_end
declare monospaced[name:s]
declare monospaced{'t}
declare monospaced_begin
declare monospaced_end
declare i[name:s]
declare i{'t}
declare it[name:s]
declare it{'t}
declare it_begin
declare it_end
declare math_it{'t}
declare math_it[text:s]
declare html_sym[name:s]
declare html_uni[unicode:n]
declare em[s:s]
declare em{'s}
declare em_begin
declare em_end
declare emph{'t}
declare tt[name:s]
declare tt{'t}
declare tt_begin
declare tt_end
declare url[url:s]
declare sub[name:s]
declare sub{'t}
declare sub[i:l]
declare sub_begin
declare sub_end
declare sup[name:s]
declare sup{'t}
declare sup_begin
declare sup_end
declare small[name:s]
declare small{'t}
declare small_begin
declare small_end
declare esquash{'t}
declare text{'t}
declare atomic[text:s]

(*
 * HTML control.
 *)
declare html[content:s]
declare cd_begin[command:s]
declare cd_end
declare html_anchor[name:s]{'body}
declare html_head[l:n]{'body}

(* Displays *)
declare mathbbA
declare mathbbB
declare mathbbC
declare mathbbD
declare mathbbE
declare mathbbF
declare mathbbG
declare mathbbH
declare mathbbI
declare mathbbJ
declare mathbbK
declare mathbbL
declare mathbbM
declare mathbbN
declare mathbbO
declare mathbbP
declare mathbbQ
declare mathbbR
declare mathbbS
declare mathbbT
declare mathbbU
declare mathbbV
declare mathbbW
declare mathbbX
declare mathbbY
declare mathbbZ

declare shortLeftarrow
declare Leftarrow
declare Middlearrow
declare shortRightarrow
declare Rightarrow
declare Leftrightarrow
declare ulcorner
declare urcorner
declare mid
declare vdash
declare integral
declare cdot
declare downarrow
declare uparrow
declare alpha
declare beta
declare pi
declare lambda
declare gamma
declare delta
declare rho
declare sigma
declare epsilon
declare eta
declare theta
declare iota
declare kappa
declare mu
declare nu
declare omicron
declare tau
declare phi
declare xi
declare omega

declare wedge
declare tneg
declare member
declare plusminus
declare oplus
declare infty
declare partial
declare cap
declare cup
declare forall
declare "exists"
declare oinfty
declare shortleftrightarrow
declare shortleftarrow
declare shortrightarrow
declare longleftrightarrow
declare longleftarrow
declare longrightarrow
declare neq
declare sim
declare cong
declare le
declare ge
declare equiv
declare vee
declare perp
declare esq_l
declare esq_r
declare leftarrow
declare middlearrow
declare rightarrow
declare vartriangleleft
declare vartriangleright
declare Gamma
declare Delta
declare Lambda
declare Pi
declare Sigma
declare Omega
declare times
declare "div"
declare circ
declare supplus
declare supminus
declare supcirc
declare "subset"
declare supset
declare sqsubset
declare sqsupset
declare subseteq
declare supseteq
declare sqsubseteq
declare sqsupseteq
declare subzero
declare subone
declare subtwo
declare subthree
declare suba
declare subb
declare subc
declare subd
declare sube
declare subf
declare subg
declare subh
declare subi
declare subj
declare subk
declare subl
declare subm
declare subn
declare subo
declare subp
declare subq
declare subr
declare subs
declare subt
declare subu
declare subv
declare subw
declare subx
declare suby
declare subz

declare math_div

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

dform atomic_df : atomic[text:s] =
   azone slot[text:s] ezone

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
   popfont["bf"]

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
   popfont["monospaced"]

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
   popfont["it"]

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

(*
 * These are atomic symbols.
 *)
dform html_sym_df :  mode[html] :: html_sym[text:s] =
   azone `"&" slot[text:s] `";" ezone

dform html_uni_df : mode[html] :: html_uni[num:n] =
   azone `"&#" slot[num:n] `";" ezone

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

dform esquash_df : esquash{'t} =
   esq_l 't esq_r

(************************************************************************
 * TEX HELPERS                                                          *
 ************************************************************************)

declare ensuremath[name:s]
declare ensuremath{'t}
declare mathBB[name:s]
declare mathmacro[name:s]

dform ensuremath_df0 : except_mode[tex] :: ensuremath{'t} =
   't

dform ensuremath_df1 : ensuremath[text:s] =
   ensuremath{slot[text:s]}

dform ensuremath_df2 : mode[tex] :: ensuremath{'t} =
   tzone["ensuremath"] slot{'t} ezone

dform ensuremath_cons_df : mode[tex] :: cons{ensuremath{'t1}; ensuremath{'t2}} =
   ensuremath{cons{'t1;'t2}}

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
   popfont["bf"]

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

(************************************************************************
 * NUPRL FONT                                                           *
 ************************************************************************)

(* Displays *)
dform mathbbA_df		: mode[prl] :: mathbbA                   = bf["A"]
dform mathbbB_df		: mode[prl] :: mathbbB                   = bf["B"]
dform mathbbC_df		: mode[prl] :: mathbbC                   = atomic["ℂ"]
dform mathbbD_df		: mode[prl] :: mathbbD                   = bf["D"]
dform mathbbE_df		: mode[prl] :: mathbbE                   = bf["E"]
dform mathbbF_df		: mode[prl] :: mathbbF                   = bf["F"]
dform mathbbG_df		: mode[prl] :: mathbbG                   = bf["G"]
dform mathbbH_df		: mode[prl] :: mathbbH                   = atomic["ℍ"]
dform mathbbI_df		: mode[prl] :: mathbbI                   = bf["I"]
dform mathbbJ_df		: mode[prl] :: mathbbJ                   = bf["J"]
dform mathbbK_df		: mode[prl] :: mathbbK                   = bf["K"]
dform mathbbL_df		: mode[prl] :: mathbbL                   = bf["L"]
dform mathbbM_df		: mode[prl] :: mathbbM                   = bf["M"]
dform mathbbN_df		: mode[prl] :: mathbbN                   = atomic["ℕ"]
dform mathbbO_df		: mode[prl] :: mathbbO                   = bf["O"]
dform mathbbP_df		: mode[prl] :: mathbbP                   = atomic["ℙ"]
dform mathbbQ_df		: mode[prl] :: mathbbQ                   = atomic["ℚ"]
dform mathbbR_df		: mode[prl] :: mathbbR                   = atomic["ℝ"]
dform mathbbS_df		: mode[prl] :: mathbbS                   = bf["S"]
dform mathbbT_df		: mode[prl] :: mathbbT                   = bf["T"]
dform mathbbU_df		: mode[prl] :: mathbbU                   = atomic["Ů"] (* or: `"Ⓤ", or `"U" *)
dform mathbbV_df		: mode[prl] :: mathbbV                   = bf["V"]
dform mathbbW_df		: mode[prl] :: mathbbW                   = bf["W"]
dform mathbbX_df		: mode[prl] :: mathbbX                   = bf["X"]
dform mathbbY_df		: mode[prl] :: mathbbY                   = bf["Y"]
dform mathbbZ_df		: mode[prl] :: mathbbZ                   = atomic["ℤ"]

dform mathbbA_df		: mode[html] :: mathbbA                   = keyword["A"]
dform mathbbB_df		: mode[html] :: mathbbB                   = keyword["B"]
dform mathbbC_df		: mode[html] :: mathbbC                   = html_uni[8450]
dform mathbbD_df		: mode[html] :: mathbbD                   = keyword["D"]
dform mathbbE_df		: mode[html] :: mathbbE                   = keyword["E"]
dform mathbbF_df		: mode[html] :: mathbbF                   = keyword["F"]
dform mathbbG_df		: mode[html] :: mathbbG                   = keyword["G"]
dform mathbbH_df		: mode[html] :: mathbbH                   = html_uni[8461]
dform mathbbI_df		: mode[html] :: mathbbI                   = keyword["I"]
dform mathbbJ_df		: mode[html] :: mathbbJ                   = keyword["J"]
dform mathbbK_df		: mode[html] :: mathbbK                   = keyword["K"]
dform mathbbL_df		: mode[html] :: mathbbL                   = keyword["L"]
dform mathbbM_df		: mode[html] :: mathbbM                   = keyword["M"]
dform mathbbN_df		: mode[html] :: mathbbN                   = html_uni[8469]
dform mathbbO_df		: mode[html] :: mathbbO                   = keyword["O"]
dform mathbbP_df		: mode[html] :: mathbbP                   = html_uni[8473]
dform mathbbQ_df		: mode[html] :: mathbbQ                   = html_uni[8474]
dform mathbbR_df		: mode[html] :: mathbbR                   = html_uni[8477]
dform mathbbS_df		: mode[html] :: mathbbS                   = keyword["S"]
dform mathbbT_df		: mode[html] :: mathbbT                   = keyword["T"]
dform mathbbU_df		: mode[html] :: mathbbU                   = keyword["U"]
dform mathbbV_df		: mode[html] :: mathbbV                   = keyword["V"]
dform mathbbW_df		: mode[html] :: mathbbW                   = keyword["W"]
dform mathbbX_df		: mode[html] :: mathbbX                   = keyword["X"]
dform mathbbY_df		: mode[html] :: mathbbY                   = keyword["Y"]
dform mathbbZ_df		: mode[html] :: mathbbZ                   = html_uni[8484]

dform mathbbA_df		: mode[tex] :: mathbbA                   = mathBB["A"]
dform mathbbB_df		: mode[tex] :: mathbbB                   = mathBB["B"]
dform mathbbC_df		: mode[tex] :: mathbbC                   = mathBB["C"]
dform mathbbD_df		: mode[tex] :: mathbbD                   = mathBB["D"]
dform mathbbE_df		: mode[tex] :: mathbbE                   = mathBB["E"]
dform mathbbF_df		: mode[tex] :: mathbbF                   = mathBB["F"]
dform mathbbG_df		: mode[tex] :: mathbbG                   = mathBB["G"]
dform mathbbH_df		: mode[tex] :: mathbbH                   = mathBB["H"]
dform mathbbI_df		: mode[tex] :: mathbbI                   = mathBB["I"]
dform mathbbJ_df		: mode[tex] :: mathbbJ                   = mathBB["J"]
dform mathbbK_df		: mode[tex] :: mathbbK                   = mathBB["K"]
dform mathbbL_df		: mode[tex] :: mathbbL                   = mathBB["L"]
dform mathbbM_df		: mode[tex] :: mathbbM                   = mathBB["M"]
dform mathbbN_df		: mode[tex] :: mathbbN                   = mathBB["N"]
dform mathbbO_df		: mode[tex] :: mathbbO                   = mathBB["O"]
dform mathbbP_df		: mode[tex] :: mathbbP                   = mathBB["P"]
dform mathbbQ_df		: mode[tex] :: mathbbQ                   = mathBB["Q"]
dform mathbbR_df		: mode[tex] :: mathbbR                   = mathBB["R"]
dform mathbbS_df		: mode[tex] :: mathbbS                   = mathBB["S"]
dform mathbbT_df		: mode[tex] :: mathbbT                   = mathBB["T"]
dform mathbbU_df		: mode[tex] :: mathbbU                   = mathBB["U"]
dform mathbbV_df		: mode[tex] :: mathbbV                   = mathBB["V"]
dform mathbbW_df		: mode[tex] :: mathbbW                   = mathBB["W"]
dform mathbbX_df		: mode[tex] :: mathbbX                   = mathBB["X"]
dform mathbbY_df		: mode[tex] :: mathbbY                   = mathBB["Y"]
dform mathbbZ_df		: mode[tex] :: mathbbZ                   = mathBB["Z"]

dform shortLeftarrow_df		: mode[prl] :: shortLeftarrow            = atomic["⇐"]
dform leftarrow_df		: mode[prl] :: Leftarrow                 = atomic["⇐═"]
dform middlearrow_df		: mode[prl] :: Middlearrow               = atomic["═"]
dform shortRightarrow_df	: mode[prl] :: shortRightarrow           = atomic["⇒"]
dform rightarrow_df		: mode[prl] :: Rightarrow                = atomic["═⇒"]
dform leftrightarrow_df		: mode[prl] :: Leftrightarrow            = atomic["⇐═⇒"]
dform ulcorner_df		: mode[prl] :: ulcorner                  = atomic["⌈"]
dform urcorner_df		: mode[prl] :: urcorner                  = atomic["⌉"]
dform vdash_df                  : mode[prl] :: vdash                     = atomic["⊢"]
dform integral_df		: mode[prl] :: integral                  = atomic["∫"]
dform cdot_df                   : mode[prl] :: cdot                      = atomic["⋅"]
dform downarrow_df		: mode[prl] :: downarrow                 = atomic["↓"]
dform uparrow_df		: mode[prl] :: uparrow                   = atomic["↑"]
dform vartriangleleft_df	: mode[prl] :: vartriangleleft           = atomic["◁"]
dform vartriangleright_df	: mode[prl] :: vartriangleright          = atomic["▷"]
dform alpha_df                  : mode[prl] :: alpha                     = atomic["α"]
dform beta_df			: mode[prl] :: beta                      = atomic["β"]
dform pi_df			: mode[prl] :: pi                        = atomic["π"]
dform lambda_df			: mode[prl] :: lambda                    = atomic["λ"]
dform gamma_df			: mode[prl] :: gamma                     = atomic["γ"]
dform delta_df			: mode[prl] :: delta                     = atomic["δ"]
dform rho_df			: mode[prl] :: rho                       = atomic["ρ"]
dform sigma_df			: mode[prl] :: sigma                     = atomic["σ"]
dform epsilon_df		: mode[prl] :: epsilon                   = atomic["ε"]
dform eta_df			: mode[prl] :: eta                       = atomic["η"]
dform theta_df			: mode[prl] :: theta                     = atomic["θ"]
dform iota_df			: mode[prl] :: iota                      = atomic["ι"]
dform kappa_df			: mode[prl] :: kappa                     = atomic["κ"]
dform mu_df			: mode[prl] :: mu                        = atomic["μ"]
dform nu_df			: mode[prl] :: nu                        = atomic["ν"]
dform omicron_df		: mode[prl] :: omicron                   = atomic["ο"]
dform tau_df			: mode[prl] :: tau                       = atomic["τ"]
dform phi_df			: mode[prl] :: phi                       = atomic["φ"]
dform xi_df			: mode[prl] :: xi                        = atomic["χ"]
dform omega_df			: mode[prl] :: omega                     = atomic["ω"]

dform shortLeftarrow_df		: mode[html] :: shortLeftarrow            = html_uni[8656]
dform leftarrow_df		: mode[html] :: Leftarrow                 = html_uni[8656]
dform middlearrow_df		: mode[html] :: Middlearrow               = html_uni[9552]
dform shortRightarrow_df	: mode[html] :: shortRightarrow           = html_uni[8658]
dform rightarrow_df		: mode[html] :: Rightarrow                = html_uni[8658]
dform leftrightarrow_df		: mode[html] :: Leftrightarrow            = html_uni[8660]
dform ulcorner_df		: mode[html] :: ulcorner                  = html_uni[8968]
dform urcorner_df		: mode[html] :: urcorner                  = html_uni[8969]
dform vdash_df                  : mode[html] :: vdash                     = html_uni[8866]
dform integral_df		: mode[html] :: integral                  = html_uni[8747]
dform cdot_df                   : mode[html] :: cdot                      = html_sym["middot"]
dform downarrow_df		: mode[html] :: downarrow                 = html_uni[8595]
dform uparrow_df		: mode[html] :: uparrow                   = html_uni[8593]
dform vartriangleleft_df	: mode[html] :: vartriangleleft           = html_uni[9665]
dform vartriangleright_df	: mode[html] :: vartriangleright          = html_uni[9655]
dform alpha_df                  : mode[html] :: alpha                     = html_uni[945]
dform beta_df			: mode[html] :: beta                      = html_uni[946]
dform pi_df			: mode[html] :: pi                        = html_uni[960]
dform lambda_df			: mode[html] :: lambda                    = html_uni[955]
dform gamma_df			: mode[html] :: gamma                     = html_uni[947]
dform delta_df			: mode[html] :: delta                     = html_uni[948]
dform rho_df			: mode[html] :: rho                       = html_uni[961]
dform sigma_df			: mode[html] :: sigma                     = html_uni[963]
dform epsilon_df		: mode[html] :: epsilon                   = html_uni[949]
dform eta_df			: mode[html] :: eta                       = html_uni[951]
dform theta_df			: mode[html] :: theta                     = html_uni[952]
dform iota_df			: mode[html] :: iota                      = html_uni[953]
dform kappa_df			: mode[html] :: kappa                     = html_uni[954]
dform mu_df			: mode[html] :: mu                        = html_uni[956]
dform nu_df			: mode[html] :: nu                        = html_uni[957]
dform omicron_df		: mode[html] :: omicron                   = html_uni[959]
dform tau_df			: mode[html] :: tau                       = html_uni[964]
dform phi_df			: mode[html] :: phi                       = html_uni[966]
dform xi_df			: mode[html] :: xi                        = html_uni[967]
dform omega_df			: mode[html] :: omega                     = html_uni[969]

dform shortLeftarrow_df		: mode[tex] :: shortLeftarrow            = mathmacro["leftarrow"]
dform leftarrow_df		: mode[tex] :: Leftarrow                 = mathmacro["leftarrow"]
dform middlearrow_df		: mode[tex] :: Middlearrow               = mathmacro["-"]
dform shortRightarrow_df	: mode[tex] :: shortRightarrow           = mathmacro["rightarrow"]
dform rightarrow_df		: mode[tex] :: Rightarrow                = mathmacro["rightarrow"]
dform leftrightarrow_df		: mode[tex] :: Leftrightarrow            = mathmacro["leftrightarrow"]
dform ulcorner_df		: mode[tex] :: ulcorner                  = mathmacro["ulcorner"]
dform urcorner_df		: mode[tex] :: urcorner                  = mathmacro["urcorner"]
dform mid			: mode[tex] :: mid                       = `"|"
dform vdash_df                  : mode[tex] :: vdash                     = mathmacro["vdash"]
dform integral_df		: mode[tex] :: integral                  = mathmacro["int"]
dform cdot_df                   : mode[tex] :: cdot                      = mathmacro["cdot"]
dform downarrow_df		: mode[tex] :: downarrow                 = mathmacro["downarrow"]
dform uparrow_df		: mode[tex] :: uparrow                   = mathmacro["uparrow"]
dform vartriangleleft_df	: mode[tex] :: vartriangleleft           = mathmacro["vartriangleleft"]
dform vartriangleright_df	: mode[tex] :: vartriangleright          = mathmacro["vartriangleright"]
dform alpha_df                  : mode[tex] :: alpha                     = mathmacro["alpha"]
dform beta_df			: mode[tex] :: beta                      = mathmacro["beta"]
dform pi_df			: mode[tex] :: pi                        = mathmacro["pi"]
dform lambda_df			: mode[tex] :: lambda                    = mathmacro["lambda"]
dform gamma_df			: mode[tex] :: gamma                     = mathmacro["gamma"]
dform delta_df			: mode[tex] :: delta                     = mathmacro["delta"]
dform rho_df			: mode[tex] :: rho                       = mathmacro["rho"]
dform sigma_df			: mode[tex] :: sigma                     = mathmacro["sigma"]
dform epsilon_df		: mode[tex] :: epsilon                   = mathmacro["epsilon"]
dform eta_df			: mode[tex] :: eta                       = mathmacro["eta"]
dform theta_df			: mode[tex] :: theta                     = mathmacro["theta"]
dform iota_df			: mode[tex] :: iota                      = mathmacro["iota"]
dform kappa_df			: mode[tex] :: kappa                     = mathmacro["kappa"]
dform mu_df			: mode[tex] :: mu                        = mathmacro["mu"]
dform nu_df			: mode[tex] :: nu                        = mathmacro["nu"]
dform omicron_df		: mode[tex] :: omicron                   = mathmacro["omicron"]
dform tau_df			: mode[tex] :: tau                       = mathmacro["tau"]
dform phi_df			: mode[tex] :: phi                       = mathmacro["phi"]
dform xi_df			: mode[tex] :: xi                        = mathmacro["xi"]
dform omega_df			: mode[tex] :: omega                     = mathmacro["omega"]

dform wedge_df			: mode[prl] :: wedge                     = atomic["∧"]
dform tneg_df			: mode[prl] :: tneg                      = atomic["¬"]
dform member_df			: mode[prl] :: member                    = atomic["∈"]
dform plusminus_df		: mode[prl] :: plusminus                 = atomic["±"]
dform oplus_df			: mode[prl] :: oplus                     = atomic["⊕"]
dform infty_df			: mode[prl] :: infty                     = atomic["∞"]
dform partial_df		: mode[prl] :: partial                   = atomic["∂"]
dform cap_df			: mode[prl] :: cap                       = atomic["∩"]
dform cup_df			: mode[prl] :: cup                       = atomic["∪"]
dform forall_df			: mode[prl] :: forall                    = atomic["∀"]
dform exists_df			: mode[prl] :: "exists"                  = atomic["∃"]
dform oinfty_df			: mode[prl] :: oinfty                    = atomic["∝"]
dform shortleftrightarrow_df	: mode[prl] :: shortleftrightarrow       = atomic["↔"]
dform shortleftarrow_df		: mode[prl] :: shortleftarrow            = atomic["←"]
dform shortrightarrow_df	: mode[prl] :: shortrightarrow           = atomic["→"]
dform longleftrightarrow_df	: mode[prl] :: longleftrightarrow        = atomic["←──→"]
dform longleftarrow_df		: mode[prl] :: longleftarrow             = atomic["←──"]
dform longrightarrow_df		: mode[prl] :: longrightarrow            = atomic["──→"]
dform neq_df			: mode[prl] :: neq                       = atomic["≠"]
dform sim_df			: mode[prl] :: sim                       = atomic["∼"] (* atomic["~"] or atomic["˜"] *)
dform cong_df			: mode[prl] :: cong                      = atomic["≅"]
dform le_df			: mode[prl] :: le                        = atomic["≤"]
dform ge_df			: mode[prl] :: ge                        = atomic["≥"]
dform equiv_df			: mode[prl] :: equiv                     = atomic["≡"]
dform vee_df			: mode[prl] :: vee                       = atomic["∨"]
dform perp_df			: mode[prl] :: perp                      = atomic["⊥"]
dform esq_dfl			: mode[prl] :: esq_l                     = atomic["〚"]
dform esq_dfr			: mode[prl] :: esq_r                     = atomic["〛"]
dform leftarrow_df		: mode[prl] :: leftarrow                 = atomic["←─"]
dform middlearrow_df		: mode[prl] :: middlearrow               = atomic["─"]
dform rightarrow_df		: mode[prl] :: rightarrow                = atomic["─→"]
dform gamma_df			: mode[prl] :: Gamma                     = atomic["Γ"]
dform delta_df			: mode[prl] :: Delta                     = atomic["Δ"]
dform lambda_df		        : mode[prl] :: Lambda                    = atomic["Λ"]
dform sigma_df			: mode[prl] :: Sigma                     = atomic["Σ"]
dform pi_df			: mode[prl] :: Pi                        = atomic["Π"]
dform omega_df		        : mode[prl] :: Omega                     = atomic["Ω"]
dform times_df			: mode[prl] :: times                     = atomic["╳"] (* or atomic["⋆"] or atomic["×"] or atomic["⨉"] or atomic["⋊"] or atomic["⨯"] *)
dform div_df            	: mode[prl] :: "div"                     = atomic["÷"]
dform supplus_df		: mode[prl] :: supplus                   = atomic["⁺"]
dform supminus_df		: mode[prl] :: supminus                  = atomic["⁻"]
dform supcirc_df		: mode[prl] :: supcirc                   = atomic["°"]
dform subset_df			: mode[prl] :: "subset"                  = atomic["⊂"]
dform supset_df			: mode[prl] :: supset                    = atomic["⊃"]
dform subseteq_df		: mode[prl] :: subseteq                  = atomic["⊆"]
dform supseteq_df		: mode[prl] :: supseteq                  = atomic["⊇"]
dform sqsubset_df		: mode[prl] :: sqsubset                  = atomic["⊏"]
dform sqsupset_df		: mode[prl] :: sqsupset                  = atomic["⊐"]
dform sqsubseteq_df		: mode[prl] :: sqsubseteq                = atomic["⊑"]
dform sqsupseteq_df		: mode[prl] :: sqsupseteq                = atomic["⊒"]
dform subzero_df		: mode[prl] :: subzero                   = atomic["₀"]
dform subone_df			: mode[prl] :: subone                    = atomic["₁"]
dform subtwo_df			: mode[prl] :: subtwo                    = atomic["₂"]
dform subthree_df		: mode[prl] :: subthree                  = atomic["₃"]

dform suba_df			: mode[prl] :: suba = atomic["⒜"]
dform subb_df			: mode[prl] :: subb = atomic["⒝"]
dform subc_df			: mode[prl] :: subc = atomic["⒞"]
dform subd_df			: mode[prl] :: subd = atomic["⒟"]
dform sube_df			: mode[prl] :: sube = atomic["⒠"]
dform subf_df			: mode[prl] :: subf = atomic["⒡"]
dform subg_df			: mode[prl] :: subg = atomic["⒢"]
dform subh_df			: mode[prl] :: subh = atomic["⒣"]
dform subi_df			: mode[prl] :: subi = atomic["⒤"]
dform subj_df			: mode[prl] :: subj = atomic["⒥"]
dform subk_df			: mode[prl] :: subk = atomic["⒦"]
dform subl_df			: mode[prl] :: subl = atomic["⒧"]
dform subm_df			: mode[prl] :: subm = atomic["⒨"]
dform subn_df			: mode[prl] :: subn = atomic["⒩"]
dform subo_df			: mode[prl] :: subo = atomic["⒪"]
dform subp_df			: mode[prl] :: subp = atomic["⒫"]
dform subq_df			: mode[prl] :: subq = atomic["⒬"]
dform subr_df			: mode[prl] :: subr = atomic["⒭"]
dform subs_df			: mode[prl] :: subs = atomic["⒮"]
dform subt_df			: mode[prl] :: subt = atomic["⒯"]
dform subu_df			: mode[prl] :: subu = atomic["⒰"]
dform subv_df			: mode[prl] :: subv = atomic["⒱"]
dform subw_df			: mode[prl] :: subw = atomic["⒲"]
dform subx_df			: mode[prl] :: subx = atomic["⒳"]
dform suby_df			: mode[prl] :: suby = atomic["⒴"]
dform subz_df			: mode[prl] :: subz = atomic["⒵"]

dform wedge_df			: mode[html] :: wedge                     = html_uni[8743]
dform tneg_df			: mode[html] :: tneg                      = html_sym["not"]
dform member_df			: mode[html] :: member                    = html_uni[8712]
dform plusminus_df		: mode[html] :: plusminus                 = html_sym["plusmn"]
dform oplus_df			: mode[html] :: oplus                     = html_uni[8853]
dform infty_df			: mode[html] :: infty                     = html_uni[8734]
dform partial_df		: mode[html] :: partial                   = html_uni[8706]
dform cap_df			: mode[html] :: cap                       = html_uni[8745]
dform cup_df			: mode[html] :: cup                       = html_uni[8746]
dform forall_df			: mode[html] :: forall                    = html_uni[8704]
dform exists_df			: mode[html] :: "exists"                  = html_uni[8707]
dform oinfty_df			: mode[html] :: oinfty                    = html_uni[8733]
dform shortleftrightarrow_df	: mode[html] :: shortleftrightarrow       = html_uni[8596]
dform shortleftarrow_df		: mode[html] :: shortleftarrow            = html_uni[8592]
dform shortrightarrow_df	: mode[html] :: shortrightarrow           = html_uni[8594]
dform longleftrightarrow_df	: mode[html] :: longleftrightarrow        = html_uni[8592] html_sym["ndash"] html_uni[8594] (* html_uni[0x27F5] html_sym["ndash"] html_uni[0x27F6] *) (* html_uni[0x27F7] *)
dform longleftarrow_df		: mode[html] :: longleftarrow             = html_uni[8592] html_sym["ndash"] (* html_uni[0x27F5] html_sym["ndash"] *)
dform longrightarrow_df		: mode[html] :: longrightarrow            = html_sym["ndash"] html_uni[8594] (* html_sym["ndash"] html_uni[0x27F6] *)
dform neq_df			: mode[html] :: neq                       = html_uni[8800]
dform sim_df			: mode[html] :: sim                       = html_uni[8764]
dform cong_df			: mode[html] :: cong                      = html_uni[8773]
dform le_df			: mode[html] :: le                        = html_uni[8804]
dform ge_df			: mode[html] :: ge                        = html_uni[8805]
dform equiv_df			: mode[html] :: equiv                     = html_uni[8801]
dform vee_df			: mode[html] :: vee                       = html_uni[8744]
dform perp_df			: mode[html] :: perp                      = html_sym["perp"]
dform esq_dfl			: mode[html] :: esq_l                     = html_uni[12314]
dform esq_dfr			: mode[html] :: esq_r                     = html_uni[12315]
dform leftarrow_df		: mode[html] :: leftarrow                 = html_uni[8592]
dform middlearrow_df		: mode[html] :: middlearrow               = html_uni[9472]
dform rightarrow_df		: mode[html] :: rightarrow                = html_uni[8594]
dform gamma_df			: mode[html] :: Gamma                     = html_uni[915]
dform delta_df			: mode[html] :: Delta                     = html_uni[916]
dform lambda_df		        : mode[html] :: Lambda                    = html_uni[923]
dform pi_df			: mode[html] :: Pi                        = html_uni[928]
dform sigma_df			: mode[html] :: Sigma                     = html_uni[931]
dform omega_df			: mode[html] :: Omega                     = html_uni[937]
dform times_df			: mode[html] :: times                     = html_sym["times"]
dform div_df            	: mode[html] :: "div"                     = html_sym["divide"]
dform supplus_df		: mode[html] :: supplus                   = sup["+"]
dform supminus_df		: mode[html] :: supminus                  = sup["-"]
dform supcirc_df		: mode[html] :: supcirc                   = html_sym["deg"]
dform subset_df			: mode[html] :: "subset"                  = html_uni[8834]
dform supset_df			: mode[html] :: supset                    = html_uni[8835]
dform subseteq_df		: mode[html] :: subseteq                  = html_uni[8838]
dform supseteq_df		: mode[html] :: supseteq                  = html_uni[8839]
dform sqsubset_df		: mode[html] :: sqsubset                  = html_uni[8847]
dform sqsupset_df		: mode[html] :: sqsupset                  = html_uni[8848]
dform sqsubseteq_df		: mode[html] :: sqsubseteq                = html_uni[8849]
dform sqsupseteq_df		: mode[html] :: sqsupseteq                = html_uni[8850]
dform subzero_df		: mode[html] :: subzero                   = sub["0"]
dform subone_df			: mode[html] :: subone                    = sub["1"]
dform subtwo_df			: mode[html] :: subtwo                    = sub["2"]
dform subthree_df		: mode[html] :: subthree                  = sub["3"]

dform suba_df			: mode[html] :: suba = sub["a"]
dform subb_df			: mode[html] :: subb = sub["b"]
dform subc_df			: mode[html] :: subc = sub["c"]
dform subd_df			: mode[html] :: subd = sub["d"]
dform sube_df			: mode[html] :: sube = sub["e"]
dform subf_df			: mode[html] :: subf = sub["f"]
dform subg_df			: mode[html] :: subg = sub["g"]
dform subh_df			: mode[html] :: subh = sub["h"]
dform subi_df			: mode[html] :: subi = sub["i"]
dform subj_df			: mode[html] :: subj = sub["j"]
dform subk_df			: mode[html] :: subk = sub["k"]
dform subl_df			: mode[html] :: subl = sub["l"]
dform subm_df			: mode[html] :: subm = sub["m"]
dform subn_df			: mode[html] :: subn = sub["n"]
dform subo_df			: mode[html] :: subo = sub["o"]
dform subp_df			: mode[html] :: subp = sub["p"]
dform subq_df			: mode[html] :: subq = sub["q"]
dform subr_df			: mode[html] :: subr = sub["r"]
dform subs_df			: mode[html] :: subs = sub["s"]
dform subt_df			: mode[html] :: subt = sub["t"]
dform subu_df			: mode[html] :: subu = sub["u"]
dform subv_df			: mode[html] :: subv = sub["v"]
dform subw_df			: mode[html] :: subw = sub["w"]
dform subx_df			: mode[html] :: subx = sub["x"]
dform suby_df			: mode[html] :: suby = sub["y"]
dform subz_df			: mode[html] :: subz = sub["z"]

dform wedge_df			: mode[tex] :: wedge                     = mathmacro["wedge"]
dform tneg_df			: mode[tex] :: tneg                      = mathmacro["neg"]
dform member_df			: mode[tex] :: member                    = mathmacro["in"]
dform plusminus_df		: mode[tex] :: plusminus                 = mathmacro["pm"]
dform oplus_df			: mode[tex] :: oplus                     = mathmacro["oplus"]
dform infty_df			: mode[tex] :: infty                     = mathmacro["infty"]
dform partial_df		: mode[tex] :: partial                   = mathmacro["partial"]
dform cap_df			: mode[tex] :: cap                       = mathmacro["cap"]
dform cup_df			: mode[tex] :: cup                       = mathmacro["cup"]
dform forall_df			: mode[tex] :: forall                    = mathmacro["forall"]
dform exists_df			: mode[tex] :: "exists"                  = mathmacro["exists"]
dform oinfty_df			: mode[tex] :: oinfty                    = mathmacro["oinfty"]
dform shortleftrightarrow_df	: mode[tex] :: shortleftrightarrow       = mathmacro["shortleftrightarrow"]
dform shortleftarrow_df		: mode[tex] :: shortleftarrow            = mathmacro["shortleftarrow"]
dform shortrightarrow_df	: mode[tex] :: shortrightarrow           = mathmacro["shortrightarrow"]
dform longleftrightarrow_df	: mode[tex] :: longleftrightarrow        = mathmacro["longleftrightarrow"]
dform longleftarrow_df		: mode[tex] :: longleftarrow             = mathmacro["longleftarrow"]
dform longrightarrow_df		: mode[tex] :: longrightarrow            = mathmacro["longrightarrow"]
dform neq_df			: mode[tex] :: neq                       = mathmacro["neq"]
dform sim_df			: mode[tex] :: sim                       = mathmacro["sim"]
dform cong_df			: mode[tex] :: cong                      = mathmacro["cong"]
dform le_df			: mode[tex] :: le                        = mathmacro["le"]
dform ge_df			: mode[tex] :: ge                        = mathmacro["ge"]
dform equiv_df			: mode[tex] :: equiv                     = mathmacro["equiv"]
dform vee_df			: mode[tex] :: vee                       = mathmacro["vee"]
dform perp_df			: mode[tex] :: perp                      = mathmacro["perp"]
dform esq_dfl			: mode[tex] :: esq_l                     = `"[" izone `"\\![" ezone
dform esq_dfr			: mode[tex] :: esq_r                     = `"]" izone `"\\!]" ezone
dform leftarrow_df		: mode[tex] :: leftarrow                 = mathmacro["leftarrow"]
dform middlearrow_df		: mode[tex] :: middlearrow               = mathmacro["-"]
dform rightarrow_df		: mode[tex] :: rightarrow                = mathmacro["rightarrow"]
dform gamma_df			: mode[tex] :: Gamma                     = mathmacro["Gamma"]
dform delta_df			: mode[tex] :: Delta                     = mathmacro["Delta"]
dform lambda_df		        : mode[tex] :: Lambda                    = mathmacro["Lambda"]
dform pi_df			: mode[tex] :: Pi                        = mathmacro["Pi"]
dform sigma_df			: mode[tex] :: Sigma                     = mathmacro["Sigma"]
dform omega_df			: mode[tex] :: Omega                     = mathmacro["Omega"]
dform times_df			: mode[tex] :: times                     = mathmacro["times"]
dform div_df            	: mode[tex] :: "div"                     = mathmacro["div"]
dform circ_df		        : mode[tex] :: circ                      = mathmacro["circ"]
dform supplus_df		: mode[tex] :: supplus                   = sup["+"]
dform supminus_df		: mode[tex] :: supminus                  = sup["-"]
dform supcirc_df		: mode[tex] :: supcirc                   = sup{circ}
dform subset_df			: mode[tex] :: "subset"                  = mathmacro["subset"]
dform supset_df			: mode[tex] :: supset                    = mathmacro["supset"]
dform subseteq_df		: mode[tex] :: subseteq                  = mathmacro["subseteq"]
dform supseteq_df		: mode[tex] :: supseteq                  = mathmacro["supseteq"]
dform sqsubset_df		: mode[tex] :: sqsubset                  = mathmacro["sqsubset"]
dform sqsupset_df		: mode[tex] :: sqsupset                  = mathmacro["sqsupset"]
dform sqsubseteq_df		: mode[tex] :: sqsubseteq                = mathmacro["sqsubseteq"]
dform sqsupseteq_df		: mode[tex] :: sqsupseteq                = mathmacro["sqsupseteq"]
dform subzero_df		: mode[tex] :: subzero                   = sub["0"]
dform subone_df			: mode[tex] :: subone                    = sub["1"]
dform subtwo_df			: mode[tex] :: subtwo                    = sub["2"]
dform subthree_df		: mode[tex] :: subthree                  = sub["3"]

dform suba_df			: mode[tex] :: suba = sub["a"]
dform subb_df			: mode[tex] :: subb = sub["b"]
dform subc_df			: mode[tex] :: subc = sub["c"]
dform subd_df			: mode[tex] :: subd = sub["d"]
dform sube_df			: mode[tex] :: sube = sub["e"]
dform subf_df			: mode[tex] :: subf = sub["f"]
dform subg_df			: mode[tex] :: subg = sub["g"]
dform subh_df			: mode[tex] :: subh = sub["h"]
dform subi_df			: mode[tex] :: subi = sub["i"]
dform subj_df			: mode[tex] :: subj = sub["j"]
dform subk_df			: mode[tex] :: subk = sub["k"]
dform subl_df			: mode[tex] :: subl = sub["l"]
dform subm_df			: mode[tex] :: subm = sub["m"]
dform subn_df			: mode[tex] :: subn = sub["n"]
dform subo_df			: mode[tex] :: subo = sub["o"]
dform subp_df			: mode[tex] :: subp = sub["p"]
dform subq_df			: mode[tex] :: subq = sub["q"]
dform subr_df			: mode[tex] :: subr = sub["r"]
dform subs_df			: mode[tex] :: subs = sub["s"]
dform subt_df			: mode[tex] :: subt = sub["t"]
dform subu_df			: mode[tex] :: subu = sub["u"]
dform subv_df			: mode[tex] :: subv = sub["v"]
dform subw_df			: mode[tex] :: subw = sub["w"]
dform subx_df			: mode[tex] :: subx = sub["x"]
dform suby_df			: mode[tex] :: suby = sub["y"]
dform subz_df			: mode[tex] :: subz = sub["z"]

(*
 * Math mode.
 *)
dform math_div_df1              : mode[tex] :: math_div                  = izone `"\\div{}" ezone
dform math_div_df2              : except_mode[tex] :: math_div           = `"div"

(*
 * Source
 *)
dform mid_df : except_mode[tex] :: mid = `" | "
dform leftarrow_df : mode[src] :: Leftarrow = `"<="
dform leftrightarrow_df : mode[src] :: Leftrightarrow = `"<=>"
dform rightarrow_df : mode[src] :: Rightarrow = `"=>"
dform leftarrow_df : mode[src] :: leftarrow = `"<-"
dform rightarrow_df : mode[src] :: rightarrow = `"->"
dform longleftrightarrow_src_df : mode[src] :: longleftrightarrow = `"<-->"
dform longleftarrow_src_df : mode[src] :: longleftarrow = `"<--"
dform longrightarrow_src_df : mode[src] :: longrightarrow = `"-->"
dform times_df : mode[src] :: times = `"*"
dform vee_df : mode[src] :: vee = `"or"
dform wedge_df : mode[src] :: wedge = `"and"
dform sim_df : mode[src] :: sim = `"~"

(*
 * -*-
 * Local Variables:
 * Caml-master: "htmlcomp.run"
 * End:
 * -*-
 *)
