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

open Printf
open Mp_debug
open Perv

open String_util
open Refiner.Refiner.TermOp
open Rformat

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

(*
 * HTML control.
 *)
declare cd_begin[command:s]
declare cd_end

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
declare Gamma
declare Delta
declare Pi
declare Sigma
declare times
declare "div"
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
 * Change directory.
 *)
dform cd_begin_df1 : internal :: except_mode[html] :: cd_begin[name:s] =
   `""

dform cd_begin_df2 : internal :: mode[html] :: cd_begin[name:s] =
   izone `"<a href=\"http://cd.metaprl.local//" slot[name:s] `"\">" ezone

dform cd_end_df1 : internal :: except_mode[html] :: cd_end =
   `""

dform cd_end_df2 : internal :: mode[html] :: cd_end =
   izone `"</a>" ezone

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

dform info_begin_df : internal :: mode[html] :: info_begin =
   izone `"<font color=\"#115599\"><b>" ezone

dform info_end_df : internal :: mode[html] :: info_end =
   izone `"</b></font>" ezone

dform info_df1 : internal :: info[text:s] =
   info_begin slot[text:s] info_end

dform info_df2 : internal :: info{'t} =
   info_begin 't info_end

dform keyword_begin_df : internal :: mode[html] :: keyword_begin =
   izone `"<font color=\"#551155\"><b>" ezone

dform keyword_end_df : internal :: mode[html] :: keyword_end =
   izone `"</b></font>" ezone

dform info_begin_df_all : internal :: mode [src] :: info_begin = `""
dform info_end_df_all : internal :: mode[src] :: info_end = `""

dform keyword_begin_df_all : internal :: except_mode[html] :: except_mode[tex] :: keyword_begin = `""
dform keyword_end_df_all : internal :: except_mode[html] :: except_mode[tex] :: keyword_end = `""

dform keyword_df1 : internal :: except_mode[tex] :: keyword[text:s] =
   keyword_begin slot[text:s] keyword_end

dform keyword_df2 : internal :: keyword{'t} =
   keyword_begin 't keyword_end

dform bf_begin_df : internal :: mode[html] :: bf_begin =
   izone `"<b>" ezone

dform bf_end_df : internal :: mode[html] :: bf_end =
   izone `"</b>" ezone

dform bf_begin_df : internal :: mode[prl] :: bf_begin =
   pushfont["bf"]

dform bf_end_df : internal :: mode[prl] :: bf_end =
   popfont

dform bf_df1 : internal :: bf[text:s] =
   bf_begin slot[text:s] bf_end

dform bf_df2 : internal :: bf{'t} =
   bf_begin 't bf_end

dform it_begin_df : internal :: mode[html] :: it_begin =
   izone `"<b>" ezone

dform it_end_df : internal :: mode[html] :: it_end =
   izone `"</b>" ezone

dform it_begin_df : internal :: mode[prl] :: it_begin =
   `""

dform it_end_df : internal :: mode[prl] :: it_end =
   `""

dform it_df1 : internal :: it[text:s] =
   it_begin slot[text:s] it_end

dform it_df2 : internal :: it{'t} =
   it_begin 't it_end

dform i_df1 : internal :: i[text:s] =
   it[text:s]

dform i_df2 : internal :: i{'t} =
   it{'t}

dform math_it_df2 : internal :: except_mode[tex] :: math_it{'t} =
   it{'t}

dform math_it_df2 : internal :: except_mode[tex] :: math_it[text:s] =
   it[text:s]

dform html_sym_df : internal ::  mode[html] :: html_sym[text:s] =
   izone `"<b>&" ezone slot[text:s] izone `";</b>" ezone

dform html_uni_df : internal :: mode[html] :: html_uni[num:n] =
   izone `"<b>&#" ezone slot[num:n] izone `";</b>" ezone

dform em_begin_df : internal :: mode[html] :: em_begin =
   izone `"<em>" ezone

dform em_end_df : internal :: mode[html] :: em_end =
   izone `"</em>" ezone

dform em_begin_df : internal :: mode[prl] :: em_begin =
   `""

dform em_end_df : internal :: mode[prl] :: em_end =
   `""

dform em_df1 : internal :: em[text:s] =
   em_begin slot[text:s] em_end

dform em_df2 : internal :: em{'t} =
   em_begin 't em_end

dform emph_df1 : internal :: emph{'t} =
   em_begin 't em_end

dform tt_begin_df : internal :: mode[html] :: tt_begin =
   izone `"<tt>" ezone

dform tt_end_df : internal :: mode[html] :: tt_end =
   izone `"</tt>" ezone

dform tt_begin_df : internal :: mode[prl] :: tt_begin =
   `""

dform tt_end_df : internal :: mode[prl] :: tt_end =
   `""

dform tt_df1 : internal :: tt[text:s] =
   tt_begin slot[text:s] tt_end

dform tt_df2 : internal :: tt{'t} =
   tt_begin 't tt_end

dform url_df1 : internal :: mode[html] :: url[url:s] =
   izone `"<a href=\"" slot[url:s] `"\"><tt>" ezone slot[url:s] izone `"</tt></a>" ezone

dform url_df2 : internal :: mode[tex] :: url[url:s] =
   izone `"\\url{" ezone slot[url:s] izone `"}" ezone

dform url_df3 : internal :: mode[prl] :: mode [src] :: url[url:s] =
   `"@url[" slot[url:s] `"]"

dform sub_begin_df : internal :: mode[html] :: sub_begin =
   izone `"<sub>" ezone

dform sub_end_df : internal :: mode[html] :: sub_end =
   izone `"</sub>" ezone

dform sub_df1 : internal :: sub[text:s] =
   sub_begin slot[text:s] sub_end

dform sub_df2 : internal :: sub{'t} =
   sub_begin 't sub_end

dform sup_begin_df : internal :: mode[html] :: sup_begin =
   izone `"<sup>" ezone

dform sup_end_df : internal :: mode[html] :: sup_end =
   izone `"</sup>" ezone

dform sup_df1 : internal :: sup[text:s] =
   sup_begin slot[text:s] sup_end

dform sup_df2 : internal :: sup{'t} =
   sup_begin 't sup_end

dform sub_begin_df : internal :: mode[prl] :: sub_begin =
   `"_"

dform sub_end_df : internal :: mode[prl] :: sub_end =
   `""

dform sup_begin_df : internal :: mode[prl] :: sup_begin =
   `"^"

dform sup_end_df : internal :: mode[prl] :: sup_end =
   `""

dform small_begin_df : internal :: mode[html] :: small_begin =
   izone `"<small>" ezone

dform small_end_df : internal :: mode[html] :: small_end =
   izone `"</small>" ezone

dform small_df1 : internal :: small[text:s] =
   small_begin slot[text:s] small_end

dform small_df2 : internal :: small{'t} =
   small_begin 't small_end

dform esquash_df : internal :: esquash{'t} =
   esq_l 't esq_r

(************************************************************************
 * TEX HELPERS                                                          *
 ************************************************************************)

declare ensuremath[name:s]
declare ensuremath{'t}
declare mathBB[name:s]
declare mathmacro[name:s]

dform ensuremath_df0 : internal :: except_mode[tex] :: ensuremath{'t} =
   't

dform ensuremath_df1 : internal :: ensuremath[text:s] =
   ensuremath{slot[text:s]}

dform ensuremath_df2 : internal :: mode[tex] :: ensuremath{'t} =
   tzone["ensuremath"] slot{'t} ezone

dform ensuremath_cons_df : internal :: mode[tex] :: cons{ensuremath{'t1}; ensuremath{'t2}} =
   ensuremath{cons{'t1;'t2}}

dform mathBB_df : internal :: mode[tex] :: mathBB[text:s] =
   izone `"\\mathbb{" ezone slot[text:s] izone `"}" ezone

dform mathmacro_df : internal :: mode[tex] :: mathmacro[text:s] =
   izone `"\\" slot[text:s] `" " ezone

dform info_begin_df : internal :: mode[tex] :: info_begin =
   izone `"\\textbf{" ezone

dform info_end_df : internal :: mode[tex] :: info_end =
   izone `"}" ezone

dform info_begin_df : internal :: mode[prl] :: info_begin =
   pushfont["bf"]

dform info_end_df : internal :: mode[prl] :: info_end =
   popfont

let not_alnum c = not (is_alnum c)
ml_dform keyword_tex_df : internal :: mode[tex] :: keyword[text:s] format_term buf =
   fun term ->
     let text = dest_string_param term in
     let font = if String_util.for_all not_alnum text then "tt" else "bf" in
     format_izone buf;
     format_string buf ("\\mbox{\\" ^ font ^ " ");
     format_ezone buf;
     format_string buf text;
     format_izone buf;
     format_string buf "}";
     format_ezone buf

dform keyword_begin_df : internal :: mode[tex] :: keyword_begin =
   izone `"\\textbf{" ezone

dform keyword_end_df : internal :: mode[tex] :: keyword_end =
   izone `"}" ezone

dform bf_begin_df : internal :: mode[tex] :: bf_begin =
   tzone["textbf"]

dform bf_end_df : internal :: mode[tex] :: bf_end =
   ezone

dform it_begin_df : internal :: mode[tex] :: it_begin =
   tzone["textit"]

dform it_end_df : internal :: mode[tex] :: it_end =
   ezone

dform math_it_df1 : mode[tex] :: math_it{'t} =
   izone `"\\mathit{" ezone slot{'t} izone `"}" ezone

dform math_it_df1 : mode[tex] :: math_it[text:s] =
   izone `"\\mathit{" ezone slot[text:s] izone `"}" ezone

dform em_begin_df : internal :: mode[tex] :: em_begin =
   tzone["emph"]

dform em_end_df : internal :: mode[tex] :: em_end =
   ezone

dform tt_begin_df : internal :: mode[tex] :: tt_begin =
   tzone["texttt"]

dform tt_end_df : internal :: mode[tex] :: tt_end =
   ezone

dform sub_begin_df : internal :: mode[tex] :: sub_begin =
   izone `"_{" ezone

dform sub_end_df : internal :: mode[tex] :: sub_end =
   izone `"}" ezone

dform sup_begin_df : internal :: mode[tex] :: sup_begin =
   izone `"^{" ezone

dform sup_end_df : internal :: mode[tex] :: sup_end =
   izone `"}" ezone

dform small_begin_df : internal :: mode[tex] :: small_begin =
   izone `"{\\scriptsize " ezone

dform small_end_df : internal :: mode[tex] :: small_end =
   izone `"}" ezone

(************************************************************************
 * NUPRL FONT                                                           *
 ************************************************************************)

(* Displays *)
dform mathbbA_df		: internal :: mode[prl] :: mathbbA                   = `"𝔸"
dform mathbbB_df		: internal :: mode[prl] :: mathbbB                   = `"Ⓑ" (* or `"𝔸" *)
dform mathbbC_df		: internal :: mode[prl] :: mathbbC                   = `"ℂ"
dform mathbbD_df		: internal :: mode[prl] :: mathbbD                   = `"𝔻"
dform mathbbE_df		: internal :: mode[prl] :: mathbbE                   = `"𝔼"
dform mathbbF_df		: internal :: mode[prl] :: mathbbF                   = `"𝔽"
dform mathbbG_df		: internal :: mode[prl] :: mathbbG                   = `"𝔾"
dform mathbbH_df		: internal :: mode[prl] :: mathbbH                   = `"ℍ"
dform mathbbI_df		: internal :: mode[prl] :: mathbbI                   = `"�"
dform mathbbJ_df		: internal :: mode[prl] :: mathbbJ                   = `"�"
dform mathbbK_df		: internal :: mode[prl] :: mathbbK                   = `"�"
(* dform mathbbL_df		: internal :: mode[prl] :: mathbbL                   = `"�" *)
dform mathbbM_df		: internal :: mode[prl] :: mathbbM                   = `"�"
dform mathbbN_df		: internal :: mode[prl] :: mathbbN                   = `"ℕ"
dform mathbbO_df		: internal :: mode[prl] :: mathbbO                   = `"�"
dform mathbbP_df		: internal :: mode[prl] :: mathbbP                   = `"ℙ"
dform mathbbQ_df		: internal :: mode[prl] :: mathbbQ                   = `"ℚ"
dform mathbbR_df		: internal :: mode[prl] :: mathbbR                   = `"ℝ"
dform mathbbS_df		: internal :: mode[prl] :: mathbbS                   = `"S" (* or `"�" *)
dform mathbbT_df		: internal :: mode[prl] :: mathbbT                   = `"�"
dform mathbbU_df		: internal :: mode[prl] :: mathbbU                   = `"Ů" (* or: `"Ⓤ", or `"U" *) (* real: `"�" *)
dform mathbbV_df		: internal :: mode[prl] :: mathbbV                   = `"�"
dform mathbbW_df		: internal :: mode[prl] :: mathbbW                   = `"�"
dform mathbbX_df		: internal :: mode[prl] :: mathbbX                   = `"�"
dform mathbbY_df		: internal :: mode[prl] :: mathbbY                   = `"�"
dform mathbbZ_df		: internal :: mode[prl] :: mathbbZ                   = `"ℤ"

dform mathbbA_df		: internal :: mode[html] :: mathbbA                   = keyword["A"]
dform mathbbB_df		: internal :: mode[html] :: mathbbB                   = keyword["B"]
dform mathbbC_df		: internal :: mode[html] :: mathbbC                   = html_uni[8450]
dform mathbbD_df		: internal :: mode[html] :: mathbbD                   = keyword["D"]
dform mathbbE_df		: internal :: mode[html] :: mathbbE                   = keyword["E"]
dform mathbbF_df		: internal :: mode[html] :: mathbbF                   = keyword["F"]
dform mathbbG_df		: internal :: mode[html] :: mathbbG                   = keyword["G"]
dform mathbbH_df		: internal :: mode[html] :: mathbbH                   = html_uni[8461]
dform mathbbI_df		: internal :: mode[html] :: mathbbI                   = keyword["I"]
dform mathbbJ_df		: internal :: mode[html] :: mathbbJ                   = keyword["J"]
dform mathbbK_df		: internal :: mode[html] :: mathbbK                   = keyword["K"]
dform mathbbL_df		: internal :: mode[html] :: mathbbL                   = keyword["L"]
dform mathbbM_df		: internal :: mode[html] :: mathbbM                   = keyword["M"]
dform mathbbN_df		: internal :: mode[html] :: mathbbN                   = html_uni[8469]
dform mathbbO_df		: internal :: mode[html] :: mathbbO                   = keyword["O"]
dform mathbbP_df		: internal :: mode[html] :: mathbbP                   = html_uni[8473]
dform mathbbQ_df		: internal :: mode[html] :: mathbbQ                   = html_uni[8474]
dform mathbbR_df		: internal :: mode[html] :: mathbbR                   = html_uni[8477]
dform mathbbS_df		: internal :: mode[html] :: mathbbS                   = keyword["S"]
dform mathbbT_df		: internal :: mode[html] :: mathbbT                   = keyword["T"]
dform mathbbU_df		: internal :: mode[html] :: mathbbU                   = keyword["U"]
dform mathbbV_df		: internal :: mode[html] :: mathbbV                   = keyword["V"]
dform mathbbW_df		: internal :: mode[html] :: mathbbW                   = keyword["W"]
dform mathbbX_df		: internal :: mode[html] :: mathbbX                   = keyword["X"]
dform mathbbY_df		: internal :: mode[html] :: mathbbY                   = keyword["Y"]
dform mathbbZ_df		: internal :: mode[html] :: mathbbZ                   = html_uni[8484]

dform mathbbA_df		: internal :: mode[tex] :: mathbbA                   = mathBB["A"]
dform mathbbB_df		: internal :: mode[tex] :: mathbbB                   = mathBB["B"]
dform mathbbC_df		: internal :: mode[tex] :: mathbbC                   = mathBB["C"]
dform mathbbD_df		: internal :: mode[tex] :: mathbbD                   = mathBB["D"]
dform mathbbE_df		: internal :: mode[tex] :: mathbbE                   = mathBB["E"]
dform mathbbF_df		: internal :: mode[tex] :: mathbbF                   = mathBB["F"]
dform mathbbG_df		: internal :: mode[tex] :: mathbbG                   = mathBB["G"]
dform mathbbH_df		: internal :: mode[tex] :: mathbbH                   = mathBB["H"]
dform mathbbI_df		: internal :: mode[tex] :: mathbbI                   = mathBB["I"]
dform mathbbJ_df		: internal :: mode[tex] :: mathbbJ                   = mathBB["J"]
dform mathbbK_df		: internal :: mode[tex] :: mathbbK                   = mathBB["K"]
dform mathbbL_df		: internal :: mode[tex] :: mathbbL                   = mathBB["L"]
dform mathbbM_df		: internal :: mode[tex] :: mathbbM                   = mathBB["M"]
dform mathbbN_df		: internal :: mode[tex] :: mathbbN                   = mathBB["N"]
dform mathbbO_df		: internal :: mode[tex] :: mathbbO                   = mathBB["O"]
dform mathbbP_df		: internal :: mode[tex] :: mathbbP                   = mathBB["P"]
dform mathbbQ_df		: internal :: mode[tex] :: mathbbQ                   = mathBB["Q"]
dform mathbbR_df		: internal :: mode[tex] :: mathbbR                   = mathBB["R"]
dform mathbbS_df		: internal :: mode[tex] :: mathbbS                   = mathBB["S"]
dform mathbbT_df		: internal :: mode[tex] :: mathbbT                   = mathBB["T"]
dform mathbbU_df		: internal :: mode[tex] :: mathbbU                   = mathBB["U"]
dform mathbbV_df		: internal :: mode[tex] :: mathbbV                   = mathBB["V"]
dform mathbbW_df		: internal :: mode[tex] :: mathbbW                   = mathBB["W"]
dform mathbbX_df		: internal :: mode[tex] :: mathbbX                   = mathBB["X"]
dform mathbbY_df		: internal :: mode[tex] :: mathbbY                   = mathBB["Y"]
dform mathbbZ_df		: internal :: mode[tex] :: mathbbZ                   = mathBB["Z"]

dform shortLeftarrow_df		: internal :: mode[prl] :: shortLeftarrow            = `"⇐"
dform leftarrow_df		: internal :: mode[prl] :: Leftarrow                 = `"⇐═"
dform middlearrow_df		: internal :: mode[prl] :: Middlearrow               = `"═"
dform shortRightarrow_df	: internal :: mode[prl] :: shortRightarrow           = `"⇒"
dform rightarrow_df		: internal :: mode[prl] :: Rightarrow                = `"═⇒"
dform leftrightarrow_df		: internal :: mode[prl] :: Leftrightarrow            = `"⇐═⇒"
dform ulcorner_df		: internal :: mode[prl] :: ulcorner                  = `"⌈"
dform urcorner_df		: internal :: mode[prl] :: urcorner                  = `"⌉"
dform vdash_df                  : internal :: mode[prl] :: vdash                     = `"⊢"
dform integral_df		: internal :: mode[prl] :: integral                  = `"∫"
dform cdot_df                   : internal :: mode[prl] :: cdot                      = `"⋅"
dform downarrow_df		: internal :: mode[prl] :: downarrow                 = `"↓"
dform uparrow_df		: internal :: mode[prl] :: uparrow                   = `"↑"
dform alpha_df                  : internal :: mode[prl] :: alpha                     = `"α"
dform beta_df			: internal :: mode[prl] :: beta                      = `"β"
dform pi_df			: internal :: mode[prl] :: pi                        = `"π"
dform lambda_df			: internal :: mode[prl] :: lambda                    = `"λ"
dform gamma_df			: internal :: mode[prl] :: gamma                     = `"γ"
dform delta_df			: internal :: mode[prl] :: delta                     = `"δ"
dform rho_df			: internal :: mode[prl] :: rho                       = `"ρ"
dform sigma_df			: internal :: mode[prl] :: sigma                     = `"σ"
dform epsilon_df		: internal :: mode[prl] :: epsilon                   = `"ε"
dform eta_df			: internal :: mode[prl] :: eta                       = `"η"
dform theta_df			: internal :: mode[prl] :: theta                     = `"θ"
dform iota_df			: internal :: mode[prl] :: iota                      = `"ι"
dform kappa_df			: internal :: mode[prl] :: kappa                     = `"κ"
dform mu_df			: internal :: mode[prl] :: mu                        = `"μ"
dform nu_df			: internal :: mode[prl] :: nu                        = `"ν"
dform omicron_df		: internal :: mode[prl] :: omicron                   = `"ο"
dform tau_df			: internal :: mode[prl] :: tau                       = `"τ"
dform phi_df			: internal :: mode[prl] :: phi                       = `"φ"
dform xi_df			: internal :: mode[prl] :: xi                        = `"χ"
dform omega_df			: internal :: mode[prl] :: omega                     = `"ω"

dform shortLeftarrow_df		: internal :: mode[html] :: shortLeftarrow            = html_uni[8656]
dform leftarrow_df		: internal :: mode[html] :: Leftarrow                 = html_uni[8656]
dform middlearrow_df		: internal :: mode[html] :: Middlearrow               = html_uni[9552]
dform shortRightarrow_df	: internal :: mode[html] :: shortRightarrow           = html_uni[8658]
dform rightarrow_df		: internal :: mode[html] :: Rightarrow                = html_uni[8658]
dform leftrightarrow_df		: internal :: mode[html] :: Leftrightarrow            = html_uni[8660]
dform ulcorner_df		: internal :: mode[html] :: ulcorner                  = html_uni[8968]
dform urcorner_df		: internal :: mode[html] :: urcorner                  = html_uni[8969]
dform vdash_df                  : internal :: mode[html] :: vdash                     = html_uni[8866]
dform integral_df		: internal :: mode[html] :: integral                  = html_uni[8747]
dform cdot_df                   : internal :: mode[html] :: cdot                      = html_sym["middot"]
dform downarrow_df		: internal :: mode[html] :: downarrow                 = html_uni[8595]
dform uparrow_df		: internal :: mode[html] :: uparrow                   = html_uni[8593]
dform alpha_df                  : internal :: mode[html] :: alpha                     = html_uni[945]
dform beta_df			: internal :: mode[html] :: beta                      = html_uni[946]
dform pi_df			: internal :: mode[html] :: pi                        = html_uni[960]
dform lambda_df			: internal :: mode[html] :: lambda                    = html_uni[955]
dform gamma_df			: internal :: mode[html] :: gamma                     = html_uni[947]
dform delta_df			: internal :: mode[html] :: delta                     = html_uni[948]
dform rho_df			: internal :: mode[html] :: rho                       = html_uni[961]
dform sigma_df			: internal :: mode[html] :: sigma                     = html_uni[963]
dform epsilon_df		: internal :: mode[html] :: epsilon                   = html_uni[949]
dform eta_df			: internal :: mode[html] :: eta                       = html_uni[951]
dform theta_df			: internal :: mode[html] :: theta                     = html_uni[952]
dform iota_df			: internal :: mode[html] :: iota                      = html_uni[953]
dform kappa_df			: internal :: mode[html] :: kappa                     = html_uni[954]
dform mu_df			: internal :: mode[html] :: mu                        = html_uni[956]
dform nu_df			: internal :: mode[html] :: nu                        = html_uni[957]
dform omicron_df		: internal :: mode[html] :: omicron                   = html_uni[959]
dform tau_df			: internal :: mode[html] :: tau                       = html_uni[964]
dform phi_df			: internal :: mode[html] :: phi                       = html_uni[966]
dform xi_df			: internal :: mode[html] :: xi                        = html_uni[967]
dform omega_df			: internal :: mode[html] :: omega                     = html_uni[969]

dform shortLeftarrow_df		: internal :: mode[tex] :: shortLeftarrow            = mathmacro["leftarrow"]
dform leftarrow_df		: internal :: mode[tex] :: Leftarrow                 = mathmacro["leftarrow"]
dform middlearrow_df		: internal :: mode[tex] :: Middlearrow               = mathmacro["-"]
dform shortRightarrow_df	: internal :: mode[tex] :: shortRightarrow           = mathmacro["rightarrow"]
dform rightarrow_df		: internal :: mode[tex] :: Rightarrow                = mathmacro["rightarrow"]
dform leftrightarrow_df		: internal :: mode[tex] :: Leftrightarrow            = mathmacro["leftrightarrow"]
dform ulcorner_df		: internal :: mode[tex] :: ulcorner                  = mathmacro["ulcorner"]
dform urcorner_df		: internal :: mode[tex] :: urcorner                  = mathmacro["urcorner"]
dform mid            : internal :: mode[tex] :: mid                       = `"|"
dform vdash_df                  : internal :: mode[tex] :: vdash                     = mathmacro["vdash"]
dform integral_df		: internal :: mode[tex] :: integral                  = mathmacro["int"]
dform cdot_df                   : internal :: mode[tex] :: cdot                      = mathmacro["cdot"]
dform downarrow_df		: internal :: mode[tex] :: downarrow                 = mathmacro["downarrow"]
dform uparrow_df		: internal :: mode[tex] :: uparrow                   = mathmacro["uparrow"]
dform alpha_df                  : internal :: mode[tex] :: alpha                     = mathmacro["alpha"]
dform beta_df			: internal :: mode[tex] :: beta                      = mathmacro["beta"]
dform pi_df			: internal :: mode[tex] :: pi                        = mathmacro["pi"]
dform lambda_df			: internal :: mode[tex] :: lambda                    = mathmacro["lambda"]
dform gamma_df			: internal :: mode[tex] :: gamma                     = mathmacro["gamma"]
dform delta_df			: internal :: mode[tex] :: delta                     = mathmacro["delta"]
dform rho_df			: internal :: mode[tex] :: rho                       = mathmacro["rho"]
dform sigma_df			: internal :: mode[tex] :: sigma                     = mathmacro["sigma"]
dform epsilon_df		: internal :: mode[tex] :: epsilon                   = mathmacro["epsilon"]
dform eta_df			: internal :: mode[tex] :: eta                       = mathmacro["eta"]
dform theta_df			: internal :: mode[tex] :: theta                     = mathmacro["theta"]
dform iota_df			: internal :: mode[tex] :: iota                      = mathmacro["iota"]
dform kappa_df			: internal :: mode[tex] :: kappa                     = mathmacro["kappa"]
dform mu_df			: internal :: mode[tex] :: mu                        = mathmacro["mu"]
dform nu_df			: internal :: mode[tex] :: nu                        = mathmacro["nu"]
dform omicron_df		: internal :: mode[tex] :: omicron                   = mathmacro["omicron"]
dform tau_df			: internal :: mode[tex] :: tau                       = mathmacro["tau"]
dform phi_df			: internal :: mode[tex] :: phi                       = mathmacro["phi"]
dform xi_df			: internal :: mode[tex] :: xi                        = mathmacro["xi"]
dform omega_df			: internal :: mode[tex] :: omega                     = mathmacro["omega"]

dform wedge_df			: internal :: mode[prl] :: wedge                     = `"∧"
dform tneg_df			: internal :: mode[prl] :: tneg                      = `"¬"
dform member_df			: internal :: mode[prl] :: member                    = `"∈"
dform plusminus_df		: internal :: mode[prl] :: plusminus                 = `"±"
dform oplus_df			: internal :: mode[prl] :: oplus                     = `"⊕"
dform infty_df			: internal :: mode[prl] :: infty                     = `"∞"
dform partial_df		: internal :: mode[prl] :: partial                   = `"∂"
dform cap_df			: internal :: mode[prl] :: cap                       = `"∩"
dform cup_df			: internal :: mode[prl] :: cup                       = `"∪"
dform forall_df			: internal :: mode[prl] :: forall                    = `"∀"
dform exists_df			: internal :: mode[prl] :: "exists"                  = `"∃"
dform oinfty_df			: internal :: mode[prl] :: oinfty                    = `"∝"
dform shortleftrightarrow_df	: internal :: mode[prl] :: shortleftrightarrow       = `"↔"
dform shortleftarrow_df		: internal :: mode[prl] :: shortleftarrow            = `"←"
dform shortrightarrow_df	: internal :: mode[prl] :: shortrightarrow           = `"→"
dform longleftrightarrow_df	: internal :: mode[prl] :: longleftrightarrow        = `"←──→"
dform longleftarrow_df		: internal :: mode[prl] :: longleftarrow             = `"←──"
dform longrightarrow_df		: internal :: mode[prl] :: longrightarrow            = `"──→"
dform neq_df			: internal :: mode[prl] :: neq                       = `"≠"
dform sim_df			: internal :: mode[prl] :: sim                       = `"∼" (* `"~" or `"˜" *)
dform cong_df			: internal :: mode[prl] :: cong                      = `"≅"
dform le_df			: internal :: mode[prl] :: le                        = `"≤"
dform ge_df			: internal :: mode[prl] :: ge                        = `"≥"
dform equiv_df			: internal :: mode[prl] :: equiv                     = `"≡"
dform vee_df			: internal :: mode[prl] :: vee                       = `"∨"
dform perp_df			: internal :: mode[prl] :: perp                      = `"⊥"
dform esq_dfl			: internal :: mode[prl] :: esq_l                     = `"〚"
dform esq_dfr			: internal :: mode[prl] :: esq_r                     = `"〛"
dform leftarrow_df		: internal :: mode[prl] :: leftarrow                 = `"←─"
dform middlearrow_df		: internal :: mode[prl] :: middlearrow               = `"─"
dform rightarrow_df		: internal :: mode[prl] :: rightarrow                = `"─→"
dform gamma_df			: internal :: mode[prl] :: Gamma                     = `"Γ"
dform delta_df			: internal :: mode[prl] :: Delta                     = `"Δ"
dform sigma_df			: internal :: mode[prl] :: Sigma                     = `"Σ"
dform pi_df			: internal :: mode[prl] :: Pi                        = `"Π"
dform times_df			: internal :: mode[prl] :: times                     = `"╳" (* `"⨉" or `"⋊" or `"⨯" *)
dform div_df            	: internal :: mode[prl] :: "div"                     = `"÷"
dform supplus_df		: internal :: mode[prl] :: supplus                   = `"⁺"
dform supminus_df		: internal :: mode[prl] :: supminus                  = `"⁻"
dform supcirc_df		: internal :: mode[prl] :: supcirc                   = `"°"
dform subset_df			: internal :: mode[prl] :: "subset"                  = `"⊆"
dform supset_df			: internal :: mode[prl] :: supset                    = `"⊇"
dform subseteq_df		: internal :: mode[prl] :: subseteq                  = `"⊆"
dform supseteq_df		: internal :: mode[prl] :: supseteq                  = `"⊇"
dform sqsubset_df		: internal :: mode[prl] :: sqsubset                  = `"⊆" (* does unicode have symbols for sqsubset and Co? *)
dform sqsupset_df		: internal :: mode[prl] :: sqsupset                  = `"⊇"
dform sqsubseteq_df		: internal :: mode[prl] :: sqsubseteq                = `"⊆"
dform sqsupseteq_df		: internal :: mode[prl] :: sqsupseteq                = `"⊇"
dform subzero_df		: internal :: mode[prl] :: subzero                   = `"₀"
dform subone_df			: internal :: mode[prl] :: subone                    = `"₁"
dform subtwo_df			: internal :: mode[prl] :: subtwo                    = `"₂"
dform subthree_df		: internal :: mode[prl] :: subthree                  = `"₃"

dform suba_df			: internal :: mode[prl] :: suba = `"⒜"
dform subb_df			: internal :: mode[prl] :: subb = `"⒝"
dform subc_df			: internal :: mode[prl] :: subc = `"⒞"
dform subd_df			: internal :: mode[prl] :: subd = `"⒟"
dform sube_df			: internal :: mode[prl] :: sube = `"⒠"
dform subf_df			: internal :: mode[prl] :: subf = `"⒡"
dform subg_df			: internal :: mode[prl] :: subg = `"⒢"
dform subh_df			: internal :: mode[prl] :: subh = `"⒣"
dform subi_df			: internal :: mode[prl] :: subi = `"⒤"
dform subj_df			: internal :: mode[prl] :: subj = `"⒥"
dform subk_df			: internal :: mode[prl] :: subk = `"⒦"
dform subl_df			: internal :: mode[prl] :: subl = `"⒧"
dform subm_df			: internal :: mode[prl] :: subm = `"⒨"
dform subn_df			: internal :: mode[prl] :: subn = `"⒩"
dform subo_df			: internal :: mode[prl] :: subo = `"⒪"
dform subp_df			: internal :: mode[prl] :: subp = `"⒫"
dform subq_df			: internal :: mode[prl] :: subq = `"⒬"
dform subr_df			: internal :: mode[prl] :: subr = `"⒭"
dform subs_df			: internal :: mode[prl] :: subs = `"⒮"
dform subt_df			: internal :: mode[prl] :: subt = `"⒯"
dform subu_df			: internal :: mode[prl] :: subu = `"⒰"
dform subv_df			: internal :: mode[prl] :: subv = `"⒱"
dform subw_df			: internal :: mode[prl] :: subw = `"⒲"
dform subx_df			: internal :: mode[prl] :: subx = `"⒳"
dform suby_df			: internal :: mode[prl] :: suby = `"⒴"
dform subz_df			: internal :: mode[prl] :: subz = `"⒵"

dform wedge_df			: internal :: mode[html] :: wedge                     = html_uni[8743]
dform tneg_df			: internal :: mode[html] :: tneg                      = html_sym["not"]
dform member_df			: internal :: mode[html] :: member                    = html_uni[8712]
dform plusminus_df		: internal :: mode[html] :: plusminus                 = html_sym["plusmn"]
dform oplus_df			: internal :: mode[html] :: oplus                     = html_uni[8853]
dform infty_df			: internal :: mode[html] :: infty                     = html_uni[8734]
dform partial_df		: internal :: mode[html] :: partial                   = html_uni[8706]
dform cap_df			: internal :: mode[html] :: cap                       = html_uni[8745]
dform cup_df			: internal :: mode[html] :: cup                       = html_uni[8746]
dform forall_df			: internal :: mode[html] :: forall                    = html_uni[8704]
dform exists_df			: internal :: mode[html] :: "exists"                  = html_uni[8707]
dform oinfty_df			: internal :: mode[html] :: oinfty                    = html_uni[8733]
dform shortleftrightarrow_df	: internal :: mode[html] :: shortleftrightarrow       = html_uni[8596]
dform shortleftarrow_df		: internal :: mode[html] :: shortleftarrow            = html_uni[8592]
dform shortrightarrow_df	: internal :: mode[html] :: shortrightarrow           = html_uni[8594]
dform longleftrightarrow_df	: internal :: mode[html] :: longleftrightarrow        = html_uni[8596]
dform longleftarrow_df		: internal :: mode[html] :: longleftarrow             = html_uni[8592]
dform longrightarrow_df		: internal :: mode[html] :: longrightarrow            = html_uni[8594]
dform neq_df			: internal :: mode[html] :: neq                       = html_uni[8800]
dform sim_df			: internal :: mode[html] :: sim                       = html_uni[8764]
dform cong_df			: internal :: mode[html] :: cong                      = html_uni[8773]
dform le_df			: internal :: mode[html] :: le                        = html_uni[8804]
dform ge_df			: internal :: mode[html] :: ge                        = html_uni[8805]
dform equiv_df			: internal :: mode[html] :: equiv                     = html_uni[8801]
dform vee_df			: internal :: mode[html] :: vee                       = html_uni[8744]
dform perp_df			: internal :: mode[html] :: perp                      = html_sym["perp"]
dform esq_dfl			: internal :: mode[html] :: esq_l                     = html_uni[12314]
dform esq_dfr			: internal :: mode[html] :: esq_r                     = html_uni[12315]
dform leftarrow_df		: internal :: mode[html] :: leftarrow                 = html_uni[8592]
dform middlearrow_df		: internal :: mode[html] :: middlearrow               = html_uni[9472]
dform rightarrow_df		: internal :: mode[html] :: rightarrow                = html_uni[8594]
dform gamma_df			: internal :: mode[html] :: Gamma                     = html_uni[915]
dform delta_df			: internal :: mode[html] :: Delta                     = html_uni[916]
dform pi_df			: internal :: mode[html] :: Pi                        = html_uni[928]
dform sigma_df			: internal :: mode[html] :: Sigma                     = html_uni[931]
dform times_df			: internal :: mode[html] :: times                     = html_sym["times"]
dform div_df            	: internal :: mode[html] :: "div"                     = html_sym["divide"]
dform supplus_df		: internal :: mode[html] :: supplus                   = sup["+"]
dform supminus_df		: internal :: mode[html] :: supminus                  = sup["-"]
dform supcirc_df		: internal :: mode[html] :: supcirc                   = html_sym["deg"]
dform subset_df			: internal :: mode[html] :: "subset"                  = html_uni[8838] (* shoud be subset, not subseteq! *)
dform supset_df			: internal :: mode[html] :: supset                    = html_uni[8839] 
dform subseteq_df		: internal :: mode[html] :: subseteq                  = html_uni[8838] 
dform supseteq_df		: internal :: mode[html] :: supseteq                  = html_uni[8839]
dform sqsubset_df		: internal :: mode[html] :: sqsubset                  = html_uni[8838] (* Are there symbols for sqsub*? *)
dform sqsupset_df		: internal :: mode[html] :: sqsupset                  = html_uni[8839]
dform sqsubseteq_df		: internal :: mode[html] :: sqsubseteq                = html_uni[8838]
dform sqsupseteq_df		: internal :: mode[html] :: sqsupseteq                = html_uni[8839]
dform subzero_df		: internal :: mode[html] :: subzero                   = sub["0"]
dform subone_df			: internal :: mode[html] :: subone                    = sub["1"]
dform subtwo_df			: internal :: mode[html] :: subtwo                    = sub["2"]
dform subthree_df		: internal :: mode[html] :: subthree                  = sub["3"]

dform suba_df			: internal :: mode[html] :: suba = sub["a"]
dform subb_df			: internal :: mode[html] :: subb = sub["b"]
dform subc_df			: internal :: mode[html] :: subc = sub["c"]
dform subd_df			: internal :: mode[html] :: subd = sub["d"]
dform sube_df			: internal :: mode[html] :: sube = sub["e"]
dform subf_df			: internal :: mode[html] :: subf = sub["f"]
dform subg_df			: internal :: mode[html] :: subg = sub["g"]
dform subh_df			: internal :: mode[html] :: subh = sub["h"]
dform subi_df			: internal :: mode[html] :: subi = sub["i"]
dform subj_df			: internal :: mode[html] :: subj = sub["j"]
dform subk_df			: internal :: mode[html] :: subk = sub["k"]
dform subl_df			: internal :: mode[html] :: subl = sub["l"]
dform subm_df			: internal :: mode[html] :: subm = sub["m"]
dform subn_df			: internal :: mode[html] :: subn = sub["n"]
dform subo_df			: internal :: mode[html] :: subo = sub["o"]
dform subp_df			: internal :: mode[html] :: subp = sub["p"]
dform subq_df			: internal :: mode[html] :: subq = sub["q"]
dform subr_df			: internal :: mode[html] :: subr = sub["r"]
dform subs_df			: internal :: mode[html] :: subs = sub["s"]
dform subt_df			: internal :: mode[html] :: subt = sub["t"]
dform subu_df			: internal :: mode[html] :: subu = sub["u"]
dform subv_df			: internal :: mode[html] :: subv = sub["v"]
dform subw_df			: internal :: mode[html] :: subw = sub["w"]
dform subx_df			: internal :: mode[html] :: subx = sub["x"]
dform suby_df			: internal :: mode[html] :: suby = sub["y"]
dform subz_df			: internal :: mode[html] :: subz = sub["z"]

dform wedge_df			: internal :: mode[tex] :: wedge                     = mathmacro["wedge"]
dform tneg_df			: internal :: mode[tex] :: tneg                      = mathmacro["neg"]
dform member_df			: internal :: mode[tex] :: member                    = mathmacro["in"]
dform plusminus_df		: internal :: mode[tex] :: plusminus                 = mathmacro["pluminus"]
dform oplus_df			: internal :: mode[tex] :: oplus                     = mathmacro["oplus"]
dform infty_df			: internal :: mode[tex] :: infty                     = mathmacro["infty"]
dform partial_df		: internal :: mode[tex] :: partial                   = mathmacro["partial"]
dform cap_df			: internal :: mode[tex] :: cap                       = mathmacro["cap"]
dform cup_df			: internal :: mode[tex] :: cup                       = mathmacro["cup"]
dform forall_df			: internal :: mode[tex] :: forall                    = mathmacro["forall"]
dform exists_df			: internal :: mode[tex] :: "exists"                  = mathmacro["exists"]
dform oinfty_df			: internal :: mode[tex] :: oinfty                    = mathmacro["oinfty"]
dform shortleftrightarrow_df	: internal :: mode[tex] :: shortleftrightarrow       = mathmacro["shortleftrightarrow"]
dform shortleftarrow_df		: internal :: mode[tex] :: shortleftarrow            = mathmacro["shortleftarrow"]
dform shortrightarrow_df	: internal :: mode[tex] :: shortrightarrow           = mathmacro["shortrightarrow"]
dform longleftrightarrow_df	: internal :: mode[tex] :: longleftrightarrow        = mathmacro["longleftrightarrow"]
dform longleftarrow_df		: internal :: mode[tex] :: longleftarrow             = mathmacro["longleftarrow"]
dform longrightarrow_df		: internal :: mode[tex] :: longrightarrow            = mathmacro["longrightarrow"]
dform neq_df			: internal :: mode[tex] :: neq                       = mathmacro["neq"]
dform sim_df			: internal :: mode[tex] :: sim                       = mathmacro["sim"]
dform cong_df			: internal :: mode[tex] :: cong                      = mathmacro["cong"]
dform le_df			: internal :: mode[tex] :: le                        = mathmacro["le"]
dform ge_df			: internal :: mode[tex] :: ge                        = mathmacro["ge"]
dform equiv_df			: internal :: mode[tex] :: equiv                     = mathmacro["equiv"]
dform vee_df			: internal :: mode[tex] :: vee                       = mathmacro["vee"]
dform perp_df			: internal :: mode[tex] :: perp                      = mathmacro["perp"]
dform esq_dfl			: internal :: mode[tex] :: esq_l                     = `"[" izone `"\\![" ezone
dform esq_dfr			: internal :: mode[tex] :: esq_r                     = `"]" izone `"\\!]" ezone
dform leftarrow_df		: internal :: mode[tex] :: leftarrow                 = mathmacro["leftarrow"]
dform middlearrow_df		: internal :: mode[tex] :: middlearrow               = mathmacro["-"]
dform rightarrow_df		: internal :: mode[tex] :: rightarrow                = mathmacro["rightarrow"]
dform gamma_df			: internal :: mode[tex] :: Gamma                     = mathmacro["Gamma"]
dform delta_df			: internal :: mode[tex] :: Delta                     = mathmacro["Delta"]
dform pi_df			: internal :: mode[tex] :: Pi                        = mathmacro["Pi"]
dform sigma_df			: internal :: mode[tex] :: Sigma                     = mathmacro["Sigma"]
dform times_df			: internal :: mode[tex] :: times                     = mathmacro["times"]
dform div_df            	: internal :: mode[tex] :: "div"                     = mathmacro["div"]
dform supplus_df		: internal :: mode[tex] :: supplus                   = izone `"^{+}" ezone
dform supminus_df		: internal :: mode[tex] :: supminus                  = izone `"^{-}" ezone
dform supcirc_df		: internal :: mode[tex] :: supcirc                   = izone `"^{\circ}" ezone
dform subset_df			: internal :: mode[tex] :: "subset"                  = mathmacro["subset"]
dform supset_df			: internal :: mode[tex] :: supset                    = mathmacro["supset"]
dform subseteq_df		: internal :: mode[tex] :: subseteq                  = mathmacro["subseteq"]
dform supseteq_df		: internal :: mode[tex] :: supseteq                  = mathmacro["supseteq"]
dform sqsubset_df		: internal :: mode[tex] :: sqsubset                  = mathmacro["sqsubset"]
dform sqsupset_df		: internal :: mode[tex] :: sqsupset                  = mathmacro["sqsupset"]
dform sqsubseteq_df		: internal :: mode[tex] :: sqsubseteq                = mathmacro["sqsubseteq"]
dform sqsupseteq_df		: internal :: mode[tex] :: sqsupseteq                = mathmacro["sqsupseteq"]
dform subzero_df		: internal :: mode[tex] :: subzero                   = izone `"_0" ezone
dform subone_df			: internal :: mode[tex] :: subone                    = izone `"_1" ezone
dform subtwo_df			: internal :: mode[tex] :: subtwo                    = izone `"_2" ezone
dform subthree_df		: internal :: mode[tex] :: subthree                  = izone `"_3" ezone

dform suba_df			: internal :: mode[tex] :: suba = izone `"_a" ezone
dform subb_df			: internal :: mode[tex] :: subb = izone `"_b" ezone
dform subc_df			: internal :: mode[tex] :: subc = izone `"_c" ezone
dform subd_df			: internal :: mode[tex] :: subd = izone `"_d" ezone
dform sube_df			: internal :: mode[tex] :: sube = izone `"_e" ezone
dform subf_df			: internal :: mode[tex] :: subf = izone `"_f" ezone
dform subg_df			: internal :: mode[tex] :: subg = izone `"_g" ezone
dform subh_df			: internal :: mode[tex] :: subh = izone `"_h" ezone
dform subi_df			: internal :: mode[tex] :: subi = izone `"_i" ezone
dform subj_df			: internal :: mode[tex] :: subj = izone `"_j" ezone
dform subk_df			: internal :: mode[tex] :: subk = izone `"_k" ezone
dform subl_df			: internal :: mode[tex] :: subl = izone `"_l" ezone
dform subm_df			: internal :: mode[tex] :: subm = izone `"_m" ezone
dform subn_df			: internal :: mode[tex] :: subn = izone `"_n" ezone
dform subo_df			: internal :: mode[tex] :: subo = izone `"_o" ezone
dform subp_df			: internal :: mode[tex] :: subp = izone `"_p" ezone
dform subq_df			: internal :: mode[tex] :: subq = izone `"_q" ezone
dform subr_df			: internal :: mode[tex] :: subr = izone `"_r" ezone
dform subs_df			: internal :: mode[tex] :: subs = izone `"_s" ezone
dform subt_df			: internal :: mode[tex] :: subt = izone `"_t" ezone
dform subu_df			: internal :: mode[tex] :: subu = izone `"_u" ezone
dform subv_df			: internal :: mode[tex] :: subv = izone `"_v" ezone
dform subw_df			: internal :: mode[tex] :: subw = izone `"_w" ezone
dform subx_df			: internal :: mode[tex] :: subx = izone `"_x" ezone
dform suby_df			: internal :: mode[tex] :: suby = izone `"_y" ezone
dform subz_df			: internal :: mode[tex] :: subz = izone `"_z" ezone

(*
 * Math mode.
 *)
dform math_div_df1              : internal :: mode[tex] :: math_div                  = izone `"\\div{}" ezone
dform math_div_df2              : internal :: except_mode[tex] :: math_div           = `"div"

(*
 * Source
 *)
dform mid_df : internal :: except_mode[tex] :: mid = `" | "
dform leftarrow_df : internal :: mode[src] :: Leftarrow = `"<="
dform leftrightarrow_df : internal :: mode[src] :: Leftrightarrow = `"<=>"
dform rightarrow_df : internal :: mode[src] :: Rightarrow = `"=>"
dform leftarrow_df : internal :: mode[src] :: leftarrow = `"<-"
dform rightarrow_df : internal :: mode[src] :: rightarrow = `"->"
dform longleftrightarrow_src_df : internal :: mode[src] :: longleftrightarrow = `"<-->"
dform longleftarrow_src_df : internal :: mode[src] :: longleftarrow = `"<--"
dform longrightarrow_src_df : internal :: mode[src] :: longrightarrow = `"-->"
dform times_df : internal :: mode[src] :: times = `"*"
dform vee_df : internal :: mode[src] :: vee = `"or"
dform wedge_df : internal :: mode[src] :: wedge = `"and"
dform sim_df : internal :: mode[src] :: sim = `"~"

(*
 * -*-
 * Local Variables:
 * Caml-master: "htmlcomp.run"
 * End:
 * -*-
 *)