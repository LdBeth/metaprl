(*
 * @begin[doc]
 * @module[Comment]
 *
 * Structured comments are comments with ! as the first character.
 * Currently, structured comments can be used only at the top level of
 * of an interface or implementation.  The text in the comment is parsed
 * into a sequence of strings in a list (using mk_xlist_term).  The text
 * can also contain terms.  All terms are described with the form
 *
 *    @opname[s1, ..., sm]{t1; ...; tn},
 *
 * where s1, ..., sn are strings, and where t1, ..., tn are comment text.
 * The parameters/subterms can be ommitted.
 * @end[doc]
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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
 * jyh@cs.caltech.edu
 *
 * @end[license]
 *)

extends Base_dform

(*
 * @begin[doc]
 * @terms
 * Basic comment structure.
 * @end[doc]
 *)
declare comment_white : Dform
declare comment_string[s:s] : Dform
declare comment_term{'t : Dform} : Dform
declare comment_block{'t : Dform} : Dform

(*
 * @begin[doc]
 * These are the valid comment blocks.
 * @end[doc]
 *)
declare "doc"{'t : Dform} : Dform
declare license{'t : Dform} : Dform
declare spelling{'t : Dform} : Dform
declare misspelled{'t : Dform} : Dform

(*
 * hese terms are used by the summary module to display comments.
 *)
declare prl_comment{'t : Dform} : Dform
declare tex_comment{'t : Dform} : Dform
declare html_comment{'t : Dform} : Dform

(*
 * @begin[doc]
 * @terms
 * The @theory{'t} term produces a chapter header for a collection of modules
 * and the @module[name:s] term produces a section header for the current module.
 * @end[doc]
 *)
declare "theory"{'t : Dform} : Dform
declare "module"[name:s] : Dform
declare "module"{'name : Dform} : Dform

(*
 * @begin[doc]
 * Bookmaking commands.
 * @end[doc]
 *)
declare chapter[name:s]{'t : Dform} : Dform
declare section[name:s]{'t : Dform} : Dform
declare subsection[name:s]{'t : Dform} : Dform
declare subsubsection[name:s]{'t : Dform} : Dform

(*
 * @begin[doc]
 * The @modsection{'t} term prduces a subsection header.
 * @end[doc]
 *)
declare modsection{'t : Dform} : Dform

(*
 * @begin[doc]
 * The @modsubsection{'t} term prduces a subsection header.
 * @end[doc]
 *)
declare modsubsection{'t : Dform} : Dform

declare paragraph{'t : Dform} : Dform
declare subparagraph{'t : Dform} : Dform

(*
 * Generic targets.
 *)
declare target[name:s]{'t : Dform} : Dform
declare hreftarget[name:s] : Dform

(*
 * Other forms of sectioning commands.
 *)
declare parents : Dform
declare rewrites : Dform
declare rules : Dform
declare convs : Dform
declare tactics : Dform
declare resources : Dform
declare terms : Dform

(*
 * Hypertext targets.
 *)
declare "term"[name:s] : Dform
declare "resource"[name:s] : Dform
declare "tactic"[name:s] : Dform
declare "conv"[name:s] : Dform
declare "rule"[name:s] : Dform
declare "rewrite"[name:s] : Dform

(*
 * Hypertext links.
 *)
declare hrefmodule[name:s] : Dform
declare hrefterm[name:s] : Dform
declare hrefresource[name:s] : Dform
declare hrefrewrite[name:s] : Dform
declare hreftactic[name:s] : Dform
declare hrefconv[name:s] : Dform
declare hrefrule[name:s] : Dform

declare refchapter[name:s] : Dform
declare refsection[name:s] : Dform
declare refsubsection[name:s] : Dform
declare refsubsubsection[name:s] : Dform

declare refmodule[name:s] : Dform
declare refterm[name:s] : Dform
declare refresource[name:s] : Dform
declare refrewrite[name:s] : Dform
declare reftactic[name:s] : Dform
declare refconv[name:s] : Dform
declare refrule[name:s] : Dform
declare reffigure[name:s] : Dform

(*
 * TeX structuring.
 *)
declare "begin"[name:s] : Dform
declare "end"[name:s] : Dform

(*
 * TeX control.
 *)
declare docoff : Dform
declare docon : Dform
declare noindent : Dform
declare cite[s:s] : Dform

(*
 * Special terms.
 *)
declare "MetaPRL" : Dform
declare "Nuprl" : Dform
declare "NuPRL" : Dform
declare "OCaml" : Dform
declare "LaTeX" : Dform
declare "MartinLof" : Dform

(*
 * Color.
 *)
declare color[name:s] : Dform
declare pagecolor[name:s] : Dform
declare colorbox[name:s]{'t : Dform} : Dform

(*
 * @begin[doc]
 * The @code{phantom} term produces white space, equivalent in width
 * to the term being typeset.
 * @end[doc]
 *)
declare phantom{'t : Dform} : Dform

(*
 * Formatting.
 *)
declare math[s:s] : Dform
declare math{'t : Dform} : Dform
declare centermath[s:s] : Dform
declare centermath{'t : Dform} : Dform

declare minipage[width:s]{'t : Dform} : Dform
declare minipage[width:s,pos:s]{'t : Dform} : Dform
declare code[text:s] : Dform
declare verbatim[text:s] : Dform
declare iverbatim[text:s] : Dform
declare email[text:s] : Dform
declare center{'t : Dform} : Dform
declare figure[label:s]{'t : Dform} : Dform
declare figure[label:s,pos:s]{'t : Dform} : Dform
declare caption{'caption : Dform} : Dform
declare indent{'t : Dform} : Dform
declare quote{'t : Dform} : Dform
declare quotation{'t : Dform} : Dform
declare footnote{'t : Dform} : Dform
declare enumerate{'t : Dform} : Dform
declare itemize{'t : Dform} : Dform
declare description{'t : Dform} : Dform
declare item{'t : Dform} : Dform
declare item{'label; 'body} : Dform

(*
 * Other macros.
 *)
declare lbrace : Dform
declare rbrace : Dform
declare comment[who:s]{'e : Dform} : Dform

(************************************************************************
 * MATH MODE                                                            *
 ************************************************************************)

(*
 * Toplevel forms.
 *)
declare math_misspelled{'t : Dform} : Dform

(*
 * Allow raw printing.
 *)
declare math_slot[tag:s]{'t : Dform} : Dform

(*
 * Font control in math mode.
 *)
declare math_hbox{'t : Dform} : Dform
declare math_mbox{'t : Dform} : Dform

declare math_bb{'T} : Dform
declare math_tt[TEXT:s] : Dform
declare math_tt{'t : Dform} : Dform
declare math_bf{'t : Dform} : Dform
declare math_bf[text:s] : Dform
declare math_i{'t : Dform} : Dform
declare math_i[text:s] : Dform
declare math_emph{'t : Dform} : Dform
declare math_mathop{'t : Dform} : Dform
declare math_mathrel[text:s] : Dform
declare math_mathrel{'t : Dform} : Dform

(*
 * Math symbols.
 *)
declare math_Type : Dform

declare math_colon : Dform
declare math_rightarrow : Dform
declare math_Rightarrow : Dform
declare math_leftarrow : Dform
declare math_Leftarrow : Dform
declare math_leftrightarrow : Dform
declare math_Leftrightarrow : Dform
declare math_longrightarrow : Dform
declare math_longleftrightarrow : Dform

declare math_le : Dform
declare math_ge : Dform
declare math_wedge : Dform
declare math_vee : Dform
declare math_phi : Dform
declare math_varphi : Dform
declare math_cap : Dform
declare math_cup : Dform
declare math_bigcap : Dform
declare math_bigcup : Dform
declare math_in : Dform
declare math_cdot : Dform
declare math_cdots : Dform
declare math_vdots : Dform
declare math_ldots : Dform
declare math_subset : Dform
declare math_subseteq : Dform
declare math_times : Dform
declare math_equiv : Dform
declare math_space : Dform
declare math_neg : Dform
declare math_neq : Dform
declare math_forall : Dform
declare math_exists : Dform
declare math_alpha : Dform
declare math_beta : Dform
declare math_lambda : Dform
declare math_epsilon : Dform
declare math_Gamma : Dform
declare math_vdash : Dform
declare math_int : Dform
declare math_lbrace : Dform
declare math_rbrace : Dform
declare math_lfloor : Dform
declare math_rfloor : Dform
declare math_quad : Dform
declare math_qquad : Dform
declare math_bullet : Dform
declare math_left[s] : Dform
declare math_right[s] : Dform

declare math_vec{'e : Dform} : Dform
declare math_underbrace{'e : Dform} : Dform
declare math_underbrace{'e1 : Dform; 'e2 : Dform} : Dform

(*
 * Sub/superscripts.
 *)
declare math_subscript{'t1 : Dform; 't2 : Dform} : Dform
declare math_superscript{'t1 : Dform; 't2 : Dform} : Dform

(*
 * Math blocks.
 *
 * An array looks like this:
 * @begin[array, rcl]
 * @begin[line]
 * @item{x}
 * @item{y}
 * @item{z}
 * @end[line]
 *
 * ...
 * @end[array]
 *)
declare tabular[placement,tags]{'t : Dform} : Dform
declare tabular[tags]{'t : Dform} : Dform
declare line{'t : Dform} : Dform
declare cr : Dform
declare hline : Dform
declare arraystretch{'e : Dform} : Dform
declare multicolumn[cols,align]{'t : Dform} : Dform

declare math_array[placement,tags]{'t : Dform} : Dform
declare math_tabular[placement,tags]{'t : Dform} : Dform
declare math_array[tags]{'t : Dform} : Dform
declare math_tabular[tags]{'t : Dform} : Dform
declare math_line{'t : Dform} : Dform
declare math_item{'t : Dform} : Dform
declare math_cr : Dform
declare math_hline : Dform
declare math_arraystretch{'e : Dform} : Dform
declare math_multicolumn[cols,align]{'t : Dform} : Dform

(*
 * These macros are used to declare rules,
 * and display ruleboxes, and sequents
 *)
declare math_defrule[name]{'args; 'hyps; 'goal} : Dform
declare math_rulebox{'tac; 'args; 'hyps; 'goal} : Dform
declare sequent [dummy_arg] { Dform : Dform >- Dform } : Dform

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
