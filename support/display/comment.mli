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

(*
 * @begin[doc]
 * @terms
 * Basic comment structure.
 * @end[doc]
 *)
declare comment_white
declare comment_string[s:s]
declare comment_term{'t}
declare comment_block{'t}

(*
 * @begin[doc]
 * These are the valid comment blocks.
 * @end[doc]
 *)
declare "doc"{'t}
declare license{'t}
declare spelling{'t}
declare misspelled{'t}

(*
 * These terms are used by the summary module for display forms.
 *)
declare prl_comment{'t}
declare tex_comment{'t}

(*
 * @begin[doc]
 * @terms
 * The @theory{'t} term produces a chapter header for a collection of modules
 * and the @module[name:s] term produces a section header for the current module.
 * @end[doc]
 *)
declare "theory"{'t}
declare "module"[name:s]
declare "module"{'name}

(*
 * @begin[doc]
 * Bookmaking commands.
 * @end[doc]
 *)
declare chapter[name:s]{'t}
declare section[name:s]{'t}
declare subsection[name:s]{'t}
declare subsubsection[name:s]{'t}

(*
 * @begin[doc]
 * The @modsection{'t} term prduces a subsection header.
 * @end[doc]
 *)
declare modsection{'t}

(*
 * @begin[doc]
 * The @modsubsection{'t} term prduces a subsection header.
 * @end[doc]
 *)
declare modsubsection{'t}

(*
 * Generic targets.
 *)
declare target[name:s]{'t}
declare hreftarget[name:s]

(*
 * Other forms of sectioning commands.
 *)
declare parents
declare rewrites
declare rules
declare convs
declare tactics
declare resources
declare terms

(*
 * Hypertext targets.
 *)
declare "term"[name:s]
declare "resource"[name:s]
declare "tactic"[name:s]
declare "conv"[name:s]
declare "rule"[name:s]
declare "rewrite"[name:s]

(*
 * Hypertext links.
 *)
declare hrefmodule[name:s]
declare hrefterm[name:s]
declare hrefresource[name:s]
declare hrefrewrite[name:s]
declare hreftactic[name:s]
declare hrefconv[name:s]
declare hrefrule[name:s]

declare refchapter[name:s]
declare refsection[name:s]
declare refsubsection[name:s]
declare refsubsubsection[name:s]

declare refmodule[name:s]
declare refterm[name:s]
declare refresource[name:s]
declare refrewrite[name:s]
declare reftactic[name:s]
declare refconv[name:s]
declare refrule[name:s]
declare reffigure[name:s]

(*
 * TeX structuring.
 *)
declare "begin"[name:s]
declare "end"[name:s]

(*
 * TeX control.
 *)
declare docoff
declare noindent
declare cite[s:s]

(*
 * Special terms.
 *)
declare "MetaPRL"
declare "Nuprl"
declare "NuPRL"
declare "OCaml"
declare "LaTeX"
declare "MartinLof"

(*
 * @begin[doc]
 * The @code{phantom} term produces white space, equivalent in width
 * to the term being typeset.
 * @end[doc]
 *)
declare phantom{'t}

(*
 * Formatting.
 *)
declare math[s:s]
declare math{'t}
declare centermath[s:s]
declare centermath{'t}

declare code[text:s]
declare verbatim[text:s]
declare email[text:s]
declare center{'t}
declare figure[label:s]{'t}
declare caption{'caption}
declare quote{'t}
declare quotation{'t}
declare footnote{'t}
declare enumerate{'t}
declare itemize{'t}
declare description{'t}
declare item{'t}
declare item{'label; 'body}

(*
 * Other macros.
 *)
declare lbrace
declare rbrace
declare comment[who:s]{'e}

(************************************************************************
 * MATH MODE                                                            *
 ************************************************************************)

(*
 * Toplevel forms.
 *)
declare math_misspelled{'t}

(*
 * Allow raw printing.
 *)
declare math_slot[tag:s]{'t}

(*
 * Font control in math mode.
 *)
declare math_hbox{'t}
declare math_mbox{'t}

declare math_bb{'T}
declare math_tt[TEXT:s]
declare math_tt{'t}
declare math_bf{'t}
declare math_bf[text:s]
declare math_i{'t}
declare math_i[text:s]
declare math_emph{'t}
declare math_mathop{'t}
declare math_mathrel[text:s]
declare math_mathrel{'t}

(*
 * Math symbols.
 *)
declare math_Type

declare math_colon
declare math_rightarrow
declare math_Rightarrow
declare math_leftarrow
declare math_Leftarrow
declare math_leftrightarrow
declare math_Leftrightarrow
declare math_longrightarrow
declare math_longleftrightarrow

declare math_le
declare math_ge
declare math_wedge
declare math_vee
declare math_phi
declare math_varphi
declare math_cap
declare math_cup
declare math_bigcap
declare math_bigcup
declare math_in
declare math_cdot
declare math_cdots
declare math_vdots
declare math_ldots
declare math_subset
declare math_subseteq
declare math_times
declare math_equiv
declare math_space
declare math_neg
declare math_neq
declare math_forall
declare math_exists
declare math_alpha
declare math_beta
declare math_lambda
declare math_epsilon
declare math_Gamma
declare math_vdash
declare math_int
declare math_lbrace
declare math_rbrace
declare math_quad
declare math_qquad
declare math_bullet
declare math_left[s]
declare math_right[s]

declare math_vec{'e}
declare math_underbrace{'e}
declare math_underbrace{'e1; 'e2}

(*
 * Sub/superscripts.
 *)
declare math_subscript{'t1; 't2}
declare math_superscript{'t1; 't2}

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
declare tabular[placement,tags]{'t}
declare tabular[tags]{'t}
declare line{'t}
declare cr
declare hline
declare arraystretch{'e}
declare multicolumn[cols,align]{'t}

declare math_array[placement,tags]{'t}
declare math_tabular[placement,tags]{'t}
declare math_array[tags]{'t}
declare math_tabular[tags]{'t}
declare math_line{'t}
declare math_item{'t}
declare math_cr
declare math_hline
declare math_arraystretch{'e}
declare math_multicolumn[cols,align]{'t}

(*
 * These macros are used to declare rules,
 * and display ruleboxes.
 *)
declare math_defrule[name]{'args; 'hyps; 'goal}
declare math_rulebox{'tac; 'args; 'hyps; 'goal}

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
