(*
 * Characters in the nuprl font.
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * Modified By: Alexei Kopylov @email{kopylov@cs.cornell.edu}
 * Modified By: Xin Yu @email{xiny@cs.caltech.edu}
 *)

extends Perv

(*
 * Display control.
 *)
declare pagebreak : Dform

(*
 * Fonts.
 * Each of these can be used with a string, with a term,
 * or unbalanced.
 *
 * For example, all of these are the same:
 *    bold["["]
 *    bold{"["}
 *    bold_begin "[" bold_end
 *)
declare info[name:s] : Dform
declare info_begin : Dform
declare info_end : Dform
declare keyword[name:s] : Dform
declare bf[name:s] : Dform
declare bf{'t : Dform} : Dform
declare i[name:s] : Dform
declare i{'t : Dform} : Dform
declare it[name:s] : Dform
declare it{'t : Dform} : Dform
declare math_it{'t : Dform} : Dform
declare math_it[text:s] : Dform
declare em[s:s] : Dform
declare em{'s : Dform} : Dform
declare emph{'t : Dform} : Dform
declare tt[name:s] : Dform
declare tt{'t : Dform} : Dform
declare url[url:s] : Dform
declare sub{'t : Dform} : Dform
declare sub[name:s] : Dform
declare sub_begin : Dform
declare sub_end : Dform
declare sub[i:l] : Dform
declare sup{'t : Dform} : Dform
declare sup_begin : Dform
declare sup_end : Dform
declare sup[name:s] : Dform
declare small[name:s] : Dform
declare small{'t : Dform} : Dform
declare esquash{'t : Dform} : Dform
declare atomic[text:s] : Dform
declare monospaced[name:s] : Dform
declare monospaced{'t : Dform} : Dform
declare monospaced_begin : Dform
declare monospaced_end : Dform
declare underline_begin : Dform
declare underline_end : Dform
declare underline{'t : Dform} : Dform

(*
 * HTML control.
 *)
declare html[content:s] : Dform
declare cd_begin[command:s] : Dform
declare cd_end : Dform
declare html_anchor[name:s]{'body : Dform} : Dform
declare html_head[l:n]{'body : Dform} : Dform

(*
 * TeX control.
 *)
declare text{'t : Dform} : Dform
declare mathBB[name:s] : Dform
declare ensuremath[name:s] : Dform
declare ensuremath{'t : Dform} : Dform
declare mathmacro[name:s] : Dform

(* Displays *)
declare mathbbA : Dform
declare mathbbB : Dform
declare mathbbC : Dform
declare mathbbD : Dform
declare mathbbE : Dform
declare mathbbF : Dform
declare mathbbG : Dform
declare mathbbH : Dform
declare mathbbI : Dform
declare mathbbJ : Dform
declare mathbbK : Dform
declare mathbbL : Dform
declare mathbbM : Dform
declare mathbbN : Dform
declare mathbbO : Dform
declare mathbbP : Dform
declare mathbbQ : Dform
declare mathbbR : Dform
declare mathbbS : Dform
declare mathbbT : Dform
declare mathbbU : Dform
declare mathbbV : Dform
declare mathbbW : Dform
declare mathbbX : Dform
declare mathbbY : Dform
declare mathbbZ : Dform

declare shortLeftarrow : Dform
declare Leftarrow : Dform
declare Middlearrow : Dform
declare shortRightarrow : Dform
declare Rightarrow : Dform
declare Leftrightarrow : Dform
declare ulcorner : Dform
declare urcorner : Dform
declare mid : Dform
declare vdash : Dform
declare integral : Dform
declare cdot : Dform
declare downarrow : Dform
declare uparrow : Dform
declare alpha : Dform
declare beta : Dform
declare pi : Dform
declare lambda : Dform
declare gamma : Dform
declare delta : Dform
declare rho : Dform
declare sigma : Dform
declare epsilon : Dform
declare eta : Dform
declare theta : Dform
declare iota : Dform
declare kappa : Dform
declare mu : Dform
declare nu : Dform
declare omicron : Dform
declare tau : Dform
declare phi : Dform
declare xi : Dform
declare omega : Dform
declare wedge : Dform
declare tneg : Dform
declare member : Dform
declare plusminus : Dform
declare oplus : Dform
declare tensor : Dform
declare infty : Dform
declare partial : Dform
declare cap : Dform
declare cup : Dform
declare forall : Dform
declare "exists" : Dform
declare oinfty : Dform
declare shortleftrightarrow : Dform
declare shortleftarrow : Dform
declare shortrightarrow : Dform
declare longleftrightarrow : Dform
declare longleftarrow : Dform
declare longrightarrow : Dform
declare neq : Dform
declare sim : Dform
declare cong : Dform
declare le : Dform
declare ge : Dform
declare equiv : Dform
declare vee : Dform
declare perp : Dform
declare leftarrow : Dform
declare middlearrow : Dform
declare rightarrow : Dform
declare vartriangleleft : Dform
declare vartriangleright : Dform
declare Gamma : Dform
declare Delta : Dform
declare Lambda : Dform
declare Pi : Dform
declare Sigma : Dform
declare Omega : Dform
declare times : Dform
declare "div" : Dform
declare circ : Dform
declare supplus : Dform
declare supminus : Dform
declare supcirc : Dform
declare "subset" : Dform
declare supset : Dform
declare subseteq : Dform
declare supseteq : Dform
declare sqsubset : Dform
declare sqsupset : Dform
declare sqsubseteq : Dform
declare sqsupseteq : Dform
declare subzero : Dform
declare subone : Dform
declare subtwo : Dform
declare subthree : Dform
declare suba : Dform
declare subb : Dform
declare subc : Dform
declare subd : Dform
declare sube : Dform
declare subf : Dform
declare subg : Dform
declare subh : Dform
declare subi : Dform
declare subj : Dform
declare subk : Dform
declare subl : Dform
declare subm : Dform
declare subn : Dform
declare subo : Dform
declare subp : Dform
declare subq : Dform
declare subr : Dform
declare subs : Dform
declare subt : Dform
declare subu : Dform
declare subv : Dform
declare subw : Dform
declare subx : Dform
declare suby : Dform
declare subz : Dform
declare math_div : Dform

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
