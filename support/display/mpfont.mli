(*
 * Basic font control for display forms.
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
