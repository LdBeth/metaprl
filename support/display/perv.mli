(*
 * These are the public pervasive terms.
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
open Lm_symbol
open Refiner.Refiner.TermType

declare "nil"
declare "cons"{'car; 'cdr}
declare "string"[s:s]
declare "hyp"{'A; x. 'B}
declare "concl"{'A; 'B}
declare "rewrite"{'redex; 'contractum}
declare "bind"{x. 'b}
declare "bind"{x,y. 'b}
declare "bind"{x,y,z. 'b}
declare "bind"{x,y,z,u. 'b}
declare "bind"{x,y,z,u,v. 'b}
declare "bind"{x,y,z,u,v,w. 'b}
declare "bind"{x,y,z,u,v,w,t. 'b}
declare "bind"{x,y,z,u,v,w,t,s. 'b}
declare sbreak[yes, no]
declare cbreak[yes, no]
declare hbreak[yes, no]
declare space
declare hspace
declare newline
declare lzone
declare szone
declare hzone
declare izone
declare azone
declare ezone
declare tzone[tag]
declare pushm[n:n]
declare pushm[s]
declare pushm (* = pushm[0] *)
declare popm
declare pushfont[font]
declare popfont
declare slot[raw, s]
declare slot[s]
declare slot[l:l]
declare slot[tok:t]
declare slot[n:n]
declare slot[v:v]
declare slot[eq]{'t}
declare slot{'t}
declare parens
declare mode[mode]
declare except_mode[mode]
declare "prec"[p]

val mk_bind1_term : var -> term -> term
val is_bind1_term : term -> bool
val dest_bind1 : term -> var * term

val mk_bind2_term : var -> var -> term -> term
val is_bind2_term : term -> bool
val dest_bind2 : term -> var * var * term

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
