(*
 * @begin[doc]
 * @module[Perv]
 *
 * The @hrefmodule[Perv] module defines some basic built-in terms used
 * by the @MetaPRL compiler.
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
 *
 * @end[license]
 *)

open Lm_debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermOp

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Perv%t"

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

(*
 * @begin[doc]
 * @terms
 *
 * The @tt{nil} and @tt{cons} terms are used to represent abstract
 * lists.  The lists are used internally by the @MetaPRL compiler to
 * represent collections of syntax.
 * @end[doc]
 *)
declare "nil"
declare "cons"{'car; 'cdr}

(*
 * @begin[doc]
 * The @tt{string} term is used internally by the @MetaPRL compiler
 * to represent strings.
 * @end[doc]
 *)
declare "string"[s:s]

(*
 * @begin[doc]
 * The @tt{bind} term is used internally by the @MetaPRL
 * to represent generic variable binding.
 * @end[doc]
 *)
declare "bind"{x. 'b}
declare "bind"{x,y. 'b}
declare "bind"{x,y,z. 'b}
declare "bind"{x,y,z,u. 'b}
declare "bind"{x,y,z,u,v. 'b}
declare "bind"{x,y,z,u,v,w. 'b}
declare "bind"{x,y,z,u,v,w,t. 'b}
declare "bind"{x,y,z,u,v,w,t,s. 'b}

let bind_opname = opname_of_term <<"bind"{x. 'b}>>

let is_bind1_term = is_dep1_term bind_opname
let mk_bind1_term = mk_dep1_term bind_opname
let dest_bind1 = dest_dep1_term bind_opname

let is_bind2_term = is_dep2_term bind_opname
let mk_bind2_term = mk_dep2_term bind_opname
let dest_bind2 = dest_dep2_term bind_opname
(*
let is_bind3_term = is_dep3_term bind_opname
let mk_bind3_term = mk_dep3_term bind_opname
let dest_bind3 = dest_dep3_term bind_opname

let is_bind4_term = is_dep4_term bind_opname
let mk_bind4_term = mk_dep4_term bind_opname
let dest_bind4 = dest_dep4_term bind_opname

let is_bind5_term = is_dep5_term bind_opname
let mk_bind5_term = mk_dep5_term bind_opname
let dest_bind5 = dest_dep5_term bind_opname
*)

(*
 * @begin[doc]
 * The @tt{hyp} and @tt{concl} terms are used to represent
 * the parts of a sequent for the purposes of display.  Internally,
 * the @MetaPRL compiler uses an optimized representation of
 * sequents.
 * @end[doc]
 *)
declare "hyp"{'A; x. 'B}
declare "concl"{'A; 'B}

(*
 * @begin[doc]
 * The @tt{rewrite} term is used to represent a computational equivalence.
 * The @MetaPRL{} refiner uses a proof of a judgment of the form
 * << "rewrite"{'redex; 'contractum} >> to establish computation equivalence.
 * @end[doc]
 *)
declare "rewrite"{'redex; 'contractum}
(* @docoff *)

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

(*
 * Pervasive display forms.
 *)
dform perv_nil_df : "nil" = `""

dform perv_cons_df : "cons"{'car; 'cdr} =
   slot{'car} slot{'cdr}

dform perv_string_df : "string"[s:s] =
   `"\"" slot[s:s] `"\""

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
