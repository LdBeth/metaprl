(*
 * Unification for Term_ds
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
 * Copyright (C) 2000 Vladimir N. Krupski, Moscow State University
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
 * Author: Vladimir N. Krupski <krupski@lpcs.math.msu.ru>
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

(*
 *    MM-unification deals with the first order unification problems of the form
 *               T1_i = T2_i  , i=1,2,...
 *    for terms with bindings from Term_ds.TermType.term. The problems are members
 *    of eqnlist type. The unification is treated as a transformation of
 *    an arbitrary unification problem into an equivalent  problem in the
 *    "solved" form:
 *           x1=F1(x2,...,xm)
 *           x2=F2(x3,...,xm)
 *           ...
 *    If it is impossible then the exceptions Cycle or Clash are raised.
 *    The conversion of a unification problem into its mgu always
 *    implies  the transformation into the "solved" form (N*log N) and
 *    the calculation of the product of substitutions which may be more
 *    expensive (m^2). The internal representation gives some speed-up
 *    (5-10 times faster) but failes to reduce the order.
 *       Extract the mgu only when you need it!
 *       Use unify_eqnl_eqnl for iterative calls.
 *       The unifiable* functions are much faster!
 *       In the negative case all the functions run in the same time!
 *       Use  eqnlist2ttlist if you need the unification problem as is.
 *)

open Lm_symbol

open Refine_error_sig
open Term_ds_sig
open Term_ds

open Refiner.Refiner.TermType

type term_subst = (var * term) list
type eqnlist

val eqnlist_empty : eqnlist
val eqnlist_append_eqn : eqnlist -> term -> term -> eqnlist
val eqnlist_append_var_eqn : var -> term -> eqnlist -> eqnlist
val eqnlist_append_eqns : eqnlist -> (term*term) list -> eqnlist
val eqnlist2ttlist : eqnlist -> (term*term) list

val new_eqns_var : eqnlist -> var -> var

(*
 * The SymbolSet.t argument below specifies the set of constants
 * that can not be substituted for
 *)

val unifiable : term -> term -> SymbolSet.t -> bool
val unifiable_eqnl : eqnlist -> SymbolSet.t -> bool

val unify : term -> term -> SymbolSet.t -> term_subst
val unify_eqnl : eqnlist -> SymbolSet.t -> term_subst
val unify_eqnl_eqnl : eqnlist -> SymbolSet.t -> eqnlist
