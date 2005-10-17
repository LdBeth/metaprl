(*
 * These are the public pervasive terms.
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
open Refiner.Refiner.TermTy

(************************************************************************
 * Typeclasses.
 *)

(*
 * All normal terms should be in the @tt{Term} typeclass.
 *)
declare typeclass Term

(*
 * The @tt{Ignore} typeclass should be used for terms that
 * should never be used anywhere.
 *)
declare typeclass Ignore <- "Term"

(*
 * Quoted terms belong to the @tt{Quote} typeclass.
 *)
declare typeclass Quote -> Term

(*
 * The @tt{Dform} typeclass should be used for terms that are used
 * in display forms, but not anywhere else.  Every term can be displayed.
 *)
declare typeclass Dform <- Term

(*
 * Terms that describe types are in the @tt{Ty} typeclass.
 *)
declare typeclass Ty -> Term

(*
 * The @tt{Prop} typeclass is for terms that represent propositions.
 *)
declare typeclass Prop -> Term

(*
 * The sentences in a rule should be in the typeclass @{Judgment}.
 *)
declare typeclass Judgment -> Dform

(*
 * Tokens belong to the @tt{Token} typeclass.
 *)
declare typeclass Token

(*
 * The type of lexers.
 *)
declare typeclass Lexer

(*
 * Grammar terms.
 *)
declare typeclass Precedence
declare typeclass Nonterminal -> Term
declare typeclass Terminal -> Nonterminal
(* @docoff *)

(*
 * Type constraint.
 *)
declare ty_constrain{'e : Term; 'a : Ty} : 'a

(* Sequents -- these are internal terms, and should not be used directly *)
declare typeclass ty_sequent_arg -> Dform
declare typeclass ty_hyp -> Ty
declare type ty_sequent{'ty_hyp : ty_hyp; 'ty_concl : Ty; 'ty_seq : Ty} : ty_sequent_arg
declare type ty_hyp{'ty_var : Ty; 'ty_hyp : Ty} : ty_hyp
declare type ty_exists{'a : Ty; v : 'a. 'ty['v] : ty_hyp} : ty_hyp

(************************************************************************
 * Builtin input forms.
 *)

(*
 * For constructing a second-order variables and contexts.
 * The arguments are of type Dform because they are xlists.
 *
 * xsovar[v:v]{[v1; ...; vn]; [t1; ...; tn]} is rewritten to
 *     v<|v1, ..., vn|>[t1; ...; tn]
 *
 * A hyp of the form
 *    xhypcontext[C:v]{[v1; ...; vn]; [t1; ...; tn]}
 * is rewritten to
 *    <C<|v1, ..., vn|>[t1; ...; tn]>
 *)
declare xsovar[v:v]{'contexts : Dform; 'args : Dform} : 'a
declare xcontext[v:v]{'contexts : Dform; 'args : Dform} : 'a
declare xhypcontext[v:v]{'contexts : Dform; 'args : Dform} : 'a

(*
 * For constructing terms.  We will eventually need to extend these
 * with parameters.
 *)
declare xbterm{x : 'a. 'e : Dform} : Dform
declare xterm[op:s]{'bterms : Dform} : 'a

(*
 * Quotations.
 *)
declare xquotation[name:s, quote:s] : 'a

(************************************************************************
 * Normal terms.
 *)

(*
 * @tt{xoncl} is the ``null'' conclusion.  It is the term used for the conclusion
 * of any sequent that is written without a conclusion.
 *)
declare xconcl : Term

(*
 * @terms
 *
 * The @tt{nil} and @tt{cons} terms are used to represent abstract
 * lists.  The lists are used internally by the @MetaPRL compiler to
 * represent collections of syntax.  Externally, the elements of the
 * list must be display forms.
 *)
declare "xnil" : Dform
declare "xcons"{'car : Dform; 'cdr : Dform} : Dform

(*
 * The @tt{string} term is used internally by the @MetaPRL compiler
 * to represent strings.
 *)
declare "string"[s:s]

(*
 * The @tt{bind} term is used internally by the @MetaPRL
 * to represent generic variable binding.
 *)
declare "bind"{a. 'z}
declare "bind"{a, b. 'z}
declare "bind"{a, b, c. 'z}
declare "bind"{a, b, c, d. 'z}
declare "bind"{a, b, c, d, e. 'z}
declare "bind"{a, b, c, d, e, f. 'z}
declare "bind"{a, b, c, d, e, f, g. 'z}

(*
 * The @tt{xbinder} term is used to specify first-order variables in rules
 * and rewrites.
 *)
declare xbinder{'e : 'a}

(*
 * The @tt{hyp} and @tt{concl} terms are used to represent
 * the parts of a sequent for the purposes of display.  Internally,
 * the @MetaPRL compiler uses an optimized representation of
 * sequents.
 *)
declare "hyp"{'A; x. 'B} : Dform
declare "concl"{'A; 'B} : Dform

(*
 * The @tt{rewrite} term is used to represent a computational equivalence.
 * The @MetaPRL{} refiner uses a proof of a judgment of the form
 * << "rewrite"{'redex; 'contractum} >> to establish computation equivalence.
 *)
declare "rewrite"{'redex; 'contractum} : Term

(*
 * Terms used in display forms.
 *)
declare sbreak[yes, no] : Dform
declare cbreak[yes, no] : Dform
declare hbreak[yes, no] : Dform
declare space  : Dform
declare hspace : Dform
declare newline : Dform
declare lzone : Dform
declare szone : Dform
declare hzone : Dform
declare izone : Dform
declare azone : Dform
declare ezone : Dform
declare tzone[tag] : Dform
declare pushm[n:n] : Dform
declare pushm[s] : Dform
declare pushm (* = pushm[0] *) : Dform
declare popm : Dform
declare pushfont[font] : Dform
declare popfont : Dform
declare slot[raw, s] : Dform
declare slot[s] : Dform
declare slot[l:l] : Dform
declare slot[tok:t] : Dform
declare slot[n:n] : Dform
declare slot[v:v] : Dform
declare slot[sh:sh] : Dform
declare slot[sh:op] : Dform
declare slot[eq]{'t : Dform} : Dform
declare slot{'t : Dform} : Dform

declare parens : Dform
declare except_mode[mode] : Dform
declare mode[mode] : Dform
declare "prec"[p] : Dform

(* ML utilities *)
val mk_bind1_term : var -> term -> term
val is_bind1_term : term -> bool
val dest_bind1    : term -> var * term

val mk_bind2_term : var -> var -> term -> term
val is_bind2_term : term -> bool
val dest_bind2    : term -> var * var * term

(* Whether a term declaration mentions Dform *)
val is_dform_type : ty_term -> bool

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
