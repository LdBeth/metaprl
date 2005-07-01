(*
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

open Term_ty_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermSubst

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Perv%t"

(************************************************************************
 * DISPLAY FORMS
 *)

(*
 * Pervasive display forms.
 *)
dform perv_nil_df : "xnil" = `""

dform perv_cons_df : "xcons"{'car; 'cdr} =
   'car 'cdr

dform perv_string_df : "string"[s:s] =
   `"\"" slot[s:s] `"\""

(*
 * For displaying separated lists.
 *)
declare sep_list[sep:s]{'l : Dform} : Dform

dform sep_list_xnil_df : sep_list[sep:s]{xnil} =
   `""

dform sep_list_df1 : sep_list[sep:s]{xcons{'item; xnil}} =
   'item

dform sep_list_df2 : sep_list[sep:s]{xcons{'item1; xcons{'item2; 'rest}}} =
   'item1 slot[sep:s] hspace sep_list[sep:s]{xcons{'item2; 'rest}}

(*
 * Typeclasses.
 *)
dform term_df : except_mode["src"] :: "Term" =
   `"Term"

dform ty_df : except_mode["src"] :: "Ty" =
   `"Ty"

dform prop_df : except_mode["src"] :: "Prop" =
   `"Prop"

dform judgment_df : except_mode["src"] :: "Judgment" =
   `"Judgment"

dform token_df : except_mode["src"] :: "Token" =
   `"Token"

dform dform_df : except_mode["src"] :: "Dform" =
   `"Dform"

dform ignore_df : except_mode["src"] :: "Ignore" =
   `"Ignore"

dform sequent_arg_df : except_mode["src"] :: "ty_sequent_arg" =
   `"ty_sequent_arg"

dform sequent_type_df : except_mode["src"] :: "ty_sequent"{'ty_hyp; 'ty_concl; 'ty_seq} =
   pushm[3] szone
   `"SequentType" hspace pushm[3] `"{" 'ty_hyp `">-" 'ty_concl popm hspace `"}"
   popm ezone

dform hyp_type_df : except_mode["src"] :: ty_hyp{'ty_var; 'ty_hyp} =
   pushm[3] szone 'ty_var `" :" hspace 'ty_hyp ezone popm

dform all_type_df : except_mode["src"] :: ty_exists{'ty1; v. 'ty2} =
   pushm[3] szone
   `"TyExists(" slot{'v} `":" hspace slot{'ty1} `"." hspace slot{'ty2} `")"
   popm ezone

(************************************************************************
 * Terms produced by the type checker.
 * ---These are *private terms*---
 * Do not add them to the inferface perv.mli.
 *)
declare ty_var{'v}
declare ty_term{'t}

declare ty_so_cvars{'cvars}
declare ty_so_args{'args}
declare ty_so_var{'cvars; 'args; 'ty}

declare ty_context_cvars{'cvars}
declare ty_context_args{'args}
declare ty_context_var{'cvars; 'args; 'ty}

declare ty_sequent_context_cvars{'cvars}
declare ty_sequent_context_args{'args}
declare ty_sequent_context_var{'cvars; 'args; 'ty}

declare constrain{'arg; 'ty}

dform ty_var_df : ty_var{'v} =
   `"TypeVar(" 'v `")"

dform ty_term_df : ty_term{'t} =
   `"TypeTerm(" 't `")"

dform ty_so_cvars_df : ty_so_cvars{'cvars} =
   `"<|" sep_list[","]{'cvars} `"|>"

dform ty_so_args_df : ty_so_args{'args} =
   `"[" sep_list[";"]{'args} `"]"

dform ty_so_var_df : ty_so_var{'cvars; 'args; 'e} =
   pushm[3] szone
   `"TypeSoVar("
      pushm[3] szone 'cvars ezone popm `","
      hspace pushm[3] szone 'args ezone popm `","
      hspace slot{'e}
   `")"
   popm ezone


dform ty_context_cvars_df : ty_context_cvars{'cvars} =
   `"<|" sep_list[","]{'cvars} `"|>"

dform ty_context_args_df : ty_context_args{'args} =
   `"[" sep_list[";"]{'args} `"]"

dform ty_context_var_df : ty_context_var{'cvars; 'args; 'e} =
   pushm[3] szone
   `"TypeCVar("
      pushm[3] szone 'cvars ezone popm `","
      hspace pushm[3] szone 'args ezone popm `","
      hspace slot{'e}
   `")"
   popm ezone


dform ty_sequent_context_cvars_df : ty_sequent_context_cvars{'cvars} =
   `"<|" sep_list[","]{'cvars} `"|>"

dform ty_sequent_context_args_df : ty_sequent_context_args{'args} =
   `"[" sep_list[";"]{'args} `"]"

dform ty_sequent_context_var_df : ty_sequent_context_var{'cvars; 'args; 'e} =
   pushm[3] szone
   `"TypeCVar("
      pushm[3] szone 'cvars ezone popm `","
      hspace pushm[3] szone 'args ezone popm `","
      hspace slot{'e}
   `")"
   popm ezone

(************************************************************************
 * Term operations.
 *)
let bind_opname = opname_of_term <<"bind"{x. 'b}>>

let is_bind1_term = is_dep1_term bind_opname
let mk_bind1_term = mk_dep1_term bind_opname
let dest_bind1 = dest_dep1_term bind_opname

let is_bind2_term = is_dep2_term bind_opname
let mk_bind2_term = mk_dep2_term bind_opname
let dest_bind2 = dest_dep2_term bind_opname

let dform_term = << Dform >>

let is_dform_bterm bt =
   alpha_equal bt.ty_bterm dform_term || List.exists (alpha_equal dform_term) bt.ty_bvars

let is_dform_type t =
   alpha_equal t.ty_type dform_term || List.exists is_dform_bterm t.ty_bterms

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
