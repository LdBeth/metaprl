(*
 * Display forms for patterns are a problem, because the innermost term is the
 * term to be evaluated under all the pattern bindings.
 * We contruct the pattern term and extract the inner expression.
 * Patterns are collected on a stack.
 *
 * Here is how a "let" expression looks:
 *    1. let : (p1, e1), ..., (pn, en), e
 *       let{pl; el}, where
 *       pl = patt_arg{...; patt_end{... patt_in{e}}}
 *       el = ocons{e1; ocons{e2; ... onil}}
 *
 * Here is how a "match" expression looks:
 *    2. match e, (p1, w1, e1), ..., (pn, wn, en)
 *       match{patt_ifelse{pwe1; patt_ifelse{pwe2; ... patt_if{pwen}}}}
 *       where
 *          pwe = patt_* ... {we}
 *          we  = patt_with{w; e} | patt_body{e}
 *
 * Here is how a "let rec" expression looks:
 * The arguments are _inside_ the pattern.
 *    3. letrec (p1, e1), ..., (pn, en), e
 *       fix{x. let{pl; x}}, where
 *       pl = patt_arg{...; patt_end{... patt_match{e1; ... patt_in{e}}}}
 *
 * BUG!!! nogin: the actual code in filter/base/filter_ocaml does
 * something else, there is no bound variable in the fix term in creates!
 *
 * These three forms are different, but we can combine their
 * display forms somewhat.
 *
 * Display form terms have three parts.
 *    1. opname represents the key: (patt_format usually)
 *    2. first arg represents the current pattern
 *    3. second arg represents stack of terms representing the pattern
 *       being constructed
 *
 * For the "let" form, we initialize the stack with the list "el".
 *
 * BUG!!! I edited a lot of these display forms when I added
 * the argument-checking code to parser, but haven't really tested them.
 * I am pretty sure I broke some of the display forms and they would have to
 * be fixed if we want them back.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

extends Ocaml
extends Ocaml_base_df

(*
 * Special forms.
 *)
declare "as"{'a : TyOCaml;'b : TyOCaml} : TyOCaml
declare wildcard : TyOCaml
declare choice{'a : TyOCaml;'b : TyOCaml} : TyOCaml
declare range{'a : TyOCaml;'b : TyOCaml} : TyOCaml
declare patt_let : TyOCaml
declare patt_fix : TyOCaml
declare patt_reverse : TyOCaml
declare patt_else : TyOCaml
declare patt_char[s:s]{'p1 : TyOCaml} : TyOCaml
declare patt_int[i:n]{'p1 : TyOCaml} : TyOCaml
declare patt_string[s:s]{'p1 : TyOCaml} : TyOCaml
declare patt_float[s:s]{'p1 : TyOCaml} : TyOCaml

(*
 * Final form.
 *)
dform patt_null_df1 : patt_format{onil; onil} =
   `""

dform patt_null_df2 : patt_format{onil; patt_else} =
   `""

(*
 * Constant.
 *)
dform patt_char_df1 : patt_format{patt_char[c:s]{'p1}; 'p2} =
   patt_format{'p1; ocons{."char"[c:s]; 'p2}}

dform patt_int_df1 : patt_format{patt_int[i:n]{'p1}; 'p2} =
   patt_format{'p1; ocons{."int"[i:n]; 'p2}}

dform patt_string_df1 : patt_format{patt_string[s:s]{'p1}; 'p2} =
   patt_format{'p1; ocons{."string"[s:s]; 'p2}}

dform patt_float_df1 : patt_format{patt_float[f:s]{'p1}; 'p2} =
   patt_format{'p1; ocons{."float"[f:s]; 'p2}}

dform patt_char_df2 : patt_format{patt_char[start:n, finish:n, c:s]{'p1}; 'p2} =
   patt_format{patt_char[c:s]{'p1}; 'p2}

dform patt_int_df2 : patt_format{patt_int[start:n, finish:n, i:n]{'p1}; 'p2} =
   patt_format{patt_int[i:n]{'p1}; 'p2}

dform patt_string_df2 : patt_format{patt_string[start:n, finish:n, s:s]{'p1}; 'p2} =
   patt_format{patt_string[s:s]{'p1}; 'p2}

dform patt_float_df2 : patt_format{patt_float[start:n, finish:n, x:s]{'p1}; 'p2} =
   patt_format{patt_float[x:s]{'p1}; 'p2}

(*
 * Binding.
 *)
dform patt_var_df1 : patt_format{patt_var{x. 'p1}; 'p2} =
   patt_format{'p1; ocons{'x; 'p2}}

dform patt_var_df3 : patt_format{patt_var[start:n, finish:n]{x. 'p1}; 'p2} =
   patt_format{patt_var{x. 'p1}; 'p2}

dform patt_uid_df1 : patt_format{patt_uid{patt_uid[name:s]; 'p1}; 'p2} =
   patt_format{'p1; ocons{'name; 'p2}}

dform patt_uid_df2 : patt_format{patt_uid[start:n, finish:n]{'p1; 'p2}; 'p3} =
   patt_format{patt_uid{'p1; 'p2}; 'p3}

(*
 * Projection.
 *)
dform patt_proj_df1 : patt_format{patt_proj{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_proj_arg_df1 : patt_format{patt_proj_arg{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_proj_end_df1 : patt_format{patt_proj_end{'p1}; ocons{'p2; ocons{'p3; 'p4}}} =
   patt_format{'p1; ocons{proj{'p2; 'p3}; 'p4}}

dform patt_proj_df2 : patt_format{patt_proj[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_proj{'p1}; 'p2}

dform patt_proj_arg_df2 : patt_format{patt_proj_arg[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_proj_arg{'p1}; 'p2}

dform patt_proj_end_df2 : patt_format{patt_proj_end[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_proj_end{'p1}; 'p2}

(*
 * Optional expr.
 *)
(* XXX: HACK: LDB: need to add unique form for label *)
dform patt_poe_df1 : patt_format{some{'e1}; ocons{'p1; 'p2}} =
   slot{'p1} ":" slot{'e1}
   patt_format{'p2; onil}

dform patt_poe_df2 : patt_format{none; ocons{'p1; 'p2}} =
   slot{'p1}
   patt_format{'p2; onil}

(*
 * Simultaneous match.
 *)
dform patt_as_df1 : patt_format{patt_as{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_as_arg_df1 : patt_format{patt_as_arg{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_as_end_df1 : patt_format{patt_as_end{'p1}; ocons{'p2; ocons{'p3; 'p4}}} =
   patt_format{'p1; .ocons{."as"{'p2; 'p3}; 'p4}}

dform patt_as_df2 : patt_format{patt_as[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_as{'p1}; 'p2}

dform patt_as_arg_df2 : patt_format{patt_as_arg[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_as_arg{'p1}; 'p2}

dform patt_as_end_df2 : patt_format{patt_as_end[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_as_end{'p1}; 'p2}

(*
 * Wildcard.
 *)
dform patt_wildcard_df1 : patt_format{patt_wildcard{'p1}; 'p2} =
   patt_format{'p1; ocons{wildcard; 'p2}}

dform patt_wildcard_df2 : patt_format{patt_wildcard[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_wildcard{'p1}; 'p2}

dform patt_wildcard_df3 : wildcard =
   "_"

(*
 * Application.
 *)
dform patt_apply_df1 : patt_format{patt_apply{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_apply_arg_df1 : patt_format{patt_apply_arg{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_apply_end_df1 : patt_format{patt_apply_end{'p1}; ocons{'p2; ocons{'p3; 'p4}}} =
   patt_format{'p1; ocons{apply{'p3; 'p2}; 'p4}}

dform patt_apply_df2 : patt_format{patt_apply[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_apply{'p1}; 'p2}

dform patt_apply_arg_df2 : patt_format{patt_apply_arg[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_apply_arg{'p1}; 'p2}

dform patt_apply_end_df2 : patt_format{patt_apply_end[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_apply_end{'p1}; 'p2}

(*
 * Alternates.
 *)
dform patt_choice_df1 : patt_format{patt_choice{'p1}; 'p2} =
   patt_format{'p1; ocons{onil; 'p2}}

dform patt_choice_arg_df1 : patt_format{patt_choice_arg{'p1}; ocons{'p2; 'p3}} =
   patt_format{'p1; ocons{'p2; 'p3}}

dform patt_choice_end_df1 : patt_format{patt_choice_end{'p1}; ocons{'p2; ocons{'p3; 'p4}}} =
   patt_format{'p1; ocons{choice{'p3; 'p2}; 'p4}}

dform patt_choice_df2 :  patt_format{patt_choice[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_choice{'p1}; 'p2}

dform patt_choice_arg_df2 : patt_format{patt_choice_arg[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_choice_arg{'p1}; 'p2}

dform patt_choice_end_df2 : patt_format{patt_choice_end[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_choice_end{'p1}; 'p2}

(*
 * Range of choices.
 *)
dform patt_range_df1 : patt_format{patt_range{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_range_arg_df1 : patt_format{patt_range_arg{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_range_end_df1 : patt_format{patt_range_end{'p1}; ocons{'p2; ocons{'p3; 'p4}}} =
   patt_format{'p1; ocons{range{'p2; 'p3}; 'p4}}

dform patt_range_df2 : patt_format{patt_range[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_range{'p1}; 'p2}

dform patt_range_arg_df2 : patt_format{patt_range_arg[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_range_arg{'p1}; 'p2}

dform patt_range_end_df2 : patt_format{patt_range_end[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_range_end{'p1}; 'p2}

(*
 * List pattern.
 *)
dform patt_list_df1 : patt_format{patt_list{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_list_arg_df1 : patt_format{patt_list_arg{'p1}; ocons{'p2; 'p3}} =
   patt_format{'p1; ocons{'p2; 'p3}}

dform patt_list_end_df1 : patt_format{patt_list_end{'p1}; ocons{'p2; ocons{'p3; 'p4}}} =
   patt_format{'p1; ocons{choice{'p3; 'p2}; 'p4}}

dform patt_list_df2 : patt_format{patt_list[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_list{'p1}; 'p2}

dform patt_list_arg_df2 : patt_format{patt_list_arg[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_list_arg{'p1}; 'p2}

dform patt_list_end_df2 : patt_format{patt_list_end[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_list_end{'p1}; 'p2}

(*
 * Tuple pattern.
 *)
declare patt_tuple{'p1 : TyOCaml; 'p2 : TyOCaml; 'p3 : TyOCaml; 'p4 : TyOCaml} : TyOCaml

dform patt_tuple_df1 : patt_format{patt_tuple{'p1}; 'p2} =
   patt_format{'p1; ocons{onil; 'p2}}

dform patt_tuple_arg_df1 : patt_format{patt_tuple_arg{'p1}; ocons{'p2; ocons{'p3; 'p4}}} =
   patt_format{'p1; ocons{ocons{'p2; 'p3}; 'p4}}

dform patt_tuple_end_df1 : patt_format{patt_tuple_end{'p1}; ocons{'p2; ocons{'p3; 'p4}}} =
   patt_tuple{'p1; ocons{'p2; onil}; 'p3; 'p4}

dform patt_tuple_rev_df1 : patt_tuple{'p1; 'p2; onil; 'p3} =
   patt_format{'p1; ocons{tuple{'p2}; 'p3}}

dform patt_tuple_rev_df2 : patt_tuple{'p1; 'p2; ocons{'p3; 'p4}; 'p5} =
   patt_tuple{'p1; ocons{'p3; 'p2}; 'p4; 'p5}

dform patt_tuple_df2 : patt_format{patt_tuple[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_tuple{'p1}; 'p2}

dform patt_tuple_arg_df2 : patt_format{patt_tuple_arg[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_tuple_arg{'p1}; 'p2}

dform patt_tuple_end_df2 : patt_format{patt_tuple_end[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_tuple_end{'p1}; 'p2}

(*
 * Records.
 *)
dform patt_record_df1 : patt_format{patt_record{patt_record_proj[start:n, finish:n]{'e; 'p1}}; 'p2} =
    patt_format{'p1; ocons{'e; ocons{onil; 'p2}}}

dform patt_record_proj_df1 : patt_format{patt_record_proj{'e1; 'p1}; ocons{'p2; ocons{'e2; ocons{'ee; 'p3}}}} =
   patt_format{'p1; ocons{'e1; ocons{ocons{ee{'e2; 'p2}; 'ee}; 'p3}}}

dform patt_record_end_df1 : patt_format{patt_record_end{'p1}; ocons{'p2; ocons{'e2; ocons{'ee; 'p3}}}} =
   patt_format{'p1; ocons{record{ocons{ee{'e2; 'p2}; 'ee}; none}; 'p3}}

dform patt_record_df2 : patt_format{patt_record[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_record{'p1}; 'p2}

dform patt_record_proj_df2 : patt_format{patt_record_proj[start:n, finish:n]{'e1; 'p1}; 'p2} =
   patt_format{patt_record_proj{'e1; 'p1}; 'p2}

dform patt_record_end_df : patt_format{patt_record_end[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_record_end{'p1}; 'p2}

(*
 * "Let" forms.
 * Clauses are delimited by patt_and.
 * The stack contains one of the following:
 *    patt_let: this is the first clause of a let
 *    patt_and: this is the second or greater clause of a let
 *    patt_fix: this is a clause of a fix
 *)
dform patt_and_df1 : patt_format{patt_and{'p1}; ocons{'p2; ocons{'e; 'el}}} =
   pushm[0] slot{'p2} `" " "=" hspace slot{'e} popm
   patt_format{'p1; 'el}

dform patt_and_df2 : patt_format{patt_and[start:n, finish:n]{'p1}; 'e1} =
   patt_format{patt_and{'p1}; 'e1}

dform patt_done_df1 : patt_format{patt_done; ocons{'p; ocons{'e; 'el}}} =
   pushm[0] slot{'p} `" " "=" hspace szone{'e} popm

dform patt_done_df2 : patt_format{patt_done[start:n, finish:n]; 'e1} =
   patt_format{patt_done; 'e1}

dform patt_in_df1 : patt_format{patt_in{'e1}; ocons{'p; ocons{'e2; 'el}}} =
   pushm[0] szone{'p} `" " "=" hspace 'e2 popm hspace "_in"
   szone hspace slot{'e1} ezone

dform patt_in_df2 : patt_format{patt_in[start:n, finish:n]{'e1}; 'e2} =
   patt_format{patt_in{'e1}; 'e2}

(*
 * "Fix" forms.
 *)
declare patt_fix_arg : TyOCaml

dform patt_fix_and_df1 : patt_format{patt_fix_and{'p1}; 'p2} =
   patt_format{'p1; 'p2}

dform patt_fix_and_df2 : patt_format{patt_fix_and[start:n, finish:n]{'p1}; 'p2} =
   patt_format{patt_fix_and{'p1}; 'p2}

dform patt_fix_arg_df1 : patt_format{patt_fix_arg{'e1; 'p1}; ocons{'p2; 'p3}} =
   szone pushm[0] slot{'p2} `" " "=" hspace szone{'e1} popm ezone
   patt_format{'p1; ocons{patt_fix_arg; 'p3}}

dform patt_fix_arg_df2 : patt_format{patt_fix_arg{'e1; 'p1}; ocons{patt_fix_arg; ocons{'p2; 'p3}}} =
   newline szone `"and " pushm[0] slot{'p2} `" " "=" hspace szone{'e1} popm ezone
   patt_format{'p1; ocons{patt_fix_arg; 'p3}}

dform patt_fix_arg_df3 : patt_format{patt_fix_arg[start:n, finish:n]{'e1; 'p1}; 'p2} =
   patt_format{patt_fix_arg{'e1; 'p1}; 'p2}

dform patt_done_df3 : patt_format{patt_done; ocons{patt_fix_arg; onil}} =
   `""

dform patt_in_df3 : patt_format{patt_in{'e1}; ocons{patt_fix_arg; onil}} =
   newline "_in" `" " hspace slot{'e1}

(*
 * "Match" forms.
 *)
dform patt_ifelse_df1 : patt_format{patt_ifelse{'pwe; 'pwel}; onil} =
   patt_format{'pwe; 'pwel}

dform patt_if_df1 : patt_format{patt_if{'pwe}; onil} =
   patt_format{'pwe; onil}

dform patt_with_df1 : patt_format{patt_with{'e1; 'e2}; 'pwel} =
   "_with" `" " szone{'e1} "->" `" " szone{'e2}
   patt_format{'pwel; onil}

dform patt_ifelse_df2 : patt_format{patt_ifelse{'pwe; 'pwel}; patt_else} =
   hspace "|" `" " szone patt_format{'pwe; 'pwel} ezone

dform patt_if_df2 : patt_format{patt_if{'pwe}; patt_else} =
   hspace "|" `" " szone patt_format{'pwe; onil} ezone

dform patt_body_df1 : patt_format{patt_body{'e1}; ocons{'e2; 'pwel}} =
   szone{'e2} `" " keyword["->"] hspace szone{'e1}
   patt_format{'pwel; patt_else}

dform patt_ifelse_df3 : patt_format{patt_ifelse[start:n, finish:n]{'pwe; 'pwel}; 'e} =
   patt_format{patt_ifelse{'pwe; 'pwel}; 'e}

dform patt_if_df3 : patt_format{patt_if[start:n, finish:n]{'pwe}; 'e} =
   patt_format{patt_if{'pwe}; 'e}

dform patt_with_df3 : patt_format{patt_with[start:n, finish:n]{'e1; 'e2}; 'e} =
   patt_format{patt_with{'e1;'e2}; 'e}

dform patt_body_df3 : patt_format{patt_body[start:n, finish:n]{'e}; 'pwel} =
   patt_format{patt_body{'e}; 'pwel}

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
