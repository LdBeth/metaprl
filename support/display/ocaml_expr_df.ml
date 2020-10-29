(*
 * This file contains the primitive syntax and display
 * for ocaml terms.
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

extends Ocaml
extends Ocaml_base_df

(*
 * Special flags.
 *)
declare ident_expr{'expr : TyOCaml} : TyOCaml
declare list_expr{'l : TyOCaml} : TyOCaml
declare se_list{'l : TyOCaml} : TyOCaml
declare ee_list{'l : TyOCaml} : TyOCaml
declare ee_list'{'l : TyOCaml} : TyOCaml
declare e_list{'l : TyOCaml}

(*
 * Precedences.
 *)
prec prec_proj
prec prec_apply
prec prec_cons
prec prec_assign
prec prec_equal
prec prec_if
prec prec_rel
prec prec_not
prec prec_fun
prec prec_let

(*
 * Lists.
 *)
dform ocons_df : ocons{'hd; 'tl} =
   'hd 'tl

dform onil_df : onil =
   `""

(*
 * List concatenation
 *)
dform df_concat_cons : mode[src] :: mode[html] :: mode[prl] :: mode[tex] :: df_concat{'sep; ocons{'hd; 'tl}} =
   slot{'hd} 'sep df_concat{'sep;'tl}

dform df_concat_onil : mode[src] :: mode[html] :: mode[prl] :: mode[tex] :: df_concat{'sep; ocons{'hd; onil}} =
   slot{'hd}

dform df_concat_onil2 : mode[src] :: mode[html] :: mode[prl] :: mode[tex] :: df_concat{'sep; onil} =
   `""

dform df_rev_concat_cons : mode[src] :: mode[html] :: mode[prl] :: mode[tex] :: df_rev_concat{'sep; ocons{'hd; 'tl}} =
   slot{'hd} 'sep df_rev_concat{'sep;'tl}

dform df_rev_concat_onil : mode[src] :: mode[html] :: mode[prl] :: mode[tex] :: df_rev_concat{'sep; ocons{'hd; onil}} =
   slot{'hd}

dform df_rev_concat_onil2 : mode[src] :: mode[html] :: mode[prl] :: mode[tex] :: df_rev_concat{'sep; onil} =
   `""

(*
 * Type hacking.
 *)
declare df_term{'t : Dform} : TyOCaml

dform df_term_df : df_term{'t} =
   't

(*
 * Constants.
 *)
dform char_df1 : "char"[c:s] =
   "'" slot[c:s] "'"

dform char_df2 : "char"[start:n, finish:n, c:s] =
   "char"[c:s]

dform int_df1 : "int"[i:n] =
   slot[i:n]

dform int_df2 : "int"[start:n, finish:n, i:n] =
   "int"[i:n]

dform string_df1 : Ocaml!"string"[s:s] =
   "\"" slot[s:s] "\""

dform string_df2 : Ocaml!"string"[start:n, finish:n, s:s] =
   Ocaml!"string"[s:s]

dform float_df1 : "float"[f:s] =
   slot[f:s]

dform float_df2 : "float"[start:n, finish:n, f:s] =
   "float"[f:s]

dform lid_df1 : "lid"{'v} =
   slot{'v}

dform lid_df2 : "lid"[start:n, finish:n]{'v} =
   "lid"{'v}

dform lid_df3 : "lid"[v:s] =
   slot[v:s]

dform uid_df1 : "uid"{'v} =
   slot{'v}

dform uid_df2 : "uid"[start:n, finish:n]{'v} =
   "uid"{'v}

dform uid_df3 : "uid"[v:s] =
   slot[v:s]

(*
 * Projection.
 *)
dform proj_df1 : parens :: "prec"[prec_proj] :: "proj"{'A; 'B} =
   pushm[0] slot{'A} "." slot{'B} popm

dform proj_df2 : "proj"[start:n, finish:n]{'A; 'B} =
   "proj"{'A; 'B}

(*
 * Application.
 *)
declare "apply"[lid:s]{'e1 : TyOCaml; 'e2 : TyOCaml} : TyOCaml
declare apply_cons_list_parse{'reversed_parsed : TyOCaml; 'tail : TyOCaml} : TyOCaml

dform apply_df1 : parens :: "prec"[prec_apply] :: "apply"{'e1; 'e2} =
   pushm[0] slot{'e1} hspace slot{'e2} popm

dform apply_df2 : "apply"[start:n, finish:n]{'e1; 'e2} =
   "apply"{'e1; 'e2}

dform apply_df3 : "apply"{"apply"{lid{lid[name:s]}; 'e1}; 'e2} =
   szone{"apply"[name:s]{'e1; 'e2}}

dform apply_df4 : "apply"{."apply"[start1:n, finish1:n]{.lid[start2:n, finish2:n]{lid[name:s]}; 'e1}; 'e2} =
   "apply"{."apply"{lid{lid[name:s]}; 'e1}; 'e2}

dform apply_df4 : "apply"{."apply"[start1:n, finish1:n]{.uid[start2:n, finish2:n]{uid[name:s]}; 'e1}; 'e2} =
   "apply"{."apply"{lid{lid[name:s]}; 'e1}; 'e2}

dform apply_any_df : "apply"[name:s]{'e1; 'e2} =
   pushm[0] slot[name:s] hspace slot{'e1} hspace slot{'e2} popm

dform apply_plus_df : "apply"["+"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace "+" hspace slot{'e2} popm

dform apply_minus_df : "apply"["-"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace "-" hspace slot{'e2} popm

dform apply_star_df : "apply"["*"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace "*" hspace slot{'e2} popm

dform apply_slash_df : "apply"["/"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace "/" hspace slot{'e2} popm

dform apply_hat_df : "apply"["^"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["^"] hspace slot{'e2} popm

dform apply_at_df : "apply"["@"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["@"] hspace slot{'e2} popm

dform apply_lt_df : "apply"["<"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["<"] hspace slot{'e2} popm

dform apply_le_df : "apply"["<="]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["<="] hspace slot{'e2} popm

dform apply_eq_df : "apply"["="]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["="] hspace slot{'e2} popm

dform apply_eqeq_df : "apply"["=="]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["=="] hspace slot{'e2} popm

dform apply_ge_df : "apply"[">="]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword[">="] hspace slot{'e2} popm

dform apply_gt_df : "apply"[">"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword[">"] hspace slot{'e2} popm

dform apply_cons_df : "apply"["::"]{'e1; 'e2} =
   apply_cons_list_parse{ocons{df_term{szone{'e1}}; onil}; 'e2}

dform apply_cons_parse_df1 : apply_cons_list_parse{'list;."apply"[start:n,finish:n]{."apply"[start1:n, finish1:n]{.uid[start2:n, finish2:n]{uid["::"]}; 'e1}; 'e2}} =
   apply_cons_list_parse{ocons{df_term{szone{'e1}}; 'list}; 'e2}

dform apply_cons_parse_df2 : apply_cons_list_parse{'e1; uid[start1:n, finish1:n]{uid["[]"]}} =
   `"[" pushm[0] df_rev_concat{xcons{keyword[";"]; hspace}; 'e1} popm `"]"

dform apply_cons_parse_df3 : parens :: apply_cons_list_parse{'e1; 'e2} =
   pushm[0] df_rev_concat{xcons{hspace; xcons{keyword["::"]; hspace}}; xcons{szone{'e2}; 'e1}} popm

(*
 * Label.
 *)

dform lab_df1 : "lab"{'poel} =
   szone "~" patt_format{'poel; onil} ezone

dform lab_df2 : "lab"[start:n, finish:n]{'e1} =
   lab{'e1}

(*
 * Subscripting.
 *)
dform array_subscript_df1 : parens :: "prec"[prec_proj] :: "array_subscript"{'e1; 'e2} =
   slot{'e1} `"array_subscript(" pushm[0] slot{'e2} popm ")"

dform array_subscript_df2 : "array_subscript"[start:n, finish:n]{'e1; 'e2} =
   "array_subscript"{'e1; 'e2}

dform string_subscript_df1 : parens :: "prec"[prec_proj] :: "string_subscript"{'e1; 'e2} =
   slot{'e1} `"string_subscript(" pushm[0] slot{'e2} popm ")"

dform string_subscript_df2 : "string_subscript"[start:n, finish:n]{'e1; 'e2} =
   "string_subscript"{'e1; 'e2}

(*
 * Sequencing.
 *)
dform sequence_df1 : "sequence"{'e1} =
   szone pushm[0] list_expr{'e1} popm ezone

dform sequence_df2 : "sequence"[start:n, finish:n]{'e1} =
   sequence{'e1}

(*
 * Lists, arrays, streams, records.
 * This is a recursive display form.
 *)
dform list_df1 : "list"{'e1} =
   "[" pushm[0] list_expr{'e1} popm "]"

dform array_df1 : "array"{'e1} =
   "[|" pushm[0] list_expr{'e1} popm "|]"

dform stream_df1 : "stream"{'e1} =
   "[<" pushm[0] se_list{'e1} popm ">]"

dform record_df1 : "record"{'e1; none} =
   "{" pushm[0] ee_list{'e1} popm "}"

dform record_df2 : "record"{'e1; some{'e2}} =
   "{" pushm[0] szone{'e2} " " keyword["with"] hspace ee_list{'e1} popm "}"

dform tuple_df1 : "tuple"{'e1} =
   "(" pushm[0] e_list{'e1} popm ")"

dform array_df2 : "array"[start:n, finish:n]{'e1} =
   "array"{'e1}

dform stream_df2 : "stream"[start:n, finish:n]{'e1} =
   "stream"{'e1}

dform record_df : "record"[start:n, finish:n]{'e1; 'e2} =
   "record"{'e1; 'e2}

dform tuple_df2 : "tuple"[start:n, finish:n]{'e1} =
   "tuple"{'e1}

(*
 * Lists & arrays.
 *)
dform list_expr_df : list_expr{'e} =
   df_concat{xcons{keyword[";"]; hspace}; 'e}

(*
 * Module name.
 *)
dform ident_expr_cons_df : ident_expr{ocons{.Ocaml!"string"[name:s]; ocons{'e1; 'e2}}} =
   slot[name:s] `"." ident_expr{ocons{'e1; 'e2}}

dform ident_expr_onil_df : ident_expr{ocons{.Ocaml!"string"[name:s]; onil}} =
   slot[name:s]

(*
 * Streams.
 *)
dform se_list_onil_df : se_list{onil} =
   `""

dform se_list_cons_df1 : se_list{ocons{ocons{'s; 'e}; onil}} =
   slot{'s} `"XXX" slot{'e}

dform se_list_cons_df2 : se_list{ocons{ocons{'s; 'e}; ocons{'e2; 'e3}}} =
   slot{'s} `"XXX" slot{'e} ";" hspace se_list{ocons{'e2; 'e3}}

(*
 * Tuples.
 *)
dform e_list_df : e_list{'e} =
   df_concat{xcons{keyword[","]; hspace}; 'e}

(*
 * Records.
 *)
dform ee_list_onil_df : ee_list{onil} =
   `""

dform ee_list2_onil_df : ee_list'{onil} =
   `""

dform ee_list_onil_df2 : ee_list{ocons{ee{'e1; 'e2}; 'e3}} =
   szone slot{'e1} hspace "=" hspace slot{'e2} ezone
   ee_list'{'e3}

dform ee_list2_onil_df2 : ee_list'{ocons{ee{'e1; 'e2}; 'e3}} =
   ";" hspace szone slot{'e1} `" " "=" hspace slot{'e2} ezone
   ee_list'{'e3}

(*
 * Assignment.
 *)
dform assign_df1 : parens :: "prec"[prec_assign] :: assign{'e1; 'e2} =
   szone push_indent slot{'e1} hspace `"= " slot{'e2} popm ezone

dform assign_df2 : assign[start:n, finish:n]{'e1; 'e2} =
   assign{'e1; 'e2}

(*
 * Conditional.
 *)
dform ifthenelse_df : parens :: "prec"[prec_if] :: ifthenelse{'e1; 'e2; 'e3} =
   pushm[0] szone push_indent "_if" `" " szone{'e1} `" " "_then" hspace
   szone{'e2} popm hspace
   push_indent "_else" hspace szone{'e3} popm ezone popm

dform ifthenelse_df2 : ifthenelse[start:n, finish:n]{'e1; 'e2; 'e3} =
   ifthenelse{'e1; 'e2; 'e3}

(*
 * Loops.
 *)
dform for_upto_df1 : for_upto{'e1; 'e2; x. 'e3} =
   pushm[0] push_indent
   "_for" hspace slot{'x} hspace `"= " slot{'e2} hspace "_to" slot{'e3} hspace "_do" hspace
      slot{'e3} popm hspace
      "_done" popm

dform for_upto_df2 : for_upto[start:n, finish:n]{'e1; 'e2; x. 'e3} =
   for_upto{'e1; 'e2; x. 'e3}

dform for_downto_df1 : for_downto{'e1; 'e2; x. 'e3} =
   pushm[0] push_indent
   "_for" hspace slot{'x} hspace `"= " slot{'e2} hspace "_downto" slot{'e3} hspace "_do" hspace
      slot{'e3} popm hspace
      "_done" popm

dform for_downto_df2 : for_downto[start:n, finish:n]{'e1; 'e2; x. 'e3} =
   for_downto{'e1; 'e2; x. 'e3}

dform while_df1 : "while"{'e1; 'e2} =
   szone pushm[0] push_indent "_while" hspace slot{'e1} hspace "_do" hspace
   slot{'e2} popm hspace
   "_done" popm ezone

dform while_df2 : "while"[start:n, finish:n]{'e1; 'e2} =
   "while"{'e1; 'e2}

(*
 * Type casting.
 *)
dform cast_df1 : cast{'e; 't} =
   "(" slot{'e} hspace ":" hspace slot{'t} ")"

dform cast_df2 : cast[start:n, finish:n]{'e; 't} =
   cast{'e; 't}

(*
 * Class coercion.
 *)
dform class_coerce_df1 : parens :: "prec"[prec_rel] :: class_coerce{'e1; 'e2} =
   push_indent slot{'e1} hspace `"Ocaml!class_coerce" slot{'e2} popm

dform class_coerce_df2 : class_coerce[start:n, finish:n]{'e1; 'e2} =
   class_coerce{'e1; 'e2}

(*
 * New object.
 *)
declare "new"{'e1 : TyOCaml}

dform new_df1 : parens :: "prec"[prec_not] :: "new"{'e1} =
   "_new" slot{'e1}

(*
 * "Match" forms.
 *)
dform fun_df1 : parens :: "prec"[prec_fun] :: "fun"{'pwel} =
   szone "_fun" `" " patt_format{'pwel; onil} ezone

dform fun_df2 : "fun"[start:n, finish:n]{'pwel} =
   "fun"{'pwel}

dform match_df1 : parens :: "prec"[prec_fun] :: "match"{'pwel; 'e} =
   szone push_indent "_match" hspace 'e hspace "_with" hspace
   patt_format{'pwel; onil}
   popm ezone

dform match_df2 : "match"[start:n, finish:n]{'e; 'pwel} =
   "match"{'e; 'pwel}

dform try_df1 : parens :: "prec"[prec_fun] :: "try"{'pwel; 'e} =
   szone push_indent "_try" hspace slot{'e} hspace "_with" hspace
   patt_format{'pwel; onil}
   popm ezone

dform try_df2 : "try"[start:n, finish:n]{'e; 'pwel} =
   "try"{'e; 'pwel}

(*
 * "Let" forms.  The real work is performed in the patterns.
 *)
dform let_df1 : parens :: "prec"[prec_let] :: "let"{'p; 'e} =
   szone pushm[0] keyword["_let"] `" " patt_format{'p; 'e} popm ezone

dform let_df2 : "let"[start:n, finish:n]{'p; 'e} =
   "let"{'p; 'e}

dform fix_df1 : parens :: "prec"[prec_let] :: "fix"{'p} =
   szone pushm[0] keyword["_letrec"] hspace patt_format{'p; onil} popm ezone

dform fix_df2 : "fix"[start:n, finish:n]{'p} =
   "fix"{'p}

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
