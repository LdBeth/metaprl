(*
 * Display forms for types.
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

extends Ocaml
extends Ocaml_base_df
extends Ocaml_expr_df

open Lm_debug

open Ocaml_expr_df

let _ =
   show_loading "Loading Ocaml_type_df%t"

(*
 * Precedences.
 *)
prec prec_as
prec prec_apply
prec prec_arrow
prec prec_star

(*
 * Projection.
 *)
dform type_proj_df1 : parens :: "prec"[prec_proj] :: type_proj{'t1; 't2} =
   slot{'t1} "." slot{'t2}

dform type_proj_df2 : type_proj[start:n, finish:n]{'t1; 't2} =
   type_proj{'t1; 't2}

(*
 * "As" type.
 *)
dform type_as_df1 : parens :: "prec"[prec_as] :: type_as{'t1; 't2} =
   slot{'t1} space "_as" space slot{'t2}

dform type_as_df2 : type_as[start:n, finish:n]{'t1; 't2} =
   type_as{'t1; 't2}

(*
 * Wildcard type.
 *)
dform type_wildcard_df1 : type_wildcard =
   "_"

dform type_wildcard_df2 : type_wildcard[start:n, finish:n] =
   type_wildcard

(*
 * Application.
 *)
declare type_apply_list{'t}
declare type_apply_aux{'t1; 't2}
declare type_apply_aux{'t1}

dform type_apply_df1 : parens :: "prec"[prec_apply] :: type_apply{'t1; 't2} =
   type_apply_aux{'t1; cons{'t2; nil}}

dform type_apply_df2 : type_apply[start:n, finish:n]{'t1; 't2} =
   type_apply{'t1; 't2}

dform type_apply_df3a : type_apply_aux{type_apply{'t1; 't2}; 't3} =
   type_apply_aux{'t1; cons{'t2; 't3}}

dform type_apply_df3b : type_apply_aux{type_apply[start:n, finish:n]{'t1; 't2}; 't3} =
   type_apply_aux{type_apply{'t1; 't2}; 't3}

dform type_apply_df4 : type_apply_aux{'t1; 't2} =
   "(" type_apply_aux{'t2} ")" `" " slot{'t1}

dform type_apply_df5 : type_apply_aux{cons{'t1; 't2}} =
   slot{'t1} type_apply_list{'t2}

dform type_apply_df6 : type_apply_list{nil} =
   `""

dform type_apply_df7 : type_apply_list{cons{'t1; 't2}} =
   `", " slot{'t1} type_apply_list{'t2}

(*
 * Function type.
 *)
dform type_fun_df1 : parens :: "prec"[prec_arrow] :: type_fun{'t1; 't2} =
   slot{'t1} space "->" space slot{'t2}

dform type_fun_df2 : type_fun[start:n, finish:n]{'t1; 't2} =
   type_fun{'t1; 't2}

(*
 * Class identifier.
 *)
dform type_class_id_df1 : parens :: "prec"[prec_not] :: type_class_id{'t1} =
   "#" space slot{'t1}

dform type_class_id_df2 : type_class_id[start:n, finish:n]{'t1} =
   type_class_id{'t1}

(*
 * Identifiers.
 *)
dform type_lid_df1 : type_lid[v:s] =
   slot[v:s]

dform type_lid_df2 : type_lid{'v} =
   slot{'v}

dform type_lid_df3 : type_lid[start:n, finish:n]{'v} =
   type_lid{'v}

dform type_uid_df1 : type_uid[v:s] =
   slot[v:s]

dform type_uid_df2 : type_uid{'v} =
   slot{'v}

dform type_uid_df3 : type_uid[start:n, finish:n]{'v} =
   type_uid{'v}

(*
 * Type parameter.
 *)
dform type_param_df1 : type_param[s:s] =
   `"'" slot[s:s]

dform type_param_df2 : type_param[start:n, finish:n, s:s] =
   type_param[s:s]

(*
 * Type equivalence.
 *)
dform type_equal_df1 : parens :: "prec"[prec_equal] :: type_equal{'t1; 't2} =
   slot{'t1} space "==" space slot{'t2}

dform type_equal_df2 : type_equal[start:n, finish:n]{'t1; 't2} =
   type_equal{'t1; 't2}

(*
 * Record type.
 * I'm not sure what the boolean is for.
 *)
declare type_record_aux{'t}

dform type_record_df1 : type_record{cons{'sbt; 'sbtl}} =
   "{" `" " szone pushm[0] slot{'sbt} type_record_aux{'sbtl} popm ezone `" " "}"

dform type_record_cons_df1 : type_record_aux{cons{'sbt; 'sbtl}} =
   ";" hspace `" " slot{'sbt}
   type_record_aux{'sbtl}

dform type_record_nil_df1 : type_record{nil} =
   `""

dform sbt_df1 : sbt{.Ocaml!"string"[name:s]; .Ocaml!"false"; 't} =
   szone pushm[3] slot[name] `" =" hspace slot{'t} popm ezone

dform sbt_df1 : sbt{.Ocaml!"string"[name:s]; .Ocaml!"true"; 't} =
   szone pushm[3] `"mutable " slot[name] `" =" hspace slot{'t} popm ezone

dform type_record_df2 : type_record[start:n, finish:n]{'t} =
   type_record{'t}

(*
 * Product types.
 *)
declare type_prod_aux{'t}

dform type_prod_df1 : parens :: "prec"[prec_star] :: type_prod{cons{'t; 'tl}} =
   szone pushm[0] slot{'t} type_prod_aux{'tl} popm ezone

dform type_prod_nil_df1 : type_prod_aux{nil} =
   `""

dform type_prod_cons_df1 : type_prod_aux{cons{'t; 'tl}} =
   `" " "*" `" " slot{'t} type_prod_aux{'tl}

dform type_prod_df2 : type_prod[start:n, finish:n]{'tl} =
   type_prod{'tl}

(*
 * Disjoint unions.
 *)
declare stl{'lst}
declare type_list_aux{'stll}

dform type_list_df1 : type_list{cons{'stl; 'stll}} =
   szone{'stl} type_list_aux{'stll}

dform type_list_df2 : type_list[start:n, finish:n]{'stl} =
   type_list{'stl}

dform type_list_nil_df1 : type_list_aux{nil} =
   `""

dform type_list_cons_df1 : type_list_aux{cons{'stl; 'stll}} =
   hspace keyword["|"] `" " szone{'stl} type_list_aux{'stll}

dform stl_df1 : stl{.Ocaml!"string"[name:s]; nil} =
   slot[name:s]

dform stl_df2 : stl{.Ocaml!"string"[name:s]; cons{'t; 'tl}} =
   slot[name:s] `" of " szone slot{'t} stl{'tl} ezone

dform stl_df3 : stl{nil} =
   `""

dform stl_df4 : stl{cons{'t; 'tl}} =
   hspace "*" `" " slot{'t} stl{'tl}

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
