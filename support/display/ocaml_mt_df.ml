(*
 * Display forms for module types.
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
open Printf

open Ocaml_expr_df

let _ =
   show_loading "Loading Ocaml_mt_df%t"

(*
 * Projection.
 *)
dform mt_proj_df1 : parens :: "prec"[prec_proj] :: mt_proj{'mt1; 'mt2} =
   slot{'mt1} "." slot{'mt2}

dform mt_proj_df2 : internal :: mt_proj[start:n, finish:n]{'mt1; 'mt2} =
   mt_proj{'mt1; 'mt2}

(*
 * Application.
 *)
dform mt_apply_df1 : parens :: "prec"[prec_apply] :: mt_apply{'mt1; 'mt2} =
   slot{'mt1} space slot{'mt2}

dform mt_apply_df2 : internal :: mt_apply[start:n, finish:n]{'mt1; 'mt2} =
   mt_apply{'mt1; 'mt2}

(*
 * Functor.
 *)
dform mt_functor_df1 : parens :: "prec"[prec_fun] :: mt_functor[name:s]{'mt1; 'mt2} =
   "_functor" space "(" slot[name:s] space ":" space slot{'mt1} ")"
   space "->" slot{'mt2}

dform mt_functor_df2 : internal :: mt_functor[start:n, finish:n, name:s]{'mt1; 'mt2} =
mt_functor[name:s]{'mt1; 'mt2}

(*
 * Id.
 *)
dform mt_lid_df1 : mt_lid[name:s] =
   slot[name:s]

dform mt_lid_df2 : mt_lid{'v} =
   slot{'v}

dform mt_lid_df3 : internal :: mt_lid[start:n, finish:n]{'v} =
   mt_lid{'v}

dform mt_uid_df1 : mt_uid[name:s] =
   slot[name:s]

dform mt_uid_df2 : mt_uid{'v} =
   slot{'v}

dform mt_uid_df3 : internal :: mt_uid[start:n, finish:n]{'v} =
   mt_uid{'v}

(*
 * Signature.
 *)
dform mt_sig_df1 : mt_sig{'sil} =
   szone "_sig" hspace
   list_expr{'sil}
   hspace "_end" ezone

dform mt_sig_df2 : internal :: mt_sig[start:n, finish:n]{'sil} =
   mt_sig{'sil}

(*
 * Module type with clause.
 *)
declare mt_with{'lst}

dform mt_with_df1 : mt_with{'mt; 'wcl} =
   szone pushm[0] slot{'mt} mt_with{'wcl} popm ezone

dform mt_with_df2 : internal :: mt_with[start:n, finish:n]{'mt; 'wcl} =
   mt_with{'mt; 'wcl}

dform mt_with_nil_df : internal :: mt_with{nil} = `""

dform mt_with_cons_df : internal :: mt_with{cons{'wc; 'wcl}} =
   slot{'wc} mt_with{'wcl}

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
