(*
 * Display forms for module expressions.
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
extends Ocaml_mt_df

open Lm_debug
open Printf

let _ =
   show_loading "Loading Ocaml_me_df%t"

(*
 * Projection.
 *)
dform me_proj_df : me_proj{'me1; 'me2} = proj{'me1; 'me2}

(*
 * Application.
 *)
dform me_apply_df : me_apply{'me1; 'me2} = apply{'me1; 'me2}

(*
 * Functor.
 *)
dform me_functor_df : me_functor[name:s]{'mt; 'me} = mt_functor[name:s]{'mt; 'me}

(*
 * Structure.
 *)
dform me_struct_df : me_struct{'sil} =
   szone pushm[0] push_indent "_struct" space
   list_expr{'sil}
   popm space "_end" popm ezone

(*
 * Type cast.
 *)
dform me_cast_df : me_cast{'me; 'mt} =
   "(" szone pushm[0] slot{'me} space ":" space slot{'mt} popm ezone ")"

(*
 * Variables.
 *)
dform me_lid_df : me_lid[name:s] = slot[name:s]
dform me_uid_df : me_uid[name:s] = slot[name:s]

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
