(*
 * Null object raises exception on every operation.
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

include Shell_type

open Printf
open Mp_debug

open Rformat
open Refiner.Refiner.RefineError

open Shell_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Shell_null%t" eflush

let null_object =
   let edit_format _ buf =
      format_string buf "There is no object to edit\n"
   in
   let one_error _ =
      raise (RefineError ("Shell_null.null_object", StringError "no current object"))
   in
   let three_errors _ _ _ =
      raise (RefineError ("Shell_null.null_object", StringError "no current object"))
   in
      { edit_format = edit_format;
        edit_set_goal = one_error;
        edit_set_redex = one_error;
        edit_set_contractum = one_error;
        edit_set_assumptions = one_error;
        edit_set_params = one_error;
        edit_save = one_error;
        edit_check = one_error;
        edit_expand = one_error;
        edit_root = one_error;
        edit_up = one_error;
        edit_down = one_error;
        edit_addr = one_error;
        edit_goal = one_error;
        edit_tactic = one_error;
        edit_children = one_error;
        edit_extras = one_error;
        edit_refine = three_errors;
        edit_undo = one_error;
        edit_fold = one_error;
        edit_fold_all = one_error
      }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
