(*
 * Pretty printer for terms.
 *
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

open Rformat
open Opname

module type SimplePrintSig =
sig
   type term
   type level_exp
   type param
   type bound_term
   type meta_term
   type address

   type buffer = term Rformat.buffer

   (************************************************************************
    * PRINTERS                                                             *
    ************************************************************************)

   val format_simple_level_exp : buffer -> level_exp -> unit
   val print_simple_level_exp_fp : out_channel -> level_exp -> unit
   val print_simple_level_exp : level_exp -> unit
   val prerr_simple_level_exp : level_exp -> unit
   val string_of_level_exp : level_exp -> string

   val string_of_opname : opname -> string

   val format_simple_param : buffer -> param -> unit
   val print_simple_param_fp : out_channel -> param -> unit
   val print_simple_param : param -> unit
   val prerr_simple_param : param -> unit
   val string_of_param : param -> string

   val format_simple_term : buffer -> term -> unit
   val print_simple_term_fp : out_channel -> term -> unit
   val print_simple_term : term -> unit
   val prerr_simple_term : term -> unit
   val string_of_term : term -> string

   val format_simple_bterm : buffer -> bound_term -> unit
   val print_simple_bterm_fp : out_channel -> bound_term -> unit
   val print_simple_bterm : bound_term -> unit
   val prerr_simple_bterm : bound_term -> unit
   val string_of_bterm : bound_term -> string

   val format_simple_mterm : buffer -> meta_term -> unit
   val print_simple_mterm_fp : out_channel -> meta_term -> unit
   val print_simple_mterm : meta_term -> unit
   val prerr_simple_mterm : meta_term -> unit
   val string_of_mterm : meta_term -> string

   val print_simple_address_fp : out_channel -> address -> unit
   val print_simple_address : address -> unit
   val prerr_simple_address : address -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

