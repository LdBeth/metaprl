(*
 * Display form handler.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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
 *
 *)

open Precedence
open Rformat
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite

(************************************************************************
 * DFORM INTERFACE                                                      *
 ************************************************************************)

(*
 * The display database is functional, and display formas
 * are added for term templates.
 *)
type dform_base

(*
 * A display form printer knows about this term, and
 * a printer for subterms.  The subterm printer takes
 * an extra argument that specifies parenthesization.
 *)
type parens =
   NOParens
 | LTParens
 | LEParens

type dform_printer_info =
   { dform_term : term;
     dform_stack : rewrite_stack;
     dform_items : rewrite_item list;
     dform_printer : buffer -> parens -> term -> unit;
     dform_buffer : buffer
   }

type dform_printer =
   DFormExpansion of term
 | DFormPrinter of (dform_printer_info -> unit)

(*
 * Options on a dform.
 *)
type dform_option =
   (* Parens and precedences *)
   DFormInheritPrec
 | DFormPrec of precedence
 | DFormParens

(*
 * This is the info needed for each display form.
 *)
type dform_info =
   { dform_name : string;
     dform_pattern : term;
     dform_options : dform_option list;
     dform_print : dform_printer
   }

(*
 * Destruct a base.
 *)
type dform_entry =
   DFormEntry of dform_info
 | DFormBase of dform_base

(*
 * Display form installation.
 *)
val null_base : dform_base
val add_dform : dform_base -> dform_info -> dform_base

(*
 * Join two bases.
 *)
val join_dforms : dform_base -> dform_base -> dform_base

val is_null_dfbase : dform_base -> bool
val equal_dfbases : dform_base -> dform_base -> bool
val dest_dfbase : dform_base -> dform_entry * dform_base

(************************************************************************
 * PRINTERS                                                             *
 ************************************************************************)

val format_quoted_term : dform_base -> buffer -> term -> unit
val format_term : dform_base -> buffer -> term -> unit
val print_term_fp : dform_base -> out_channel -> term -> unit
val print_term : dform_base -> term -> unit
val prerr_term : dform_base -> term -> unit
val string_of_term : dform_base -> term -> string

val format_short_term : dform_base -> (string -> opname) -> buffer -> term -> unit
val print_short_term_fp : dform_base -> (string -> opname) -> out_channel -> term -> unit

val format_bterm : dform_base -> buffer -> bound_term -> unit
val print_bterm_fp : dform_base -> out_channel -> bound_term -> unit
val print_bterm : dform_base -> bound_term -> unit
val prerr_bterm : dform_base -> bound_term -> unit
val string_of_bterm : dform_base -> bound_term -> string

val format_mterm : dform_base -> buffer -> meta_term -> unit
val print_mterm_fp : dform_base -> out_channel -> meta_term -> unit
val print_mterm : dform_base -> meta_term -> unit
val prerr_mterm : dform_base -> meta_term -> unit
val string_of_mterm : dform_base -> meta_term -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
