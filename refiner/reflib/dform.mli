(*
 * Display form handler.
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
 *
 *)
open Lm_string_set

open Opname
open Precedence
open Refiner.Refiner.TermType
open Refiner.Refiner.Rewrite

(************************************************************************
 * DFORM INTERFACE                                                      *
 ************************************************************************)

(*
 * Print to term tagged buffers.
 *)
type buffer = Lm_rformat.buffer

(*
 * A display form printer knows about this term, and
 * a printer for subterms.  The subterm printer takes
 * an extra argument that specifies parenthesization.
 *)
type parens =
   NOParens
 | LTParens
 | LEParens

type dform_base

type dform_printer_info =
   { dform_term    : term;
     dform_items   : rewrite_item list;
     dform_printer : buffer -> parens -> term -> unit;
     dform_buffer  : buffer;
     dform_state   : dform_base
   }

type dform_printer =
   DFormExpansion of term
 | DFormPrinter of (dform_printer_info -> unit)

(*
 * Options on a dform.
 *    1. InheritPrec means that the display form has
 *       the same precedence as the term it encloses.
 *    2. DFormPrec gives an exact precedence for the term
 *    3. DFormParens means the display can be enclosed in parenthesis
 *    4. DFormInternal means the display form is an
 *       intermediate form used to compute a complex display.
 *)
type dform_option =
   DFormInheritPrec
 | DFormPrec of precedence
 | DFormParens

type dform_mode = string

type dform_modes =
   Modes of dform_mode list       (* include these modes *)
 | ExceptModes of dform_mode list (* exclude these modes *)
 | AllModes
 | PrimitiveModes

(*
 * This is the info needed for each display form.
 *)
type dform_info =
   { dform_modes   : dform_modes;
     dform_pattern : term;
     dform_options : dform_option list;
     dform_print   : dform_printer;
     dform_name    : string
   }

(*
 * Shortener decides whether an opname needs to be printed in a fully-qualified
 * or a shorter form.
 *)
type shortener = opname -> op_kind -> param list -> bound_term list -> string

(*
 * Display form installation.
 *)
val null_base : dform_base
val null_shortener : shortener

(*
 * The main dform database uses the resources mechanism. The database is functional,
 * and display formas are added for term templates.
 *
 * get_mode_base may raise Failure if bookmark does not exist.
 *)
val add_dform : dform_info -> unit
val get_mode_base : Mp_resource.bookmark -> dform_mode -> shortener -> dform_base
val change_mode : dform_base -> dform_mode -> dform_base

(*
 * Save terms in slot position?
 * If requested, each term in a slot is given a unique string tag,
 * and printed in a tzone with that tag.  The table provides a map
 * from tag to term.
 *)
val save_slot_terms : dform_base -> dform_base
val get_slot_terms  : dform_base -> term StringTable.t

(************************************************************************
 * PRINTERS                                                             *
 ************************************************************************)

val format_quoted_term : dform_base -> buffer -> term -> unit
val format_term : dform_base -> buffer -> term -> unit
val print_term_fp : dform_base -> Lm_printf.out_channel -> term -> unit
val print_term : dform_base -> term -> unit
val prerr_term : dform_base -> term -> unit
val string_of_term : dform_base -> term -> string

val format_bterm : dform_base -> buffer -> bound_term -> unit
val print_bterm_fp : dform_base -> Lm_printf.out_channel -> bound_term -> unit
val print_bterm : dform_base -> bound_term -> unit
val prerr_bterm : dform_base -> bound_term -> unit
val string_of_bterm : dform_base -> bound_term -> string

val format_mterm : dform_base -> buffer -> meta_term -> unit
val print_mterm_fp : dform_base -> Lm_printf.out_channel -> meta_term -> unit
val print_mterm : dform_base -> meta_term -> unit
val prerr_mterm : dform_base -> meta_term -> unit
val string_of_mterm : dform_base -> meta_term -> string

val string_of_param : string Rewrite_sig.rewrite_param -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
