(*
 * Phobos exceptions.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)
open Lm_pervasives
open Lm_rformat

open Phobos_type
open Refiner.Refiner.TermType

type conv_exn =
   ParamError of param' * string
 | ParamError2 of param' * param' * string
 | TermError of bound_term * string * string

type conv_loc

val loc_of_string : conv_loc -> bound_term -> string -> conv_loc
val loc_start : bound_term -> string -> conv_loc

exception PhobosException of pos * string
exception PhobosError of string
exception LexerException of string
exception LexerPosException of pos * string
exception RewriteException of pos * string
exception ConvertException of conv_loc * conv_exn

exception SyntaxError of pos
exception SourceAccepted

val format_exn : buffer -> exn -> unit
val format_exn_chan : out_channel -> exn -> unit

val catch : ('a -> 'b) -> 'a -> 'b
