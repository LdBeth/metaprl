(*
 * This module defines functions used to read and write terms in a robust ASCII-based format
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
 * Author: Alexey Nogin
 * nogin@cs.cornell.edu
 *
 *)

(******************************************************************

The format for the ASCII files is the following - each line contains
either a term or a bound term or an operator or an opname or a parameter
or a hypothesis or a context hypothesis. Seuqents are printed on two lines.
The lines should be in the following format:

Term:
T<long_name> <short_name> <operator_short_name> [<bterm_short_name_1> ... <bterm_short_name_n>]

Bound term:
B<long_name> <short_name> <term_or_sequent_short_name> [<var_1> ... <var_n>]

Hypothesis:
H<long_name> <short_name> <variable> <term_short_name>

Context hyp:
C<long_name> <short_name> <variable> [<term_short_name_1> ... <term_short_name_n>]

Sequent:
S<long_name> <short_name> <arg_term_short_name> [<hyp_1_short_name> ... <hyp_n_short_name>]
G<long_name> <short_name> [<goal_1_short_name> ... <goal_n_short_name>]

Operator:
O<long_name> <short_name> <opname_short_name> [<param_short_name_1> ... <param_short_name_n>]

Opname:
N<long_name> <short_name> <main_part_of_opname> <short_name_for_the_rest_of_opname>|NIL

Param - either of:
P<long_name> <short_name> Number <number>
P<long_name> <short_name> String <string>
P<long_name> <short_name> Token <string>
P<long_name> <short_name> Var <string>
P<long_name> <short_name> MNumber <string>
P<long_name> <short_name> MString <string>
P<long_name> <short_name> MToken <string>
P<long_name> <short_name> MVar <string>
P<long_name> <short_name> MLevel <int> [<var_1_string> <off_1_int> ... <var_1_string> <off_1_int>]

Where "long name" is there only there to make the file more readable for the user,
"short name" is probably several characters long, typically - a base short name
and a serial number. If a string has non-printable characters or spaces, it would
be printed quoted. For sequents the correspondent S and G lines have to have the same short
name.

******************************************************************)

open Opname

module type AsciiIOSig =
sig

   type param
   type term
   type bound_term
   type hypothesis
   type esequent

   type io_item = string * string * string list
   type io_table

   (*
    * These are the functions that are useful.
    *)
   val read_table : in_channel -> io_table
   val write_term : out_channel -> io_table -> term -> unit

   (*
    * INPUT
    *)

   val initialize : unit -> io_table
   (* if the input string was quoted, it should be unquoted before feeding it to add_line *)
   (* you may find Lm_string_util.unquote function useful *)
   val add_line : io_table -> io_item -> unit
   val get_term : io_table -> term
   
   (* Just in case we want to read a particulare entry from the .prla file *)
   val get_named_term : io_table -> string -> term
   val read_from_file : string -> string -> term

   (*
    * OUTPUT
    *)

   (*
    * out_control specifies how to generate names and what to do with produced entries.
    * out_name_* function should produce a long name and a base short name
    * out_line function should take care of quoting strings (Lm_string_util.quote)
    *
    * input functions should work correctly no matter what out_name_* were used for output
    *)

   type out_control =
    { out_name_op : opname -> param list -> string * string;
      out_name_param : param -> string * string;
      out_name_term : term -> string * string;
      out_name_bterm : bound_term -> string * string;
      out_name_hyp : hypothesis -> string * string;
       (* arg+hyps long name, goals long name, short name *)
      out_name_seq : esequent -> string * string * string;
      out_line : io_item -> unit
    }

   val output_term : io_table -> out_control -> term -> unit

   (* simple_* functions provide a simple version of io_control *)
   val simple_name_op : opname -> param list -> string * string
   val simple_name_param : param -> string * string
   val simple_name_term : term -> string * string
   val simple_name_bterm : bound_term -> string * string
   val simple_name_hyp : hypothesis -> string * string
   val simple_name_seq : esequent -> string * string * string
   val simple_output_line : out_channel -> io_item -> unit

   (* io_control uning simple_* functions *)
   val make_simple_control : out_channel -> out_control

end
