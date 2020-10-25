(*
 * This module defines a signature for functions used to read and write
 * terms in a robust ASCII-based format
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
 * Copyright (C) 1999-2005 MetaPRL Group, Cornell University and
 * California Institute of Technology
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
 * Author: Aleksey Nogin
 * nogin@cs.cornell.edu
 *)

(******************************************************************

A file in the ASCII format is a sequence of lines, each describing and naming a
single value - a term, or a bound term, or an operator, or an opname, or a
parameter, or a hypothesis, or a hypothesis context. Each line provides a name
for the new value and described the value by providing all its component
(roughly following the structure of the "mk_*" API for constructing term
values). Variables, strings and ints are represented by strings, other values
are referred to by their names (which must be defined first).

For each value being named lines should be in the following format:

Term (not a sequent or SO variable):
T<comment> <name> <operator_name> [<bterm_name_1> ... <bterm_name_n>]

Bound term:
B<comment> <name> <term_name> [<var_1> ... <var_n>]

Hypothesis:
H<comment> <name> <variable> <term_name>

Context hyp:
C<comment> <name> <variable>\\ [<var_1> ... <var_n>]\\ [<term_name_1> ... <term_name_n>]

Sequent:
S<comment> <name> <arg_term_name>\\ [<hyp_1_name> ... <hyp_n_name>]\\ <concl_name>

SO Variable:
V<comment> <name> <variable>\\ [<var_1> ... <var_n>]\\ [<term_name_1> ... <term_name_n>]

Operator:
O<comment> <name> <opname_name> [<param_name_1> ... <param_name_n>]

Opname:
N<comment> <name> <opname_head_string> <opname_tail_name>|NIL

Param - either of:
P<comment> <name> Number <number>
P<comment> <name> String <string>
P<comment> <name> Token <opname_name>
P<comment> <name> Var <string>
P<comment> <name> Shape <term_name>
P<command> <name> Operator <term_name>
P<comment> <name> MNumber <string>
P<comment> <name> MString <string>
P<comment> <name> MToken <string>
P<comment> <name> MShape <string>
P<comment> <name> MOperator <string>
P<comment> <name> MLevel <int> [<var_1_string> <off_1_int> ... <var_1_string> <off_1_int>]
P<comment> <name> Quote

Each name may be defined at most once. The "name" is typically several
characters long, usually - a base short name and a serial number. If a string
has non-printable characters or spaces, it should be quoted and non-printable
characters escaped.

******************************************************************)

open Opname

module type AsciiIOSig =
sig

   type param
   type term
   type bound_term
   type hypothesis
   type esequent

   type io_item = string * string * string list list
   type io_table

   (*
    * These are the functions that are useful.
    *)
   val read_table : in_channel -> io_table
   val write_term : Stdlib.out_channel -> io_table -> term -> unit

   (*
    * INPUT
    *)

   val initialize : unit -> io_table
   (* if the input string was quoted, it should be unquoted before feeding it to add_line *)
   (* you may find Lm_string_util.unquote function useful *)
   val add_line : io_table -> io_item -> unit
   val get_term : io_table -> term

   (* Just in case we want to read a particular entry from the .prla file *)
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
      out_name_seq : esequent -> string * string;
      out_line : io_item -> unit
    }

   val output_term : io_table -> out_control -> term -> unit

   (* simple_* functions provide a simple version of io_control *)
   val simple_name_op : opname -> param list -> string * string
   val simple_name_param : param -> string * string
   val simple_name_term : term -> string * string
   val simple_name_bterm : bound_term -> string * string
   val simple_name_hyp : hypothesis -> string * string
   val simple_name_seq : esequent -> string * string
   val simple_output_line : Stdlib.out_channel -> io_item -> unit

   (* io_control uning simple_* functions *)
   val make_simple_control : Stdlib.out_channel -> out_control

end
