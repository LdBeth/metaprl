(*
 * This file contains the primitive syntax and display
 * for ocaml terms.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 *)
extends Nuprl_font
extends Base_dform

open Lm_debug
open Printf

open Refiner.Refiner

let _ =
   show_loading "Loading Ocaml_base_df%t"

(************************************************************************
 * DISPLAY TERMS                                                        *
 ************************************************************************)

(*
 * Operators.
 *)
declare "["
declare "]"
declare "[|"
declare "|]"
declare "[<"
declare ">]"
declare "{"
declare "}"
declare "("
declare ")"

declare "+"
declare "-"
declare "*"
declare "/"
declare "mod"

declare "&"
declare "or"
declare "="
declare "=="
declare "::"
declare ":="
declare "."
declare ".("
declare ".["
declare ":>"
declare ";"
declare "->"
declare "|"
declare "<>"
declare ":"
declare "_"
declare "#"
declare "'"
declare "\""

declare "_if"
declare "_then"
declare "_else"

declare "_for"
declare "_while"
declare "_to"
declare "_downto"
declare "_do"
declare "_done"

declare "_new"
declare "_fun"
declare "_match"
declare "_try"
declare "_type"
declare "_exception"
declare "_let"
declare "_letrec"
declare "_in"
declare "_and"
declare "_with"
declare "_val"
declare "_as"
declare "_external"
declare "_of"

declare "_module"
declare "_moduletype"
declare "_open"
declare "_sig"
declare "_struct"
declare "_functor"
declare "_end"

declare push_indent

(*
 * Display control tags.
 *)
declare patt_format{'a;'b}

(************************************************************************
 * DISPLAYS                                                             *
 ************************************************************************)

(*
 * Operators.
 *)
dform left_brack_df	: internal :: "["		= keyword["["]
dform right_brack_df	: internal :: "]"		= keyword["]"]
dform left_array_df	: internal :: "[|"		= keyword["[|"]
dform right_array_df	: internal :: "|]"		= keyword["|]"]
dform left_stream_df	: internal :: "[<"		= keyword["[<"]
dform right_stream_df	: internal :: ">]"		= keyword[">]"]
dform left_curly_df	: internal :: "{"		= keyword["{"]
dform right_curly_df	: internal :: "}"		= keyword["}"]
dform left_paren_df	: internal :: "("		= keyword["("]
dform right_paren_df	: internal :: ")"		= keyword[")"]

dform plus_df		: internal :: "+"		= keyword["+"]
dform minus_df		: internal :: "-"		= keyword["-"]
dform star_df		: internal :: "*"		= keyword["*"]
dform slash_df		: internal :: "/"		= keyword["/"]
dform mod_df_df    	: internal :: "mod"		= keyword["mod"]

dform and_df		: internal :: "&"		= keyword["&"]
dform or_df		: internal :: "or"		= keyword["or"]
dform eq_df		: internal :: "="		= keyword["="]
dform eqeq_df		: internal :: "=="		= keyword["=="]
dform colon_colon_df	: internal :: "::"		= keyword["::"]
dform colon_eq_df	: internal :: ":="		= keyword[":="]
dform dot_df		: internal :: "."		= keyword["."]
dform array_sub_df	: internal :: ".("		= keyword[".("]
dform string_sub_df	: internal :: ".["		= keyword[".["]
dform coerce_df         : internal :: ":>"		= keyword[":>"]
dform semicolon_df	: internal :: ";"		= keyword[";"]
dform arrow_df		: internal :: "->"		= keyword["->"]
dform pipe_df		: internal :: "|"		= keyword["|"]
dform neq_df    	: internal :: "<>"		= keyword["<>"]
dform colon_df          : internal :: ":"		= keyword[":"]
dform underscore_df	: internal :: "_"		= keyword["_"]
dform hash_df		: internal :: "#"		= keyword["#"]
dform quote_df		: internal :: "'"		= keyword["'"]
dform backslash_df	: internal :: "\""		= keyword["\""]

dform if_df		: internal :: "_if"		= keyword["if"]
dform then_df		: internal :: "_then"           = keyword["then"]
dform else_df		: internal :: "_else"           = keyword["else"]

dform for_df		: internal :: "_for"            = keyword["for"]
dform while_df		: internal :: "_while"          = keyword["while"]
dform to_df		: internal :: "_to"		= keyword["to"]
dform downto_df		: internal :: "_downto"         = keyword["downto"]
dform do_df		: internal :: "_do"		= keyword["do"]
dform done_df		: internal :: "_done"           = keyword["done"]

dform new_df		: internal :: "_new"            = keyword["new"]
dform fun_df		: internal :: "_fun"            = keyword["fun"]
dform match_df		: internal :: "_match"          = keyword["match"]
dform try_df		: internal :: "_try"            = keyword["try"]
dform type_df		: internal :: "_type"           = keyword["type"]
dform exception_df	: internal :: "_exception"	= keyword["exception"]
dform let_df		: internal :: "_let"            = keyword["let"]
dform letrec_df		: internal :: "_letrec"         = keyword["let rec"]
dform in_df		: internal :: "_in"		= keyword["in"]
dform and_df		: internal :: "_and"            = keyword["and"]
dform with_df		: internal :: "_with"           = keyword["with"]
dform val_df		: internal :: "_val"            = keyword["val"]
dform as_df		: internal :: "_as"		= keyword["as"]
dform external_df	: internal :: "_external"	= keyword["of"]
dform of_df		: internal :: "_of"		= keyword["external"]

dform module_df		: internal :: "_module"         = keyword["module"]
dform moduletype_df	: internal :: "_moduletype"	= keyword["module type"]
dform open_df		: internal :: "_open"           = keyword["open"]
dform sig_df		: internal :: "_sig"            = keyword["sig"]
dform struct_df		: internal :: "_struct"         = keyword["struct"]
dform functor_df	: internal :: "_functor"	= keyword["functor"]
dform end_df		: internal :: "_end"            = keyword["end"]

dform push_ident_df     : internal :: push_indent       = pushm[3]

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
