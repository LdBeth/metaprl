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
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
extends Mpsymbols
extends Base_dform
extends Ocaml

(*
 * Operators.
 *)
dform left_brack_df	: "["		= keyword["["]
dform right_brack_df	: "]"		= keyword["]"]
dform left_array_df	: "[|"		= keyword["[|"]
dform right_array_df	: "|]"		= keyword["|]"]
dform left_stream_df	: "[<"		= keyword["[<"]
dform right_stream_df	: ">]"		= keyword[">]"]
dform left_curly_df	: "{"		= keyword["{"]
dform right_curly_df	: "}"		= keyword["}"]
dform left_paren_df	: "("		= keyword["("]
dform right_paren_df	: ")"		= keyword[")"]

dform plus_df		: "+"		= keyword["+"]
dform minus_df		: "-"		= keyword["-"]
dform star_df		: "*"		= keyword["*"]
dform slash_df		: "/"		= keyword["/"]
dform mod_df_df    	: "mod"		= keyword["mod"]

dform and_df		: "&"		= keyword["&"]
dform or_df		: "or"		= keyword["or"]
dform eq_df		: "="		= keyword["="]
dform eqeq_df		: "=="		= keyword["=="]
dform colon_colon_df	: "::"		= keyword["::"]
dform colon_eq_df	: ":="		= keyword[":="]
dform dot_df		: "."		= keyword["."]
dform array_sub_df	: ".("		= keyword[".("]
dform string_sub_df	: ".["		= keyword[".["]
dform coerce_df         : ":>"		= keyword[":>"]
dform semicolon_df	: ";"		= keyword[";"]
dform arrow_df		: "->"		= keyword["->"]
dform pipe_df		: "|"		= keyword["|"]
dform neq_df    	: "<>"		= keyword["<>"]
dform tilde_df    : "~"   = keyword["~"]
dform colon_df          : ":"		= keyword[":"]
dform underscore_df	: "_"		= keyword["_"]
dform hash_df		: "#"		= keyword["#"]
dform quote_df		: "'"		= keyword["'"]
dform backslash_df	: "\""		= keyword["\""]

dform if_df		: "_if"		= keyword["if"]
dform then_df		: "_then"       = keyword["then"]
dform else_df		: "_else"       = keyword["else"]

dform for_df		: "_for"        = keyword["for"]
dform while_df		: "_while"      = keyword["while"]
dform to_df		: "_to"		= keyword["to"]
dform downto_df		: "_downto"     = keyword["downto"]
dform do_df		: "_do"		= keyword["do"]
dform done_df		: "_done"       = keyword["done"]

dform new_df		: "_new"        = keyword["new"]
dform fun_df		: "_fun"        = keyword["fun"]
dform match_df		: "_match"      = keyword["match"]
dform try_df		: "_try"        = keyword["try"]
dform type_df		: "_type"       = keyword["type"]
dform exception_df	: "_exception"	= keyword["exception"]
dform let_df		: "_let"        = keyword["let"]
dform letrec_df		: "_letrec"     = keyword["let rec"]
dform in_df		: "_in"		= keyword["in"]
dform and_df		: "_and"        = keyword["and"]
dform with_df		: "_with"       = keyword["with"]
dform val_df		: "_val"        = keyword["val"]
dform as_df		: "_as"		= keyword["as"]
dform external_df	: "_external"	= keyword["of"]
dform of_df		: "_of"		= keyword["external"]

dform module_df		: "_module"     = keyword["module"]
dform moduletype_df	: "_moduletype"	= keyword["module type"]
dform open_df		: "_open"       = keyword["open"]
dform sig_df		: "_sig"        = keyword["sig"]
dform struct_df		: "_struct"     = keyword["struct"]
dform functor_df	: "_functor"	= keyword["functor"]
dform end_df		: "_end"        = keyword["end"]

dform push_ident_df     : push_indent   = pushm[3]

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
