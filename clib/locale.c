/*
 * This file contains helper functions for locale-aware operations.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
 * Author: Aleksey Nogin <nogin@cs.cornell.edu>
 *
 * This file also contains code from str.c (C) Xavier Leroy
 * (provided there under GNU LGPL)
 */

#include <locale.h>
#include <ctype.h>
#include <caml/mlvalues.h>

value set_locale (value unit) {
	setlocale(LC_ALL, NULL);
	return Val_unit;
}

value is_print(value chr) {
	int c = Int_val(chr);
	return Val_bool(isprint(c));
}

value is_alnum(value chr) {
	int c = Int_val(chr);
	return Val_bool(isalnum(c));
}

value is_upper(value chr) {
	int c = Int_val(chr);
	return Val_bool(isupper(c));
}

value is_digit(value chr) {
	int c = Int_val(chr);
	return Val_bool(isdigit(c));
}

value is_graph(value chr) {
	int c = Int_val(chr);
	return Val_bool(isgraph(c));
}
