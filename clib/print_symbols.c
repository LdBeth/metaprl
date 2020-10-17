/*
 * This is a registry for global values that we don't want the
 * marshaler to see.  Each value is registered and retreived with
 * a number.  This is done in C because we don't want the registered
 * values to appear in the closures of any of the registration
 * functions.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#include <stdio.h>

#include "print_symbols.h"

/* To print out the function symbols */
value function_symbols = 0;

/* Store the symbol array */
value ml_extern_symbols(value symbols)
{
    register_global_root(&function_symbols);
    function_symbols = symbols;
    return Val_unit;
}

/*
 * Print the function symbol at a location.
 * Binary search.
 */
void print_symbol(int pc)
{
    int i, j, k, pos = 0, cpos, length;
    const char *modname;
    value symbol;

    if(function_symbols) {
        length = Wosize_val(function_symbols);
        i = -1;
        j = length;
        symbol = 0;
        while(i < j - 1) {
            k = (i + j) >> 1;
            symbol = Field(function_symbols, k);
            pos = Int_val(Field(symbol, 0));
            if(pos == pc)
                break;
            else if(pos > pc)
                j = k;
            else
                i = k;
        }
        if(symbol) {
            modname = String_val(Field(symbol, 1));
            cpos = Int_val(Field(symbol, 2));
            if(pos == pc)
                fprintf(stderr, "marshaling function (0x%08x): %s/%d\n", pc, modname, cpos);
            else
                fprintf(stderr, "marshaling function (0x%08x:0x%08x): %s/%d\n", pc, pos, modname, cpos);
        }
        else
            fprintf(stderr, "marshaling function (0x%08x)\n", pc);
    }
}
