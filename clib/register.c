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
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

extern void failwith(const char *s);

/*
 * This is the global array of values.
 */
#define MAX_REGISTRY_SIZE       64

static value entries = Val_unit;

/*
 * Store a value in the array.
 * Expand the array if necessary.
 */
value ml_set_register(value id, value val)
{
    int index, i;
    Begin_roots1(val);

    /* Check index */
    index = Int_val(id);
    if(index < 0 || index >= MAX_REGISTRY_SIZE)
        failwith("ml_register: value out of range");

    /* Initialize */
    if(entries == Val_unit) {
        /* Allocate the array */
        register_global_root(&entries);
        entries = alloc_shr(MAX_REGISTRY_SIZE, 0);

        /* Initialize it */
        for(i = 0; i != MAX_REGISTRY_SIZE; i++)
            initialize(&Field(entries, i), Val_unit);
    }

    /* Assign it */
    modify(&Field(entries, index), val);
    End_roots();

    return id;
}

/*
 * Get a value in the registry.
 */
value ml_get_register(value id)
{
    value v;

    int index = Int_val(id);
    if(index < 0 || index >= MAX_REGISTRY_SIZE)
        failwith("ml_get_register: invalid index");

    v = Field(entries, index);
    if(v == Val_unit)
        failwith("ml_get_register: bogus index");

    /* JDS:  This function didn't return *anything* in original
       version; I'm adding this return because this seems to be
       what is expected, but... *shrug* */
    return v;
}

/*
 * -*-
 * Local Variables:
 * Caml-master: "mp.run"
 * End:
 * -*-
 */
