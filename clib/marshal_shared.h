/*
 * This file contains helper functions for the marshal module.
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
 */

#ifdef __GNUC__
#pragma implementation
#endif __GNUC__

#include <caml/memory.h>
#include <caml/mlvalues.h>

/*
 * Write the contents of the block to the string.
 */
value ml_write_block(value buf, value v_start, value v_stop, value obj);

/*
 *
 *
 * $Log$
 * Revision 1.1  1999/05/03 12:11:20  jyh
 * Added an initial incomplete version of the distributed marshaler.
 *
 */
