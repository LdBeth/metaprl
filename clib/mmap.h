/*
 * Provide an mmap implementation of a file.
 *
 * ------------------------------------------------------------
 *
 * This is part of the Ensemble Juke Box, a program for
 * distributed digital audio.
 *
 * Copyright (C) 1997 Cornell University
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
 * Authors: Jason Hickey, Mark Hayden
 * jyh,hayden@cs.cornell.edu
 * http://www.cs.cornell.edu/home/jyh/ejb/index.html
 */

#ifdef __GNUC__
#pragma interface
#endif __GNUC__

#ifndef __MMAP_H
#define __MMAP_H

value ml_mmap_open(value name, value perms, value mode);
value ml_mmap_close(value mmap);
value ml_string_of_mmap(value mmap);

#endif __MMAP_H

/*
 * $Log$
 * Revision 1.1  1999/01/08 21:50:36  jyh
 * This is the version of the distributed prover used in the
 * CADE-16 original paper.  I'm still adjusting it though, so
 * that we can use term_ds and native-code.
 *
 * If any of you have problems compiling clib/mmap.c, let me know.  It
 * should compile on Linux and Win32, but we should put in stubs
 * if there are problems on other systems.
 *
 * Revision 1.3  1997/05/13 13:42:05  jyh
 * Fixed nested playing.
 *
 * Revision 1.2  1997/05/12 14:32:33  jyh
 * Fixed sequential playing from different sources.
 * Updated CVS and copyright info.
 * Lots of small changes getting ready for initial release.
 *
 * Revision 1.1  1997/01/20 19:02:48  jyh
 * Memory mapped file.
 */
