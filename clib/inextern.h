/*
 * An extra marshaling tag.
 *
 * $Log$
 * Revision 1.1  1999/01/08 21:50:35  jyh
 * This is the version of the distributed prover used in the
 * CADE-16 original paper.  I'm still adjusting it though, so
 * that we can use term_ds and native-code.
 *
 * If any of you have problems compiling clib/mmap.c, let me know.  It
 * should compile on Linux and Win32, but we should put in stubs
 * if there are problems on other systems.
 *
 */
#ifndef _INEXTERN_H
#define _INEXTERN_H

#ifdef __GNUC__
#pragma interface
#endif __GNUC__

#define CODE_CODEPOINTER2       0x12

#endif _INEXTERNH_
