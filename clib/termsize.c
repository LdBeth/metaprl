/*
 * Set a variable in the environment.
 */
#include <stdio.h>
#ifdef __CYGWIN__
#   include <sys/termios.h>
#endif
#ifndef WIN32
#   include <sys/ioctl.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

value caml_term_size(value arg)
{
    CAMLparam1(arg);
    CAMLlocal1(buf);

    /* Return a pair of numbers */
    buf = alloc_small(2, 0);

    /* Get the terminal size, return None on failure */
#ifdef WIN32
    Field(buf, 0) = Val_int(24);
    Field(buf, 1) = Val_int(80);
#else
    {
        struct winsize ws;

        if(ioctl(Int_val(arg), TIOCGWINSZ, &ws) < 0)
            failwith("termsize: standard input is not a terminal");
    
        /* Return the pair of numbers */
        Field(buf, 0) = Val_int(ws.ws_row);
        Field(buf, 1) = Val_int(ws.ws_col);
    }
#endif

    CAMLreturn(buf);
}

