/*
 * Set a variable in the environment.
 */
#include <stdio.h>
#include <sys/ioctl.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

value caml_term_size(value arg)
{
    CAMLparam1(arg);
    CAMLlocal1(buf);
    struct winsize ws;

    /* Get the terminal size, return None on failure */
    if(ioctl(0, TIOCGWINSZ, &ws) < 0) {
        CAMLreturn Val_unit;
    }

    /* Return the pair of numbers */
    buf = alloc_small(2, 1);
    Field(buf, 0) = Val_int(ws.ws_row);
    Field(buf, 1) = Val_int(ws.ws_col);
    CAMLreturn buf;
}

