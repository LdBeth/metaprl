/*
 * Set a variable in the environment.
 */
#include <stdio.h>
#include <sys/ioctl.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

extern void failwith(const char *s);

value caml_term_size(value arg)
{
    CAMLparam1(arg);
    CAMLlocal1(buf);
    struct winsize ws;

    /* Get the terminal size, return None on failure */
    if(ioctl(Int_val(arg), TIOCGWINSZ, &ws) < 0)
        failwith("termsize: standard input is not a terminal");

    /* Return the pair of numbers */
    buf = alloc_small(2, 0);
    Field(buf, 0) = Val_int(ws.ws_row);
    Field(buf, 1) = Val_int(ws.ws_col);
    CAMLreturn(buf);
}

