/*
 * Set a variable in the environment.
 */
#include <caml/mlvalues.h>

value caml_putenv(value var)
{
    return Val_int(putenv(String_val(var)));
}

