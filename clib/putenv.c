/*
 * Set a variable in the environment.
 */
#include <caml/mlvalues.h>

value caml_putenv(value var)
{
    return Val_int(putenv(String_val(var)));
}

/*
 * $Log$
 * Revision 1.1  1997/08/06 16:17:00  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 */
