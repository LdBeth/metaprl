/*
 * Provide a primitive exit function.
 */
#include <stdio.h>
#ifdef _WIN32
#  include <windows.h>
#  include <process.h>
#else
#  include <unistd.h>
#endif

#include <caml/mlvalues.h>

value caml_exit(value code)
{
    int ecode = Int_val(code);
    _exit(ecode);
    return Val_unit;
}
