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

#ifdef PROF
#include <sys/gmon.h>
extern int mp_gmon_status;
#endif

#include <caml/mlvalues.h>

value caml_exit(value code)
{
    int ecode = Int_val(code);
#ifdef PROF
    if (mp_gmon_status) _mcleanup();
#endif
    _exit(ecode);
    return Val_unit;
}
