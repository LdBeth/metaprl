/*
 * ML version of getrusage.
 */

#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#ifdef WIN32

/*
 * Not implemented on Win32.
 */
value ml_getrusage(value unit_val)
{
    CAMLparam1(unit_val);
    CAMLlocal1(rval);
    int i;

    rval = alloc_tuple(16);
    for(i = 0; i != 16; i++)
        Field(rval, i) = Val_unit;
    Field(rval, 0) = copy_double(0.0);
    Field(rval, 1) = copy_double(0.0);
    Field(rval, 2) = Val_int(0);
    Field(rval, 3) = Val_int(0);
    Field(rval, 4) = Val_int(0);
    Field(rval, 5) = Val_int(0);
    Field(rval, 6) = Val_int(0);
    Field(rval, 7) = Val_int(0);
    Field(rval, 8) = Val_int(0);
    Field(rval, 9) = Val_int(0);
    Field(rval, 10) = Val_int(0);
    Field(rval, 11) = Val_int(0);
    Field(rval, 12) = Val_int(0);
    Field(rval, 13) = Val_int(0);
    Field(rval, 14) = Val_int(0);
    Field(rval, 15) = Val_int(0);

    CAMLreturn(rval);
}

#else /* not WIN32 */

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

/* JDS: this isn't defined in any OCaml header, seems to be 
        an internal function.  Oh well... *shrug* */
extern void uerror(char *cmdname, value arg) Noreturn;

#define  Nothing  ((value) 0)

/*
 * Get the resources consumed.
 */
value ml_getrusage(value unit_val)
{
	 CAMLparam1(unit_val);
    struct rusage rsrc;
    CAMLlocal1(rval);
    int ret, i;

    ret = getrusage(RUSAGE_SELF, &rsrc);
    if(ret < 0)
        uerror("getrusage", Nothing);
    rval = alloc_tuple(16);
    for(i = 0; i != 16; i++)
        Field(rval, i) = Val_unit;
    fprintf(stderr, "User: %f System: %f\n",
            rsrc.ru_utime.tv_sec + rsrc.ru_utime.tv_usec * 1e-6,
            rsrc.ru_stime.tv_sec + rsrc.ru_stime.tv_usec * 1e-6);
    Field(rval, 0) = copy_double(rsrc.ru_utime.tv_sec + rsrc.ru_utime.tv_usec * 1e-6);
    Field(rval, 1) = copy_double(rsrc.ru_stime.tv_sec + rsrc.ru_stime.tv_usec * 1e-6);
    Field(rval, 2) = Val_int(rsrc.ru_maxrss);
    Field(rval, 3) = Val_int(rsrc.ru_ixrss);
    Field(rval, 4) = Val_int(rsrc.ru_idrss);
    Field(rval, 5) = Val_int(rsrc.ru_isrss);
    Field(rval, 6) = Val_int(rsrc.ru_minflt);
    Field(rval, 7) = Val_int(rsrc.ru_majflt);
    Field(rval, 8) = Val_int(rsrc.ru_nswap);
    Field(rval, 9) = Val_int(rsrc.ru_inblock);
    Field(rval, 10) = Val_int(rsrc.ru_oublock);
    Field(rval, 11) = Val_int(rsrc.ru_msgsnd);
    Field(rval, 12) = Val_int(rsrc.ru_msgrcv);
    Field(rval, 13) = Val_int(rsrc.ru_nsignals);
    Field(rval, 14) = Val_int(rsrc.ru_nvcsw);
    Field(rval, 15) = Val_int(rsrc.ru_nivcsw);

    CAMLreturn(rval);
}
#endif
