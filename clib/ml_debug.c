/*
 * These are variables for debuggin, and a hook for
 * manipulating them from ML.
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdio.h>

#include "debug.h"
#include "ml_debug.h"

/*
 * Out local description.
 */
struct debug_info {
    char *name;
    char *info;
    int *flag;
};

/* #define INFO_LEN  1 */

#ifdef INFO_LEN
static struct debug_info info[INFO_LEN] = {
    { "c_adpcm",          "Compression code",                             &debug_adpcm }
};
#endif


/*
 * Modify a particular variable.
 * This return units, or raises an exception.
 */
value ml_debug(value name, value flag)
{

#ifdef INFO_LEN
    char *l_name;
    int l_flag, i;

    l_name = String_val(name);
    l_flag = Int_val(flag);
    for(i = 0; i != INFO_LEN; i++) {
        if(strcmp(info[i].name, l_name) == 0) {
            *info[i].flag = l_flag;
            return Int_val(0);
        }
    }
#endif

    /* Did not find it */
    failwith("ml_debug");
    return Val_int(0);
}

/*
 * Get the value of a particular variable.
 * Returns string * bool
 */
value ml_get_debug(value name, value flag)
{
    value rval = 0;
#ifdef INFO_LEN
    char *l_name;
    int l_flag, i;

    Push_roots(r, 2);
#define tuple   r[0]
#define dinfo   r[1]

    /* Search for the variable */
    rval = 0;
    l_name = String_val(name);
    l_flag = Int_val(flag);
    for(i = 0; i != INFO_LEN; i++) {
        struct debug_info *infop = info + i;
        if(strcmp(infop->name, l_name) == 0) {
            dinfo = copy_string(infop->info);
            tuple = alloc_tuple(2);
            Field(tuple, 0) = dinfo;
            Field(tuple, 1) = Val_int(*infop->flag ? 1 : 0);
            rval = tuple;
        }
    }

#undef tuple
#undef dinfo
    Pop_roots();
#endif
    if(rval == 0)
        failwith("ml_get_debug");
    return rval;
}

/*
 * List all the flags.
 * Returns (string * string * bool) array
 */
value ml_debuggers(value unit)
{
#ifdef INFO_LEN
    value rval;
    int i;

    Push_roots(r, 4);
#define rinfo   r[0]
#define dname   r[1]
#define dinfo   r[2]
#define dpair   r[3]

    // Array of string pairs
    rinfo = alloc_shr(INFO_LEN, 0);
    for(i = 0; i != INFO_LEN; i++)
        initialize(&Field(rinfo, i), Int_val(0));

    // Now allocate string pairs
    for(i = 0; i != INFO_LEN; i++) {
        struct debug_info *infop = info + i;
        dname = copy_string(infop->name);
        dinfo = copy_string(infop->info);
        dpair = alloc_tuple(3);
        Field(dpair, 0) = dname;
        Field(dpair, 1) = dinfo;
        Field(dpair, 2) = Val_int(*infop->flag ? 1 : 0);
        initialize(&Field(rinfo, i), dpair);
    }

#undef dinfo
#undef name
#undef vinfo
#undef pair
    rval = rinfo;
    Pop_roots();
    return rval;
#else
    return Atom(0);
#endif
}

