/*
 * These are variables for debuggin, and a hook for
 * manipulating them from ML.
 *
 * $Log$
 * Revision 1.1  1998/04/24 02:41:16  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.3  1998/01/09 19:29:53  jyh
 * Removed NT client.
 *
 * Revision 1.2  1997/11/04 04:26:34  jyh
 * Working win32 version of debug.
 *
 * Revision 1.1  1997/11/04 01:49:19  jyh
 * Initial version of debugging.
 *
 */

#ifdef __GNUC__
#pragma implementation
#endif __GNUC__

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#include <stdio.h>

#include "debug.h"
#include "ml_debug.h"

extern void failwith(const char *s);
extern mlsize_t string_length(value);

/*
 * Out local description.
 */
struct debug_info {
    char *name;
    char *info;
    int *flag;
};

static struct debug_info info[] = {
    /* { "c_adpcm",          "Compression code",                             &debug_adpcm }, */
};

#define INFO_LEN        (sizeof(info) / sizeof(*info))

/*
 * Modify a particular variable.
 * This return units, or raises an exception.
 */
value ml_debug(value name, value flag)
{
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

    // Did not find it
    failwith("ml_debug");
    return Val_int(0);
}

/*
 * Get the value of a particular variable.
 * Returns string * bool
 */
value ml_get_debug(value name, value flag)
{
    value rval;
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
}

