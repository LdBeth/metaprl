/*
 * We duplicate the modules, with verbose
 * and simplified error reporting.
 *
 * Can't do this with the module system unfortunately.
 */
#define raise_generic_exn       (raise generic_refiner_exn)

#ifdef VERBOSE_EXN
#  define ref_raise(exn)        (raise (exn))
#else
#  define ref_raise(exn)        raise_generic_exn
#endif

