(*
 * We duplicate the modules, with verbose
 * and simplified error reporting.
 *
 * Can't do this with the module system unfortunately.
 *)
#define raise_generic_exn       (raise generic_refiner_exn)

#ifdef SIMPLE_EXN
#  define ref_raise(exn)        raise_generic_exn
#  undef VERBOSE_EXN
#else
#  ifndef VERBOSE_EXN
#    define VERBOSE_EXN
#  endif
#  define ref_raise(exn)        (raise (exn))
#endif

