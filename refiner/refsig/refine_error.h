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

/*
 * $Log$
 * Revision 1.2  1998/07/03 22:05:48  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.1  1998/07/02 18:35:47  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 */
