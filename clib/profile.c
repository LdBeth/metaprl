/* Profiling control */

#include <caml/mlvalues.h>

#ifdef PROF
#include <sys/gmon.h>
#endif

value stop_gmon (value var) {
#ifdef PROF
   _mcleanup ();
#endif
   return Val_unit;
}

value restart_gmon (value var)
{
#ifdef PROF
   u_long lowpc=_gmonparam.lowpc;
   u_long highpc=_gmonparam.highpc;

   _mcleanup ();
   monstartup (lowpc,highpc);
#endif
   
   return Val_unit;
}

