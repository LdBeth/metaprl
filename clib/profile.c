/* Profiling control */

#include <caml/mlvalues.h>

#ifdef PROF
#include <sys/gmon.h>
extern void moncontrol (int mode);
int mp_gmon_status = 1;
#endif

value stop_gmon (value var) {
#ifdef PROF
   _mcleanup ();
	mp_gmon_status = 0;
#endif
   return Val_unit;
}

value restart_gmon (value var)
{
#ifdef PROF
	if (mp_gmon_status) {
		moncontrol(0);
		moncontrol(1);
	}
#endif

   return Val_unit;
}

