/*
 * Provide our own execvp because it fails on _win32.
 */
#include <stdio.h>
#ifdef _WIN32
#  include <windows.h>
#  include <process.h>
#else
#  include <unistd.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>

value caml_execv(value command, value argv)
{
	 CAMLparam2(command,argv);
    const char *command_str = String_val(command);
    int len = Wosize_val(argv);
    char **argv_str=malloc((len+1)*sizeof (char *));
    int i;
    value v;
    for(i = 0; i != len; i++)
        argv_str[i] = String_val(Field(argv, i));
    argv_str[i] = 0;
#if 1
    for(i = 0; i != len; i++)
        fprintf(stderr, "%s ", argv_str[i]);
    putc('\n', stderr);
#endif
    v = Val_int(execv(command_str, argv_str));
    free (argv_str);
    CAMLreturn(v);
}

value caml_execvp(value command, value argv)
{
	 CAMLparam2(command,argv);
    const char *command_str = String_val(command);
    int len = Wosize_val(argv);
    char **argv_str=malloc((len+1)*sizeof (char *));
    int i;
    for(i = 0; i != len; i++)
        argv_str[i] = String_val(Field(argv, i));
    argv_str[i] = 0;
#if 0
    for(i = 0; i != len; i++)
        fprintf(stderr, "%s ", argv_str[i]);
    putc('\n', stderr);
#endif
#ifdef _WIN32
    {
        int code = spawnvp(_P_OVERLAY, command_str, argv_str);
        perror("execvp");
        free(argv_str);
        CAMLreturn(Val_int(code));
    }
#else
    {
      value v = Val_int(execvp(command_str, argv_str));
      free (argv_str);
      CAMLreturn(v);
    }
#endif
}

