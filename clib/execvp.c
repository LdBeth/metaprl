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

value caml_execv(value command, value argv)
{
    char *command_str = String_val(command);
    int len = Wosize_val(argv);
    char *argv_str[len + 1];
    int i;
    for(i = 0; i != len; i++)
        argv_str[i] = String_val(Field(argv, i));
    argv_str[i] = 0;
#if 1
    for(i = 0; i != len; i++)
        fprintf(stderr, "%s ", argv_str[i]);
    putc('\n', stderr);
#endif
    return Val_int(execv(command_str, argv_str));
}

value caml_execvp(value command, value argv)
{
    char *command_str = String_val(command);
    int len = Wosize_val(argv);
    const char *argv_str[len + 1];
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
        return Val_int(code);
    }
#else
    return Val_int(execvp(command_str, argv_str));
#endif
}

