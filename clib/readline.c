/*
 * Call the readline package.
 */
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

/*
 * Max line length without readline.
 */
#define MAX_LINE_LENGTH         1024

/*
 * Read a line into a string buffer.
 * Returns a string option, None at EOF.
 */
value caml_readline(value prompt_arg)
{
    CAMLparam1(prompt_arg);
    CAMLlocal2(v, b);
    char *line, *bufp;

#ifdef READLINE
    line = readline(String_val(prompt_arg));

    /* Readline returns null on EOF */
    if(line == 0) {
        /* None */
        CAMLreturn Val_int(0);
    }

    /* This (probably) copies the line */
    add_history(line);

#else
    bufp = malloc(MAX_LINE_LENGTH);
    if(bufp == 0) {
        /* Pretend that we have reached EOF */
        CAMLreturn Val_int(0);
    }

    /* Get the line (make sure string is terminated) */
    bufp[MAX_LINE_LENGTH - 1] = 0;
    fputs(String_val(prompt_arg), stdout);
    line = fgets(bufp, MAX_LINE_LENGTH - 1, stdin);

    /* Readline returns null on EOF */
    if(line == 0) {
        /* None */
        free(bufp);
        CAMLreturn Val_int(0);
    }
#endif

    /* Copy the line */
    v = copy_string(line);

    /* Some v */
    b = alloc(1, 0);
    Field(b, 0) = v;

    /* Free the buffer */
    free(line);

    CAMLreturn b;
}
