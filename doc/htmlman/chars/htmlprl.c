/*
 * Translate a PRL file to an HTML file.
 *
 * $Log$
 * Revision 1.1  1998/09/22 22:25:36  jyh
 * Added some documentation.  This HTML is still incomplete!  But it
 * should help understand the system.  Don't bother fixing spelling, etc,
 * because I will probably overwrite them when I make a final pass over
 * this first version.  Feel free to add documentation of course:-)
 *
 * Start in doc/htmlman/default.html
 *
 */

#include <stdio.h>

#ifdef __GNUC__
#pragma implementation
#endif __GNUC__

/*
 * Print the string for the image.
 */
static inline void print_char(int c)
{
    printf("<IMG WIDTH=8 HEIGHT=12 SRC=\"chars/CHAR%d.gif\">\n", c);
}

static void copy(FILE *fp)
{
    int c;

    while((c = getc(fp)) != EOF) {
        if(c == '\n')
            printf("<BR>\n");
        else if(c < 127)
            putchar(c);
        else
            print_char(c);
    }
}

int main(int argc, char **argv)
{
    int i, error;

    /* Parse options */
    error = 0;
    for(i = 1; i != argc && argv[i][0] == '-'; i++) {
        switch(argv[i][1]) {
        default:
            error++;
            break;
        }
    }

    /* Catch errors */
    if(error) {
        fprintf(stderr, "usage: %s filenames...\n", argv[0]);
        return 1;
    }

    /* Read the files */
    if(i == argc)
        copy(stdin);
    else {
        while(i != argc) {
            FILE *filep = fopen(argv[i], "r");
            if(filep == 0)
                fprintf(stderr, "%s: can't open %s\n", argv[0], argv[i]);
            else {
                copy(filep);
                fclose(filep);
            }
            i++;
        }
    }

    return 0;
}
