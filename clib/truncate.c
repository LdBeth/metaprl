/*
 * For Cygwin32, truncate a file.
 */
#ifdef __CYGWIN32__

#include <unistd.h>
#include <fcntl.h>

int errno;

int truncate(const char *name, off_t length)
{
    int fd, code, xerrno;

    fd = open(name, O_WRONLY);
    if(fd < 0)
        return -1;
    code = ftruncate(fd, length);
    xerrno = errno;
    close(fd);
    errno = xerrno;
    return code;
}

#endif /* __CYGWIN32__ */

/*
 * $Log$
 * Revision 1.1  1998/05/27 15:12:33  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.1  1998/02/18 18:56:29  jyh
 * Added truncate(2) for cygwin32.
 *
 * Revision 1.1  1997/08/06 16:17:00  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 */
