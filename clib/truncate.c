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

