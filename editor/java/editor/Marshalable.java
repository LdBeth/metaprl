/**
 * This interface just provides a function to
 * compute the total memory size of this object.
 * Also give numbers for ints etc.
 */

public abstract interface Marshalable
{
    /**
     * We keep a marker for the object.
     */
    int mark = 0;

    /**
     * Size of this object.
     */
    public abstract void Marshal(MarshalInfo info);
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:08  jyh
 * This is a simple term display in an applet.
 *
 * Revision 1.2  1997/12/15 23:39:35  jyh
 * New ocaml-1.07 version.
 *
 * Revision 1.1  1997/12/15 22:16:48  jyh
 * First working version using ocaml-1.07
 *
 * Revision 1.1  1997/12/15 15:26:01  jyh
 * Upgrading to ocaml-1.07.
 *
 */
