/*
 * An operator name is an array of strings.
 */

public class Opname
{
    /**
     * The is the head string.
     */
    String head;

    /**
     * Pointer to the tail opname.
     */
    Opname tail;

    /**
     * Construction.
     */
    public Opname(String s, Opname n)
    {
        head = s;
        tail = n;
    }

    /**
     * Equality.
     */
    public boolean equals(Opname opname)
    {
        return head.equals(opname.head)
            && (tail == null ? opname.tail == null : tail.equals(opname.tail));
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:56  jyh
 * This is a simple term display in an applet.
 *
 */
