/**
 * Push the current tabstop to the current column
 * plus an offset in hundredths of a point.
 */

class TermPush
extends TermDisplay
{
    /**
     * The amount to add to the current column.
     */
    final int offset;

    /**
     * New one.
     */
    TermPush(int offset)
    {
        this.offset = offset;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:02  jyh
 * This is a simple term display in an applet.
 *
 */
