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

