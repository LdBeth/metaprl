/**
 * Break the line.
 * This break is taken unless the break mode is linear.
 */

class TermBreak
extends TermDisplay
{
    /**
     * This string is printed if the break is taken.
     */
    final String take;

    /**
     * This string is printed if the break is not taken.
     */
    final String linear;

    /**
     * Zone is either soft or hard.
     */
    final boolean soft;

    /**
     * Make a new break.
     */
    TermBreak(boolean soft, String take, String linear)
    {
        this.soft = soft;
        this.take = take == null ? "" : take;
        this.linear = linear == null ? "" : linear;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:50  jyh
 * This is a simple term display in an applet.
 *
 */
