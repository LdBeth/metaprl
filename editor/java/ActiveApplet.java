/**
 * We enhance NetscapeApplet with an additional interface
 * to be called from JavaScript.
 */

public class ActiveApplet
extends NetscapeApplet
{
    /**
     * Desired width.
     */
    public int desiredWidth()
    {
        return ((ActiveApplication) application()).desiredWidth();
    }

    /**
     * Desired height.
     */
    public int desiredHeight()
    {
        return ((ActiveApplication) application()).desiredHeight();
    }
}

