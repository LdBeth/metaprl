/**
 * This just adds a JavaScript interface to the standard application.
 */

import netscape.application.*;

public abstract class ActiveApplication
extends Application
{
    /**
     * Return the width we really want.
     */
    public abstract int desiredWidth();

    /**
     * Return the height we really want.
     */
    public abstract int desiredHeight();
}

