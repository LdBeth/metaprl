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

/*
 * $Log$
 * Revision 1.1  1998/02/09 15:43:40  jyh
 * Prelimnary semi-working version.
 *
 */
