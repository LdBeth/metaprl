/**
 * Context carried around by each window.
 */

import netscape.application.*;

class Context
implements Marshalable
{
    /**
     * The file chooser is shared in common by all
     * the windows.
     */
    public FileChooser chooser;

    /**
     * Record of current status.
     */
    public Status status;

    /**
     * Alert window.
     */
    public WinAlert alert;

    /**
     * Traverse the object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal((Marshalable) chooser);
        info.Marshal(alert);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:50  jyh
 * This is a simple term display in an applet.
 *
 */
