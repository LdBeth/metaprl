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

