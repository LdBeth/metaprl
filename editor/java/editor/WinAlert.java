/*
 * This defines some notification functions.
 */
public abstract interface WinAlert
extends Marshalable
{
    public abstract void showStatus(String msg);
    public abstract void resetStatus();
    public abstract void showAlert(String msg);
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:29  jyh
 * This is a simple term display in an applet.
 *
 */
