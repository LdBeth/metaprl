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

