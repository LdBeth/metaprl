/*
 * This is an internal window with an icon.
 * When the window is closed, this icon is raised.
 * When the icon is targetted, it is hidden,
 * and this window is shown.
 */

import netscape.application.*;

public class IconWindow
extends InternalWindow
implements WindowOwner, Marshalable
{
    /**
     * Icon for this window.
     */
    Icon icon;

    /**
     * Flag is set if the window is not being shown.
     */
    boolean hiding;

    /**
     * Window may be iconified and disabled.
     */
    boolean enabled;

    /**
     * Traverse the object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal((View) icon);
        info.Marshal(hiding);
        info.Marshal(enabled);
    }

    /************************************************************************
     * WINDOW OWNER                                                         *
     ************************************************************************/

    public void windowDidBecomeMain(Window w)
    {
    }

    public void windowDidHide(Window w)
    {
        if(hiding == false)
            icon.show();
    }

    public void windowDidResignMain(Window w)
    {
    }

    public void windowDidShow(Window w)
    {
        if(hiding == false)
            icon.hide();
    }

    public boolean windowWillHide(Window w)
    {
        return true;
    }

    public boolean windowWillShow(Window w)
    {
        return true;
    }

    public void windowWillSizeBy(Window w, Size s)
    {
    }

    /************************************************************************
     * WINDOW                                                               *
     ************************************************************************/

    /**
     * Unhide handler.
     */
    public void performCommand(String cmd, Object o)
    {
        if(cmd.equals("Show")) {
            if(enabled)
                IconWindow.this.show();
        }
        else
            super.performCommand(cmd, o);
    }

    /*
     * Set the icon that wil be used
     */
    private void setIcon(Icon i)
    {
        icon = i;
        icon.setTarget(this);
        setOwner(this);
        hiding = true;
    }

    /**
     * Create a window with an icon.
     */
    public IconWindow(Icon i)
    {
        super();
        setIcon(i);
        enabled = false;
    }

    /**
     * Create a sized window.
     */
    public IconWindow(Icon i, int x, int y, int w, int h)
    {
        super(x, y, w, h);
        setIcon(i);
    }

    /**
     * Create a sized window with an icon, and an icon position.
     */
    public IconWindow(Image i, int x, int y, int w, int h, int ix, int iy)
    {
        super(x, y, w, h);
        setIcon(new Icon(i, ix, iy));
    }

    /**
     * Get the icon.
     */
    public Icon icon()
    {
        return icon;
    }

    /**
     * Hide both icon and window.
     */
    public void Hide()
    {
        hiding = true;
        icon.hide();
        hide();
    }

    /**
     * Show the window.
     */
    public void Show()
    {
        enabled = true;
        hiding = false;
        show();
    }

    /**
     * Iconify the window.
     */
    public void Iconify()
    {
        hiding = false;
        hide();
        icon.show();
    }

    /**
     * Enable the window.
     */
    public void enable(boolean flag)
    {
        enabled = flag;
    }

    /**
     * Icon X position.
     */
    public int ix()
    {
        return icon.x();
    }

    /**
     * Icon y position.
     */
    public int iy()
    {
        return icon.y();
    }
}

