/**
 * This is a window to display a term in a TextViewBuffer.
 */

import netscape.application.*;
import netscape.util.*;

public class TermView
extends TextViewBuffer
{
    /**
     * Keep a handle to the formatter.
     */
    protected DisplayTerm display;

    /**
     * Target for mouse enter/exit events.
     */
    protected Target target;

    /**
     * Mouse-enter command that is passed to the target.
     */
    protected String mouse_entered;

    /**
     * Mouse-exit command that is passed to the target.
     */
    protected String mouse_exited;

    /**
     * New window.
     */
    public TermView(DisplayTerm display, FontBase base, int x, int y, int w, int h)
    {
        super(base, x, y, w, h);
        this.display = display;
        setBackgroundColor(new Color(230, 230, 255));
    }

    /**
     * Display a term in the window.
     */
    public void appendTerm(Term term)
    {
        try {
            Vector terms = display.eval(term);
            display.format(terms, TermZone.SOFT_ZONE, this);
            setDirty(true);
        }
        catch(EvalError x) {
            appendString("<term evaluation failed>");
        }
    }

    /**
     * Set the target.
     */
    public void setTarget(Target target)
    {
        this.target = target;
    }

    /**
     * Set the mouse-enter command.
     */
    public void setEnteredCommand(String command)
    {
        mouse_entered = command;
    }

    /**
     * Set the mouse-exit command.
     */
    public void setExitedCommand(String command)
    {
        mouse_exited = command;
    }

    /*
     * Pass up the pseudo-focus events.
     */
    public void mouseEntered(MouseEvent e)
    {
        if(target != null)
            target.performCommand(mouse_entered, this);
        super.mouseEntered(e);
    }

    public void mouseExited(MouseEvent e)
    {
        if(target != null)
            target.performCommand(mouse_exited, this);
        super.mouseExited(e);
    }

    /**
     * Get the size we want.
     */
    public int desiredWidth()
    {
        return max_column + 10;
    }

    /**
     * Get the height we want.
     */
    public int desiredHeight()
    {
        return height();
    }
}

