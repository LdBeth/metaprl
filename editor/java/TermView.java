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
     * New window.
     */
    public TermView(DisplayTerm display, FontBase base, int x, int y, int w, int h)
    {
        super(base, x, y, w, h);
        this.display = display;
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
     * This is not the min size, it is a size we would like.
     */
    public Size querySize()
    {
        int lines = lineCount();
        return new Size(max_column, lines * 20);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:11  jyh
 * This is a simple term display in an applet.
 *
 */
