/**
 * This is a ScrollGroup that doesn't allow the vertical scrollbar
 * on small views.
 */

import netscape.application.*;

public class SmallScrollGroup
extends ScrollGroup
{
    /**
     * Create a new one with the given size.
     */
    public SmallScrollGroup(int x, int y, int w, int h)
    {
        super(x, y, w, h);
    }

    /**
     * Intercept the layoutView to see if we really
     * want the scrollbar.
     */
    private boolean recursive = false;

    public void layoutView(int x, int y)
    {
        if(recursive == false) {
            try {
                recursive = true;
                int code = y + bounds.height > 100 ? AS_NEEDED_DISPLAY : NEVER_DISPLAY;
                setVertScrollBarDisplay(code);
                super.layoutView(x, y);
            }
            finally {
                recursive = false;
            }
        }
    }
}

