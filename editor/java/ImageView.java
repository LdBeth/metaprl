/*
 * This is a view that contains an image.
 */
import netscape.application.*;
import netscape.util.*;

public class ImageView
extends View
implements DrawingSequenceOwner
{
    /**
     * The image to display.
     */
    protected Image image;

    /**
     * Icon image must be passed as an argument.
     */
    public ImageView(Image i, int x, int y)
    {
        super();
        image = i;
        int width = i.width();
        int height = i.height();
        setBounds(x, y, width, height);
        setMinSize(width, height);

        // Set sequence owner
        if(i instanceof DrawingSequence)
            ((DrawingSequence) i).setOwner(this);
    }

    /**
     * Transparency depends on transparency of image.
     */
    public boolean isTransparent()
    {
        return false; // image.isTransparent();
    }

    /**
     * Draw the image when we get it.
     */
    public void drawView(Graphics g)
    {
        super.drawView(g);
        image.drawAt(g, 0, 0);
    }

    /**
     * Return the image
     */
    public Image Image()
    {
        return image;
    }

    /**
     * Sequence is complete.
     */
    public void drawingSequenceCompleted(DrawingSequence image)
    {
    }

    /**
     * Image changed.
     */
    public void drawingSequenceFrameChanged(DrawingSequence image)
    {
        setDirty(true);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/09 15:43:46  jyh
 * Prelimnary semi-working version.
 *
 * Revision 1.3  1997/12/15 22:16:40  jyh
 * First working version using ocaml-1.07
 *
 * Revision 1.2  1997/12/15 15:25:52  jyh
 * Upgrading to ocaml-1.07.
 *
 * Revision 1.1  1997/10/15 01:25:13  jyh
 * Converted from ranks to abstract handles (Appl_handle).
 *
 */
