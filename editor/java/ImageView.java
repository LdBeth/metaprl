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

