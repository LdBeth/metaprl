/*
 * This is like a label, but it contains an image.
 */
import netscape.application.*;
import netscape.util.*;

public class ImageLabel
extends View
implements Marshalable
{
    // The image
    private Image image;

    /**
     * Traverse thi object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(image);
    }

    /**
     * Icon image must be passed as an argument.
     */
    public ImageLabel(Image i, int x, int y)
    {
        super();
        image = i;
        int width = i.width();
        int height = i.height();
        setBounds(x, y, width, height);
        setMinSize(width, height);
    }

    /**
     * Transparency depends on transparency of image.
     */
    public boolean isTransparent()
    {
        return image.isTransparent();
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
}

