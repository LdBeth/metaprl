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

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:05  jyh
 * This is a simple term display in an applet.
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
