/*
 * This just implements an icon, which is an Image that can be
 * dragged around.  It is implemented as an internal window, so
 * it can float around the desktop, and it is also a dragSource,
 * so that it can be dragged around, and it may be dropped.
 */
import netscape.application.*;
import netscape.util.*;

public class Icon
extends InternalWindow
implements DragSource, Marshalable
{
    /** Flag is drag is in progress */
    boolean dragging;

    /** Image to display (IconImage defined below) */
    IconImage image;

    /** Target to receive click */
    Target target;

    /** The image to draw for this icon */
    private class IconImage
    extends ImageLabel
    {
        //
        // For catching mouse placement
        //
        private int x, y;

        //
        // Create from image.
        //
        IconImage(Image i)
        {
            super(i, 0, 0);
        }

        //
        // When mouse is pressed, save current placement
        //
        public boolean mouseDown(MouseEvent event)
        {
            x = event.x;
            y = event.y;
            return true;
        }

        //
        // If a mouseUp event is received without a drag,
        // then this item was just selected.
        //
        public void mouseUp(MouseEvent event)
        {
            if(dragging == false)
                target.performCommand("Show", Icon.this);
        }

        //
        // When mouse is dragged, start the DragSession.
        // Start only one DragSession.
        //
        public void mouseDragged(MouseEvent event)
        {
            if(dragging)
                return;
            new DragSession(
                Icon.this,      // DragSource is the InternalWindow
                Image(),        // Image to drag
                0, 0,           // Coordinates of this window
                x, y,           // Initial coordinates of mouse
                "icon",         // DataType
                Icon.this);     // handle
        }

        //
        // Ask for mouse tracking.
        //
        public boolean wantsMouseTrackingEvents()
        {
            return false;
        }
    }

    /**
     * The InternalWindow owns the view
     */
    public View sourceView(DragSession session)
    {
        return this;
    }

    /**
     * Destination accepted the drag.
     */
    public void dragWasAccepted(DragSession session)
    {
        // Drag is complete
        Rect rect = session.absoluteBounds();
        moveTo(rect.x, rect.y);
        dragging = false;
    }

    /**
     * If drag was rejected, return false to indicate
     * that we want to stay where we are.
     */
    public boolean dragWasRejected(DragSession session)
    {
        dragWasAccepted(session);
        return false;
    }

    /**
     * Icon image must be passed as an argument.
     */
    public Icon(Image i, int x, int y)
    {
        super(x, y, i.width(), i.height());

        // Icon has an image
        image = new IconImage(i);
        addSubview(image);

        // Set up the window
        setType(Window.BLANK_TYPE);
        setTransparent(image.isTransparent());
        setResizable(false);
        setCloseable(false);
    }

    /**
     * Icon is initially placed at the origin.
     */
    public Icon(Image i)
    {
        this(i, 0, 0);
    }

    /**
     * Provide the target.
     */
    public Icon(Image i, Target t)
    {
        this(i);
        setTarget(t);
    }

    /**
     * Set the target.
     */
    public void setTarget(Target t)
    {
        target = t;
    }

    /**
     * Compute total size, not including image.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(dragging);
        info.Marshal((Marshalable) image);
        info.Marshal(target);
    }
}
