/**
 * This show a status bar as an internal window.
 * There is an option to show the entire
 * communication.
 */

import netscape.application.*;

/**
 * The window has a status line, and a turnstile to
 * switch in the complete communication.
 *
 * The window uses PackLayout, so make sure
 * to relayout the window when the turnstile
 * is pressed.
 */
class Status
extends IconWindow
implements WinAlert, Target, Marshalable
{
    /*
     * Size of the window.
     * large_size is the height of the window when it
     * includes the verbose display.
     */
    private final static int BORDER_SIZE = 3;
    private final static int CONTENT_SIZE = 16;
    private final static String STATUS_COM = "status";
    private Rect large_rect;
    private int small_y;

    /*
     * Widgets
     */
    private PackLayout layout;
    private Button turnstile;
    private Label status;
    private Debug debug;

    /**
     * Record size.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(large_rect);
        info.Marshal(small_y);
        info.Marshal(layout);
        info.Marshal((View) turnstile);
        info.Marshal((View) status);
        info.Marshal((Marshalable) debug);
    }

    /**
     * Show debug window
     */
    protected void ShowDebug(boolean flag)
    {
        if(flag) {
            addSubview(debug);
            setResizable(true);
            if(large_rect == null)
                large_rect = new Rect(bounds.x, bounds.y, bounds.width, 200);
            else {
                large_rect.x = bounds.x;
                large_rect.y = bounds.y;
                large_rect.width = bounds.width;
            }
            View parent = superview();
            int h = parent.height();
            if(large_rect.y + large_rect.height > h)
                large_rect.y = h - large_rect.height;
            if(large_rect.y < 0)
                large_rect.y = 0;
            small_y = y();
            setBounds(large_rect);
        }
        else {
            boolean restore = bounds.equals(large_rect);
            large_rect = bounds();
            setResizable(false);
            Size size = windowSizeForContentSize(0, CONTENT_SIZE + 2 * BORDER_SIZE);
            debug.removeFromSuperview();
            if(restore)
                setBounds(x(), small_y, width(), size.height);
            else
                sizeTo(width(), size.height);
        }
        layout.layoutView(contentView(), 0, 0);
        setDirty(true);
    }

    /*
     * Status toggle
     */
    public void performCommand(String code, Object obj)
    {
        if(code.equals(STATUS_COM))
            ShowDebug(turnstile.state());
    }

    /**
     * Setup the window
     */
    Status(int x, int y, int w, int h, int ix, int iy)
    {
        super(Bitmap.bitmapNamed("info.gif"), x, y, w, CONTENT_SIZE + 2 * BORDER_SIZE + 20, ix, iy);

        // Turnstile
        Bitmap turn_close = Bitmap.bitmapNamed("turn_close.gif");
        turn_close.setTransparent(true);
        Bitmap turn_open = Bitmap.bitmapNamed("turn_open.gif");
        turn_open.setTransparent(true);
        turnstile = new Button(BORDER_SIZE, BORDER_SIZE, CONTENT_SIZE, CONTENT_SIZE);
        turnstile.setType(Button.TOGGLE_TYPE);
        turnstile.setTarget(this);
        turnstile.setCommand(STATUS_COM);
        turnstile.setImage(turn_close);
        turnstile.setAltImage(turn_open);
        turnstile.setTransparent(true);

        // Status label
        status = new Label();
        status.moveTo(22, 0);
        status.setJustification(Graphics.LEFT_JUSTIFIED);
        status.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
        status.setVertResizeInstruction(View.CENTER_VERT);

        // Status line
        ContainerView status_line = new ContainerView(0, 0, 100, CONTENT_SIZE + 2 * BORDER_SIZE);
        status_line.addSubview(turnstile);
        status_line.addSubview(status);

        // Layout
        View content = contentView();
        layout = new PackLayout();
        content.setLayoutManager(layout);

        // Status line
        PackConstraints pack = new PackConstraints(
            PackConstraints.ANCHOR_NORTHWEST,
            false,               // expand
            true,                // fillX
            false,               // fillY
            0,                   // iPadX
            0,                   // iPadY
            0,                   // padX
            0,                   // padY
            PackConstraints.SIDE_TOP);
        addSubview(status_line);
        layout.setConstraints(status_line, pack);

        // Constraints when debug window is added
        debug = new Debug();
        pack = new PackConstraints(
            PackConstraints.ANCHOR_NORTHWEST,
            true,                // expand
            true,                // fillX
            true,                // fillY
            0,                   // iPadX
            0,                   // iPadY
            0,                   // padX
            0,                   // padY
            PackConstraints.SIDE_TOP);
        layout.setDefaultConstraints(pack);

        // Configure this window
        setCloseable(true);
        Size size = windowSizeForContentSize(0, CONTENT_SIZE + 2 * BORDER_SIZE);
        sizeTo(width(), size.height);
        layout.layoutView(content, 0, 0);
    }

    /**
     * Show a status message.
     */
    public void showStatus(String m)
    {
        System.out.println(m);
        status.setTitle(m);
        status.setDirty(true);
    }

    /**
     * Clear the status message.
     */
    public void resetStatus()
    {
        status.setTitle("");
        status.setDirty(true);
    }

    /**
     * Show an modal alert box.
     */
    public void showAlert(String m)
    {
        showStatus(m);
        Alert.runAlertInternally("Error", m, "OK", null, null);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:24  jyh
 * This is a simple term display in an applet.
 *
 */
