/**
 * This is a navigation toolbar.
 * It contains buttons for each of the four navigation types:
 *     1. cdroms
 *     2. files
 *     3. radios
 *     4. folders
 *
 * It also contains an address bar.
 */

import netscape.application.*;
import netscape.util.*;

/**
 * This is a subview that is an address menu.
 */
class AddressView
extends View
implements Target, Marshalable
{
    /** Text window displays the current address */
    protected TextField text;

    /** Popup menu display the path to the root */
    protected Popup popup;

    /** Target for receiving commands */
    protected Target target;

    /** Send this command to the target when the address changes */
    protected String command;

    /** Send this command to the target when the home button is pressed */
    protected String home_command;

    /** The roots are the root directories listed in the popup */
    protected Filename[] roots;

    /** This is the current path that is being displayed */
    protected Pathname dir;

    /** This is the index into the popup menu */
    private int index;

    // Commands
    private static final String COM_POPUP = "popdir";
    private static final String COM_CHDIR = "chdir";

    // Default sizes
    private static final int POPUP_WIDTH = 64;
    private static final int TEXT_HEIGHT = 25;

    /** Traverse the object */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal((View) text);
        info.Marshal((View) popup);
        info.Marshal(target);
        info.Marshal(command);
        info.Marshal(home_command);
        info.Marshal((Marshalable []) roots);
        info.Marshal(dir);
        info.Marshal(index);
    }

    //
    // Perform a command when a popup is selEcted.
    //
    public void performCommand(String command, Object obj)
    {      
        if(command.equals(COM_POPUP))
            HandlePopdir();
        else if(command.equals(COM_CHDIR))
            HandleChdir();
    }

    /*
     * Handle the popup button to change directories.
     */
    protected void HandlePopdir()
    {
        int i = popup.selectedIndex();
        if(i < index) {
            // Root directory
            if(DebugFlags.navigate)
                System.err.println("Initial directory: " + i);
            setPath(new Pathname(roots[i]));
        }
        else if(i < index + dir.count()) {
            // Popup some directories
            if(DebugFlags.navigate)
                System.err.println("Subdirectory: " + (i - index) + "/" + dir.count());
            setPath(dir.parent(i - index + 1));
        }
        else {
            // Root directory
            if(DebugFlags.navigate)
                System.err.println("After directory: " + (i - dir.count() + 1));
            setPath(new Pathname(roots[i - dir.count() + 1]));
        }
        target.performCommand(command, dir);
    }

    /*
     * Handle a complete change of directory.
     * The address is based on shortnames,
     * and long filename informtion is lost.
     * We assume that the target will restore
     * long filename info.
     */
    protected void HandleChdir()
    {
        Pathname path = new Pathname(text.stringValue());
        setPath(path);
        target.performCommand(command, dir);
    }

    /**
     * Get the directory.
     */
    public Pathname path()
    {
        return dir;
    }

    /**
     * Set the current directory.
     * This also resets the popup menu.
     */
    protected void setPath(Pathname path)
    {
        dir = path;

        // View the string
        text.setStringValue(path.shortName());

        // Popup menu
        popup.removeAllItems();
        Filename[] files = path.split();
        if(files.length == 0) {
            // Don't allow empty directory
            for(int i = 0; i != roots.length; i++)
                popup.addItem("//" + roots[i].longName(), COM_POPUP);
            index = 0;
            popup.selectItemAt(0);
            text.setStringValue(roots[0].shortName());
        }
        else {
            String head = files[0].shortName();
            boolean found = false;
            int count = 0;
            for(int i = 0; i != roots.length; i++) {
                Filename root = roots[i];
                popup.addItem("//" + roots[i].longName(), COM_POPUP);
                if(root.shortName().equals(head)) {
                    index = count;
                    for(int j = 1; j != files.length; j++)
                        popup.addItem("> " + files[j].longName(), COM_POPUP);
                    count += files.length - 1;
                    popup.selectItemAt(count);
                    found = true;
                }
                count++;
            }

            // Catch entries that don't have a listed root
            if(found == false) {
                index = count;
                for(int j = 0; j != files.length; j++)
                    popup.addItem("> " + files[j].longName(), COM_POPUP);
                count += files.length;
                popup.selectItemAt(count - 1);
            }
        }
    }

    /**
     * At creation time, provide the initial directory.
     */
    AddressView(Filename[] r, Pathname d, int x, int y, int w, int h)
    {
        super(x, y, w, h);
        roots = r;

        // Widgets
        popup = new Popup(0, 2, POPUP_WIDTH, TEXT_HEIGHT);
        text = new TextField(POPUP_WIDTH, 2, w - POPUP_WIDTH, TEXT_HEIGHT);

        // Targets
        text.setCommand(COM_CHDIR);
        text.setTarget(this);
        popup.setTarget(this);

        // Layout
        text.setJustification(Graphics.LEFT_JUSTIFIED);
        text.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
        text.setVertResizeInstruction(View.CENTER_VERT);

        popup.setVertResizeInstruction(View.CENTER_VERT);
        // popup.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);

        addSubview(popup);
        addSubview(text);

        // Set initial contents
        setPath(d);
    }

    /**
     * Set the target that receives the chandir commands.
     */
    public void setTarget(Target t)
    {
        target = t;
    }

    /**
     * Set the command that is sent to the target when the dir changes.
     */
    public void setCommand(String s)
    {
        command = s;
    }

    /**
     * Enable/disable this bar.
     */
    public void setEnabled(boolean flag)
    {
        popup.setEnabled(flag);
        text.setEditable(flag);
    }
}

/**
 * Navigation toolbar.
 */
public class Navigate
extends ContainerView
implements Target, Marshalable
{
    /*
     * Commands:
     *     COM_BACK: move back in the address list.
     *     COM_FORWARD: move forward in the address list.
     *     COM_UP: move up a directory.
     *     COM_HOME: go to home directory.
     *     COM_CHDIR: change the directory.
     *     COM_MKDIR: make a directory in the current directory.
     */
    private final static String COM_BACK = "back";
    private final static String COM_FORWARD = "forward";
    private final static String COM_UP = "up";
    private final static String COM_HOME = "home";
    private final static String COM_CHDIR = "chdir";
    private final static String COM_MKDIR = "mkdir";

    /*
     * Widgets.  There are some buttons and an address bar.
     */
    private GrayButton back_button;
    private GrayButton forward_button;
    private GrayButton up_button;
    private GrayButton home_button;
    private GrayButton mkdir_button;

    private static final int BUTTON_WIDTH = 44;
    private static final int BUTTON_HEIGHT = 30;
    // private static final int TEXT_WIDTH = 300;

    /** The address subview controls the popup and the text */
    protected AddressView addr;

    /** List of addresses we have visited */
    protected Vector addresses;
    
    /** This is the index into the address list */
    protected int index;

    /** This target receives the address changes */
    protected Target target;

    /** This command is sent to the target when the directory is changed */
    protected String chdir_command;

    /** This command is sent to the target when the mkdir button is pushed */
    protected String mkdir_command;

    /** This command is sent to the target when the home button os pushed */
    protected String home_command;

    /** Traverse the object */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(back_button);
        info.Marshal(forward_button);
        info.Marshal(up_button);
        info.Marshal(home_button);
        info.Marshal(mkdir_button);
        info.Marshal((View) addr);
        info.Marshal(addresses);
        info.Marshal(index);
        info.Marshal(target);
        info.Marshal(chdir_command);
        info.Marshal(mkdir_command);
        info.Marshal(home_command);
    }

    /**
     * Handle button events.
     */
    public void performCommand(String command, Object obj)
    {
        if(command.equals(COM_BACK))
            Back();
        else if(command.equals(COM_FORWARD))
            Forward();
        else if(command.equals(COM_UP))
            Up();
        else if(command.equals(COM_CHDIR))
            Chdir((Pathname) obj);
        else if(command.equals(COM_HOME))
            target.performCommand(home_command, obj);
        else if(command.equals(COM_MKDIR))
            target.performCommand(mkdir_command, obj);
    }

    /*
     * Upcalls.
     */
    protected void Chdir(Pathname path)
    {
        setPath(path);
        target.performCommand(chdir_command, path);
    }

    /*
     * Navigate in the address list.
     */
    public void Back()
    {
        if(index != 0)
            index--;
        SetIndex();
    }

    public void Forward()
    {
        if(index < addresses.count() - 1)
            index++;
        SetIndex();
    }

    public void Up()
    {
        Pathname path = (Pathname) addresses.elementAt(index);
        if(path.count() > 1) {
            path = path.parent();
            setPath(path);
        }
        target.performCommand(chdir_command, path);
    }
        
    protected void SetIndex()
    {
        // Change to the specified address
        Pathname path = (Pathname) addresses.elementAt(index);

        // Set the buttons
        back_button.setEnabled(index != 0);
        forward_button.setEnabled(index < addresses.count() - 1);

        // Send the command to the client
        target.performCommand(chdir_command, path);
    }

    /*
     * Generic button creator.
     */
    private GrayButton createButton(String command, int x, String map, String press, String gray)
    {
        GrayButton button = new GrayButton(map, gray, x, 0, BUTTON_WIDTH, BUTTON_HEIGHT);
        Bitmap bitmap = Bitmap.bitmapNamed(press);
        button.button.setAltImage(bitmap);
        button.button.setTransparent(true);
        button.button.setCommand(command);
        button.button.setTarget(this);
        addSubview(button.button);
        return button;
    }

    /**
     * Create the navigation window.
     * The roots are used to initialize the address.
     */
    Navigate(Filename[] roots, int x, int y, int w, int h)
    {
        super(x, y, w, h);

        /*
         * Create buttons.
         */
        back_button    = createButton(COM_BACK,                   0, "back.gif",    "back_press.gif",    "back_gray.gif");
        forward_button = createButton(COM_FORWARD,     BUTTON_WIDTH, "forward.gif", "forward_press.gif", "forward_gray.gif");
        up_button      = createButton(COM_UP,      2 * BUTTON_WIDTH, "up.gif",      "up_press.gif",      "up_gray.gif");
        home_button    = createButton(COM_HOME,    3 * BUTTON_WIDTH, "home.gif",    "home_press.gif",    "home_gray.gif");
        mkdir_button   = createButton(COM_MKDIR,   4 * BUTTON_WIDTH, "mkdir.gif",   "mkdir_press.gif",   "mkdir_gray.gif");

        /*
         * Addressing.
         */
        Pathname path = new Pathname(roots[0]);
        addr = new AddressView(roots, path, 5 * BUTTON_WIDTH,
                               0, w - 5 * BUTTON_WIDTH - 8, BUTTON_HEIGHT);
        addr.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
        addSubview(addr);
        addr.setTarget(this);
        addr.setCommand(COM_CHDIR);

        /*
         * Initial address list.
         */
        addresses = new Vector();
        index = 0;
        addresses.addElement(path);

        back_button.setEnabled(false);
        forward_button.setEnabled(false);
    }

    /**
     * Set the target for upcalls.
     * There is no corresponding setCommand().
     * The commands are given in this module.
     */
    public void setTarget(Target t)
    {
        target = t;
    }

    /**
     * Set the command for changing directories.
     */
    public void setChdirCommand(String s)
    {
        chdir_command = s;
    }

    /**
     * Set the command for the home directory.
     */
    public void setHomeCommand(String s)
    {
        home_command = s;
    }

    /**
     * Set the command for mkdir.
     */
    public void setMkdirCommand(String s)
    {
        mkdir_command = s;
    }

    /**
     * If the directory is read-only, the mkdir button is disabled.
     */
    public void setReadOnly(boolean flag)
    {
        mkdir_button.setEnabled(flag == false);
    }

    /**
     * Set the current directory to the given path.
     */
    public void setPath(Pathname p)
    {
        // Truncate address array
        index++;
        while(addresses.count() > index)
            addresses.removeElementAt(index);

        // Add the new path
        addresses.addElement(p);

        // Disable forward, enable back
        back_button.setEnabled(true);
        forward_button.setEnabled(false);

        // Set the address
        addr.setPath(p);
    }

    /**
     * Set the current path without pushing it on the stack.
     */
    public void resetPath(Pathname p)
    {
        // Save the address
        addresses.replaceElementAt(index, p);

        // Set the address
        addr.setPath(p);
    }

    /**
     * Enable/disable the navigation toolbar.
     */
    public void setEnabled(boolean flag)
    {
        back_button.setEnabled(flag);
        forward_button.setEnabled(flag);
        up_button.setEnabled(flag);
        home_button.setEnabled(flag);
        mkdir_button.setEnabled(flag);
        addr.setEnabled(flag);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:12  jyh
 * This is a simple term display in an applet.
 *
 */
