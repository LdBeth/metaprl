/*
 * This is an internal window with a text area.
 */

import netscape.application.*;
import netscape.util.*;

import HTMLTerm;

public class Editor
extends IconWindow
implements Marshalable, Target, TextViewOwner
{
    /** Map of mode names */
    protected static final String[] str_roots = {
        "world"
    };

    /** Map of mode Filename's.  Corresponds to str_roots. */
    protected static Filename[] roots = null;
    
    /*
     * We have a navigation toolbar to move around
     * and manage "forward" and "back".
     */
    protected Navigate navigate;

    /*
     * We keep a TextView to show off the HTML.
     */
    private TextViewBuffer text;

    /*
     * This is the context that is used in all windows.
     */
    private Context context;

    /*
     * Menu commands.
     */
    private static final String NEW_WINDOW_COM = "NewWindow";
    private static final String SAVE_FILE_COM = "SaveFile";
    private static final String OPEN_FILE_COM = "OpenFile";
    private static final String CLOSE_FILE_COM = "CloseFile";
    private static final String REVERT_FILE_COM = "RevertFile";
    private static final String EXIT_COM = "ExitApplication";

    private static final String SELECT_ALL_COM = "SelectAll";
    private static final String COPY_COM = "Copy";
    private static final String CUT_COM = "Cut";
    private static final String PASTE_COM = "Paste";

    private static final String CHDIR_COM = "Chdir";
    private static final String HOME_COM = "Home";
    private static final String MKDIR_COM = "Mkdir";

    /*
     * Border around scrollbars.
     */
    private static int BORDER_WIDTH = 3;

    /**
     * Contents of menu bar.
     */
    protected Menu menu;

    /**
     * File that we are saving to, if there is one.
     */
    protected String filename;

    /**
     * Traverse the object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal((View) text);
    }

    /************************************************************************
     * TEXT VIEW OWNER                                                      *
     ************************************************************************/

    /**
     * Do nothing when attributes change on some text.
     */
    public void attributesDidChange(TextView view, Range r)
    {
    }

    /**
     * Do nothing when preparing for attributes to change.
     */
    public void attributesWillChange(TextView view, Range range)
    {
    }

    /**
     * Follow a link when it is selected.
     */
    public void linkWasSelected(TextView view, Range range, String url)
    {
        System.err.println("Hello world");
        view.importHTMLFromURLString(url);
    }

    /**
     * Do nothing when selection changes.
     */
    public void selectionDidChange(TextView view)
    {
    }

    /**
     * Do nothing when text is changed.
     */
    public void textDidChange(TextView view, Range range)
    {
        System.err.println("Text changed");
    }

    /**
     * Do nothing when editing starts.
     */
    public void textEditingDidBegin(TextView view)
    {
    }

    /**
     * Do nothing when editing ends.
     */
    public void textEditingDidEnd(TextView view)
    {
    }

    /**
     * Do nothing when text is going to change.
     */
    public void textWillChange(TextView view, Range range)
    {
    }

    /************************************************************************
     * MENU                                                                 *
     ************************************************************************/

    /**
     * Handle events.
     */
    public void performCommand(String command, Object obj)
    {
        if(command.equals(NEW_WINDOW_COM))
            NewWindow();
        else if(command.equals(SAVE_FILE_COM))
            SaveFile();
        else if(command.equals(OPEN_FILE_COM))
            OpenFile();
        else if(command.equals(CLOSE_FILE_COM))
            CloseFile();
        else if(command.equals(REVERT_FILE_COM))
            Revert();
        else if(command.equals(EXIT_COM))
            Exit();
        else if(command.equals(SELECT_ALL_COM))
            SelectAll();
        else if(command.equals(COPY_COM))
            CopySelection();
        else if(command.equals(CUT_COM))
            CutSelection();
        else if(command.equals(PASTE_COM))
            PasteSelection();
        else if(command.equals(CHDIR_COM))
            Chdir((Pathname) obj);
        else if(command.equals(HOME_COM))
            Home();
        else if(command.equals(MKDIR_COM))
            Mkdir();
        else
            super.performCommand(command, obj);
    }

    /**
     * Make a new window.
     */
    public void NewWindow()
    {
        new Editor(x() + 16, y() + 16, width(), height(), ix(), iy());
    }

    /**
     * Save the current file.
     */
    public void SaveFile()
    {
        if(filename == null)
            SaveAsFile();
    }

    /**
     * When we save to a new file,
     * we need to get the new filename.
     */
    public void SaveAsFile()
    {
        context.chooser.showModally();
    }

    /**
     * Open a new file.
     */
    public void OpenFile()
    {
    }

    /**
     * Close the current file.
     */
    public void CloseFile()
    {
    }

    /**
     * Revert to the saved file.
     */
    public void Revert()
    {
        context.alert.showAlert("Are you sure you want to revert to saved?");
    }

    /**
     * Exit program.
     */
    public void Exit()
    {
        context.alert.showAlert("Are you sure you want to exit?");
    }

    /**
     * Select everything.
     */
    public void SelectAll()
    {
        text.selectRange(new Range(0, text.length()));
    }

    /**
     * Copy.
     */
    public void CopySelection()
    {
        text.copy();
    }

    /**
     * Cut.
     */
    public void CutSelection()
    {
        text.cut();
    }

    /**
     * Paste.
     */
    public void PasteSelection()
    {
        text.paste();
    }

    /**
     * Change to a new directory.
     */
    public void Chdir(Pathname path)
    {
    }

    /**
     * Change back to the home directory.
     */
    public void Home()
    {
    }

    /**
     * Make a new directory in the current one.
     */
    public void Mkdir()
    {
    }

    /************************************************************************
     * IMPLEMENTATION                                                       *
     ************************************************************************/

    /*
     * Create the window with the specified authorization.
     */
    Editor(int x, int y, int w, int h, int ix, int iy)
    {
        super(Bitmap.bitmapNamed("key.gif"), x, y, w, h, ix, iy);
        setTitle("Nuprl Window");
        setResizable(true);
        setCloseable(true);
        setAutoResizeSubviews(true);

        // Try to adjust size in opposite direction
        Size size = contentSize();

        /*
         * Menu bar.
         */
        menu = new Menu();
        Menu file_menu = menu.addItemWithSubmenu("File").submenu();
        file_menu.addItem("New window", NEW_WINDOW_COM, this);
        file_menu.addItem("Open file", OPEN_FILE_COM, this);
        file_menu.addItem("Close", CLOSE_FILE_COM, this);
        file_menu.addItem("Revert", REVERT_FILE_COM, this);
        file_menu.addSeparator();
        file_menu.addItem("Exit", EXIT_COM, this);
        Menu edit_menu = menu.addItemWithSubmenu("Edit").submenu();
        edit_menu.addItem("Select All", SELECT_ALL_COM, this);
        edit_menu.addItem("Cut", CUT_COM, this);
        edit_menu.addItem("Copy", COPY_COM, this);
        edit_menu.addItem("Paste", PASTE_COM, this);
        MenuView menu_view = new MenuView(0, 0, size.width, 20, menu);
        // menu_view.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
        // menu_view.setVertResizeInstruction(View.HEIGHT_CAN_CHANGE);
        addSubview(menu_view);

        /*
         * Navigation toolbar.
         */
        if(roots == null) {
            roots = new Filename[str_roots.length];
            for(int i = 0; i != str_roots.length; i++)
                roots[i] = new Filename(str_roots[i], str_roots[i]);
        }
        navigate = new Navigate(roots, 0, 20, size.width, 32);
        navigate.setTarget(this);
        navigate.setChdirCommand(CHDIR_COM);
        navigate.setHomeCommand(HOME_COM);
        navigate.setMkdirCommand(MKDIR_COM);
        navigate.setReadOnly(true);
        addSubview(navigate);

        /* Put scrollbars around the text window */
        ScrollGroup scroll = new ScrollGroup(0, 52, size.width, size.height - 52);
        scroll.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
        scroll.setVertResizeInstruction(View.HEIGHT_CAN_CHANGE);
        scroll.setVertScrollBarDisplay(ScrollGroup.AS_NEEDED_DISPLAY);
        scroll.setHasHorizScrollBar(false);

        /* Create the text window */
        text = new TextView(0, 0, size.width, size.height - 20);
        text.setOwner(this);
        text.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
        text.setVertResizeInstruction(View.HEIGHT_CAN_CHANGE);
        // text.addDefaultAttribute(text.PRESSED_LINK_COLOR_KEY, Color.red);
        // text.addDefaultAttribute(text.LINK_COLOR_KEY, Color.blue);

        /* Add HTML term parsing */
        HTMLParsingRules rules = text.htmlParsingRules();
        Hashtable hash = new Hashtable();
        Vector empty = new Vector();
        Vector markers = new Vector();
        markers.addElement("TERM");
        hash.put(rules.REPRESENTATION_KEY, "HTMLTerm");
        hash.put(rules.BEGIN_TERMINATION_MARKERS_KEY, empty);
        hash.put(rules.END_TERMINATION_MARKERS_KEY, markers);
        hash.put(rules.IS_CONTAINER_KEY, "true");
        hash.put(rules.SHOULD_RETAIN_FORMATTING_KEY, "false");
        rules.setRulesForMarker(hash, "TERM");
        text.setHTMLParsingRules(rules);

        /* Now load the initial file */
        text.importHTMLFromURLString("file:D:\\jyh\\spool\\test.html");
        text.setEditable(false);

        /* Add the text window */
        scroll.setContentView(text);
        addSubview(scroll);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:54  jyh
 * This is a simple term display in an applet.
 *
 */
