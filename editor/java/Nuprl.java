/**
 * This applet provides a menu bar for the Nuprl terms.
 * The DisplayDynamic is also associated with this applet.
 *
 * params:
 *    external: bool; editor should run in an external window
 */

import java.net.*;
import java.applet.*;

import netscape.application.*;

public class Nuprl
extends Application
implements Target, WindowOwner, ApplicationObserver
{
    /*
     * Default size of the window
     */
    private static final int WIDTH = 640;
    private static final int HEIGHT = 32;

    /*
     * Parameter names
     */
    private static final String PARAM_external = "external";
    private static final String PARAM_display_base = "display_base";

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
     * Flags.
     */
    private boolean started;
    private boolean external;
    private String display_base;
    
    /*
     * Arguments are stored locally, because main will not be
     * called for applets.
     */
    private String[] argv;

    /*
     * We keep a common display base.
     */
    DisplayDynamic display;

    /************************************************************************
     * WINDOW OWNER                                                         *
     ************************************************************************/

    public void windowDidBecomeMain(Window w)
    {
    }

    public void windowDidHide(Window w)
    {
        applicationDidStop(this);
    }

    public void windowDidResignMain(Window w)
    {
    }

    public void windowDidShow(Window w)
    {
    }

    public boolean windowWillHide(Window w)
    {
        return true;
    }

    public boolean windowWillShow(Window w)
    {
        return true;
    }

    public void windowWillSizeBy(Window w, Size s)
    {
    }

    /************************************************************************
     * APPLICATION OBSERVER                                                 *
     ************************************************************************/

    public void applicationDidStart(Application a)
    {
    }

    public void applicationDidStop(Application a)
    {
        stopRunning();
    }

    public void focusDidChange(Application a, View v)
    {
    }

    public void applicationDidPause(Application a)
    {
        applicationDidStop(a);
    }

    public void applicationDidResume(Application a)
    {
    }

    public void currentDocumentDidChange(Application a, Window w)
    {
    }

    /************************************************************************
     * PARAMETERS                                                           *
     ************************************************************************/

    /**
     * Get the parameters from the arguments.
     * The command line first,
     * then getParameter,
     * then getProperty
     */
    String GetParameter(String name, String[] argv)
    {
        String value = null;

        // Try to get the command line
        if(argv != null) {
            int i;
            String arg = name + "=";
            int length = arg.length();

            try {
                for(i = 0; i < argv.length; i++) {
                    String param = argv[i].substring(0, length);

                    if(arg.equalsIgnoreCase(param)) {
                        // Found matching parameter on command line, so extract its value.
                        // If in double quotes, remove the quotes.
                        value = argv[i].substring(length);
                        if(value.startsWith("\"")) {
                            value = value.substring(1);
                            if (value.endsWith("\""))
                                value = value.substring(0, value.length() - 1);
                        }
                        break;
                    }
                }
            }
            catch(Exception e)	{
                // Don't care about any exceptions
            }
        }

        // Try getParameter
        if(value == null && isApplet())
            value = parameterNamed(name);

        return value;
    }

    /*
     * Initialize all the parameters.
     */
    private void GetParameters(String[] argv)
    {
        // Standard parameters
        String ext = GetParameter(PARAM_external, argv);
        external = isApplet() == false || ext != null && ext.equalsIgnoreCase("true");
        display_base = GetParameter(PARAM_display_base, argv);
    }

    /**
     * Get the display database URL from the arguments.
     */
    protected URL getDisplayBase()
    {
        URL base;

        // Get URL from applet
        if(isApplet()) {
            Applet applet = AWTCompatibility.awtApplet();
            AppletContext context = applet.getAppletContext();
            URL url = applet.getDocumentBase();

            if(display_base != null) {
                try {
                    base = new URL(url, display_base);
                }
                catch(MalformedURLException e) {
                    base = url;
                }
            }
            else
                base = url;
        }
        else {
            try {
                if(display_base != null) {
                    try {
                        base = new URL(display_base);
                    }
                    catch(MalformedURLException e) {
                        base = new URL("file:.");
                    }
                }
                else
                    base = new URL("file:.");
            }
            catch(MalformedURLException e) {
                // This can't happen
                base = null;
            }
        }

        return base;
    }

    /************************************************************************
     * MENU COMMANDS                                                        *
     ************************************************************************/

    /**
     * Perform the menu command.
     */
    public void performCommand(String command, Object data)
    {
    }

    /************************************************************************
     * APPLET                                                               *
     ************************************************************************/

    /**
     * This is the function that is called when
     * this application is standalone.  Make a
     * frame, then add this applet to it.
     */
    public static void main(String[] args)
    {
        Nuprl app;

        // Create main window and size it
        app = new Nuprl();
        app.argv = args;
        app.init();

        // Show it
        app.run();
        System.exit(0);
    }

    /**
     * Constructor.
     */
    public Nuprl()
    {
        started = false;
        display = new DisplayDynamic();
        addObserver(this);
    }

    /**
     * This is the function that is called to start everything.
     * Get the parameters and add subwindows.
     *
     * Be careful to initialize only once.
     */
    public void init()
    {
        ExternalWindow mainWindow = null;

        // Start application
        super.init();

        // Watch out for multiple inits
        if(started == false) {
            // Read parameters
            GetParameters(argv);

            // Adjust display base
            display.setDisplayBase(getDisplayBase());

            // Create a new external window for the root, if desired
            if(external) {
                mainWindow = new ExternalWindow();
                setMainRootView(mainWindow.rootView());
                Size size = mainWindow.windowSizeForContentSize(WIDTH, HEIGHT);
                mainWindow.sizeTo(size.width, size.height);
                mainWindow.setOwner(this);
            }

            // Change background color
            RootView main = mainRootView();

            /*
             * Menu bar.
             */
            Menu menu = new Menu();
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
            MenuView menu_view = new MenuView(2, 2, main.width() - 4, 18, menu);
            // menu_view.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
            // menu_view.setVertResizeInstruction(View.HEIGHT_CAN_CHANGE);
            ContainerView container = new ContainerView(0, 0, main.width(), main.height());
            container.addSubview(menu_view);
            main.addSubview(container);

            // Show the window
            if(external)
                mainWindow.show();

            started = true;
        }
    }
}

