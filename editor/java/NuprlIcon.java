/**
 * This applet provides a menu bar for the Nuprl terms.
 * The DisplayDynamic is also associated with this applet.
 *
 * params:
 *    external: bool; editor should run in an external window
 */

import java.net.*;
import java.applet.*;
import java.util.Enumeration;

import netscape.application.*;
import netscape.util.*;

public class NuprlIcon
extends Application
implements WindowOwner, ApplicationObserver
{
    /*
     * Default size of the window
     */
    private static final int WIDTH = 100;
    private static final int HEIGHT = 77;

    /*
     * Parameter names
     */
    private static final String PARAM_external = "external";
    private static final String PARAM_display_base = "display_base";

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
    static DisplayDynamic display;

    /*
     * We control the gate.
     */
    GateSequence image;

    /************************************************************************
     * WINDOW OWNER                                                         *
     ************************************************************************/

    public void windowDidBecomeMain(Window w)
    {
    }

    public void windowDidHide(Window w)
    {
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
     * APPLET                                                               *
     ************************************************************************/

    /**
     * This is the function that is called when
     * this application is standalone.  Make a
     * frame, then add this applet to it.
     */
    public static void main(String[] args)
    {
        NuprlIcon app;

        // Create main window and size it
        app = new NuprlIcon();
        app.argv = args;
        app.init();

        // Show it
        app.run();
        System.exit(0);
    }

    /**
     * Constructor.
     */
    public NuprlIcon()
    {
        started = false;
        display = new DisplayDynamic();
        image = new GateSequence();
        image.start();
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

            // Add icon to main root view
            RootView main = mainRootView();
            ImageView view = new ImageView(image, 0, 0);
            main.addSubview(view);

            // Connect to other applets
            connect();

            // Show the window
            if(external)
                mainWindow.show();

            started = true;
        }
    }

    /**
     * Search for all applets.
     */
    protected void connect()
    {
        // Get applet
        Applet applet = AWTCompatibility.awtApplet();
        AppletContext context = applet.getAppletContext();
        Enumeration enum = context.getApplets();
        while(enum.hasMoreElements()) {
            Applet peer = (Applet) enum.nextElement();
            if(peer instanceof FoundationApplet) {
                Application peer_app = ((FoundationApplet) peer).application();
                if(peer_app instanceof NuprlTerm) {
                    NuprlTerm term_app = (NuprlTerm) peer_app;
                    // term_app.register(this);
                }
            }
        }
    }

    /**
     * Keep a register of who is working.
     */
    private Vector registrees = new Vector();

    /**
     * Check if a term is registered.
     */
    private boolean is_registered(NuprlTerm term)
    {
        int length = registrees.size();
        for(int i = 0; i != length; i++) {
            NuprlTerm term2 = (NuprlTerm) registrees.elementAt(i);
            if(term == term2)
                return true;
        }
        return false;
    }

    /**
     * Remove a term from the register.
     */
    private void rm_register(NuprlTerm term)
    {
        int length = registrees.size();
        for(int i = 0; i != length; i++) {
            NuprlTerm term2 = (NuprlTerm) registrees.elementAt(i);
            if(term == term2) {
                registrees.removeElementAt(i);
                return;
            }
        }
    }

    /**
     * Register and start the icon.
     */
    public void register(NuprlTerm term)
    {
        // Start icon if nobody is registered
        if(registrees.size() == 0)
            image.start();
        if(is_registered(term) == false)
            registrees.addElement(term);
    }

    /**
     * Unregister and stop icon.
     */
    public void unregister(NuprlTerm term)
    {
        rm_register(term);
        if(registrees.size() == 0)
            image.stop();
    }
}

