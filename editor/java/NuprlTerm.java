/**
 * This is a simple term display for Nuprl-Light terms.
 * It can be run as an applet, or a standalone application.
 *
 * params:
 *    external: bool; editor should run in an external window
 *    term: String: name of term to display
 *    file: String: name of file to get terms from
 */

import java.applet.*;
import java.net.*;
import java.io.*;
import java.util.Enumeration;
import netscape.application.*;
import netscape.util.*;

/*
 * Sort two applets based on their hash codes.
 */
class NuprlTermSort
extends QuickSort
{
    /* Comparison on hashCodes */
    public int compare(Object a, Object b)
    {
        int code1 = System.identityHashCode(a);
        int code2 = System.identityHashCode(b);
        return code1 - code2;
    }
}

/*
 * In an applet, we create a separate thread for waiting until the
 * term is parsed.
 */
class LoadThread
extends Thread
{
    /*
     * Save the Applet.
     */
    NuprlTerm applet;

    /*
     * Create a new thread.
     */
    LoadThread(NuprlTerm applet)
    {
        this.applet = applet;
    }

    /*
     * Get the term from the parser,
     * then display it.
     */
    public void run()
    {
        System.err.println("NuprlTerm.LoadThread: loading term " + applet.term_name);
        Term term = applet.parser.get(applet.term_name);
        if(term == null)
            term = new TermString("Term is not found: " + applet.term_name);
        TermView text = applet.text;
        text.truncate(0);
        text.appendTerm(term);
        applet.unregister();
    }
}

/*
 * The main applet class.
 */
public class NuprlTerm
extends ActiveApplication
implements WindowOwner, ApplicationObserver, Target
{
    /*
     * Default size of the window if it is external
     */
    private static final int WIDTH = 640;
    private static final int HEIGHT = 480;

    /*
     * Parameter names
     */
    private static final String PARAM_external = "external";
    private static final String PARAM_term = "term";
    private static final String PARAM_file = "file";

    /*
     * Commands.
     */
    private static final String MOUSE_ENTERED_COM = "mouse entered";
    private static final String MOUSE_EXITED_COM = "mouse exited";

    /*
     * Parsed aguments.
     */
    private boolean started;
    private boolean external;
    String term_name;
    String file_name;

    /**
     * Focus color for the border of this applet.
     */
    private Color focused_color;

    /**
     * Color when this term is not focused.
     */
    private Color unfocused_color;

    /**
     * This is the URL for an applet.
     */
    URL url;

    /**
     * This is a flag that is used to cummunicate the
     * controller Application for all the Applications
     * on this page.
     */
    NuprlTerm controller;

    /**
     * This is the term database for this particular file.
     * It is shared by all the applets on this page.
     */
    TermParser parser;

    /**
     * This is the display form database.
     * The base is looked up recursively, and it is shared by
     * all the applets on this page.
     */
    DisplayDynamic display;

    /**
     * Also keep a handle to the icon if it exists.
     */
    NuprlIcon icon;

    /**
     * Flag if the applet is idle.
     */
    private boolean completed = false;

    /*
     * Arguments are stored locally, because main will not be
     * called for applets.
     */
    private String[] argv;

    /**
     * Term window.
     */
    protected TermView text;

    /**
     * ScrollGroup surrounding text window.
     */
    protected ScrollGroup scroll;

    /**
     * This is the border around the applet.
     */
    protected LineBorder border;

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

        // Name of term to display
        file_name = GetParameter(PARAM_file, argv);
        term_name = GetParameter(PARAM_term, argv);
    }

    /************************************************************************
     * EVENT HANDLING                                                       *
     ************************************************************************/

    /**
     * Handle commands.
     */
    public void performCommand(String command, Object obj)
    {
        if(command.equals(MOUSE_ENTERED_COM))
            setMouseEntered();
        else if(command.equals(MOUSE_EXITED_COM))
            setMouseExited();
    }

    /**
     * Mouse entered.
     */
    public void setMouseEntered()
    {
        setBorderColor(focused_color);
    }

    /**
     * Mouse left.
     */
    public void setMouseExited()
    {
        setBorderColor(unfocused_color);
    }

    /*
     * Set the border color, and referesh it.
     */
    private void setBorderColor(Color color)
    {
        if(border != null && scroll != null && color != null) {
            border.setColor(color);
            scroll.setDirty(true);
        }
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
        NuprlTerm app;

        // Create main window and size it
        app = new NuprlTerm();
        app.argv = args;
        app.init();

        // Show it
        app.run();
        System.exit(0);
    }

    /**
     * Constructor.
     */
    public NuprlTerm()
    {
        started = false;
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

            // Create a new external window for the root, if desired
            if(external) {
                mainWindow = new ExternalWindow();
                setMainRootView(mainWindow.rootView());
                Size size = mainWindow.windowSizeForContentSize(WIDTH, HEIGHT);
                mainWindow.sizeTo(size.width, size.height);
                mainWindow.setOwner(this);
            }

            /* Main window for this applet */
            RootView main = mainRootView();
            main.setAutoResizeSubviews(true);

            /* Border around this applet */
            focused_color = Color.white;
            unfocused_color = Color.white;
            border = new LineBorder(unfocused_color);

            /* Put scrollbars around the text window */
            scroll = new SmallScrollGroup(0, 0, main.width(), main.height());
            scroll.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
            scroll.setVertResizeInstruction(View.HEIGHT_CAN_CHANGE);
            scroll.setHasVertScrollBar(false);
            scroll.setHasHorizScrollBar(false);
            scroll.setBackgroundColor(Color.white);
            scroll.setBorder(border);

            /* Each applet has its own FontBase */
            FontBase base = new FontBase();

            /* Get the info shared between all the applets */
            if(isApplet())
                startApplet();
            else
                startApplication();

            /* Create the text window */
            text = new TermView(display, base, 0, 0, main.width(), main.height());
            text.setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
            text.setVertResizeInstruction(View.HEIGHT_CAN_CHANGE);
            text.setTarget(this);
            text.setEnteredCommand(MOUSE_ENTERED_COM);
            text.setExitedCommand(MOUSE_EXITED_COM);

            /* Applets load the argument term from a thread */
            if(isApplet()) {
                Thread thread = new LoadThread(this);
                thread.start();
            }

            /* Add the text window */
            scroll.setContentView(text);
            main.addSubview(scroll);

            /* Show the window */
            if(external)
                mainWindow.show();

            started = true;
        }
    }

    /**
     * Load the terms in the file.
     * For an application, the file is passed as a parameter.
     *
     * For applets, lock a common static variable to ensure initialization
     * is performed in a critical section.
     */
    protected void startApplet()
    {
        // Applets and applications are different
        electController();
        display = controller.display;
        parser = controller.parser;
        parser.parseURL(url);
    }

    /**
     * Application doesn't share anything.
     */
    protected void startApplication()
    {
        // For an application, load from scratch
        try {
            url = new URL("file:.");
        }
        catch(MalformedURLException e) {
            // Never happens
        }
        display = new DisplayDynamic(url);
        parser = new TermParser(display);
        if(file_name != null) {
            // Load the term named in the argument
            Term term = null;
            try {
                parser.parseFile(file_name);
                term = parser.get(term_name);
            }
            catch(FileNotFoundException e) {
                term = new TermString("File is not found: " + file_name);
            }
            catch(SecurityException e) {
                term = new TermString("Security exception");
            }
            if(term == null)
                term = new TermString("term is not found: " + term_name);
            text.appendTerm(term);
        }
    }

    /**
     * Initialize the shared info for the applets.
     * This is really an election protocol for all the applets
     * on this page.  The asumptions are that:
     *      AppletContext.getApplets()
     *      may return a different list for every applet, but at least
     *      one applet will be in all the lists.
     *
     * The algorithm is performed as follows.  Each applet
     *    1. makes a list of applets that it knows about,
     *    2. it sorts them by their names,
     *    3. it tries to lock all the applets in descending order,
     *    4. if it locks them all,
     *       a. if none of them have elected a controller,
     *          then it elects itself,
     *       b. if any of them have elected a controller,
     *          then choose that controller.
     *
     * An invariant is that all controllers that are elected
     * are the same.
     *
     * There is no deadlock because the locks are strictly ordered.
     */
    protected void electController()
    {
        // Get applet
        Applet applet = AWTCompatibility.awtApplet();
        AppletContext context = applet.getAppletContext();
        url = applet.getDocumentBase();

        // Want to make a lock on all the classes.
        // Assume their hashCodes are all distinct.
        Vector control_vec = new Vector();
        Enumeration enum = context.getApplets();
        while(enum.hasMoreElements()) {
            Applet peer = (Applet) enum.nextElement();
            if(peer instanceof FoundationApplet) {
                Application peer_app = ((FoundationApplet) peer).application();
                if(peer_app instanceof NuprlTerm) {
                    NuprlTerm term_app = (NuprlTerm) peer_app;
                    if(url.equals(term_app.url))
                        control_vec.addElement(term_app);
                }
                else if(peer_app instanceof NuprlIcon)
                    register((NuprlIcon) peer_app);
            }
        }

        // Sort them based on their indices
        Object[] controllers = control_vec.elementArray();
        Sort sort = new NuprlTermSort();
        sort.sort(controllers);

        // Now synchronize on them all
        electController(controllers, 0);
    }

    /*
     * Lock all the controllers in order.
     * Guaranteed no deadlock if we do in exact order.
     */
    private void electController(Object[] controllers, int index)
    {
        /*
         * Check if all controllers have been locked.
         */
        if(index == controllers.length) {
            // We are elected, so create a display base and a parser
            controller = this;
            if(display == null)
                display = new DisplayDynamic(url);
            parser = new TermParser(display);
        }
        else {
            NuprlTerm peer = (NuprlTerm) controllers[index];
            synchronized (peer) {
                if(peer.controller != null)
                    controller = peer.controller;
                else
                    electController(controllers, index + 1);
            }
        }
    }

    /**
     * Register this applet with the icon.
     */
    public synchronized void register(NuprlIcon icon)
    {
        this.icon = icon;
        this.completed = false;
        icon.register(this);
    }

    /**
     * Unregister this applet with the icon.
     */
    public synchronized void unregister()
    {
        if(icon != null) {
            icon.unregister(this);
            completed = true;
        }
    }

    /**
     * Ask for the width.
     * Adjust for the scrollbars.
     */
    public int desiredWidth()
    {
        if(text == null || display.finished == false)
            return 0;
        int width = text.desiredWidth();
        if(width > 0) {
            if(scroll.vertScrollBarIsVisible())
                width += scroll.vertScrollBar().width();
        }
        return width;
    }

    /**
     * Ask for height.
     */
    public int desiredHeight()
    {
        return text == null || display.finished == false ? 0 : text.desiredHeight() + 2;
    }
}

