/**
 * This is a simple term display for Nuprl-Light files.
 * It can be run as an applet, or a standalone application.
 *
 * params:
 *    edternal: bool; editor should run in an external window
 */

import netscape.application.*;

public class Nuprl
extends Application
implements WindowOwner, ApplicationObserver
{
    /*
     * Default size of the window
     */
    private static final int WIDTH = 640;
    private static final int HEIGHT = 480;
    private static final int SPACE = 30;

    /*
     * Parameter names
     */
    private static final String PARAM_external = "external";

    /*
     * Flags.
     */
    private boolean started;
    private boolean external;
    
    /*
     * Arguments are stored locally, because main will not be
     * called for applets.
     */
    private String[] argv;

    /*
     * Windows.
     */
    private Context context;
    private Editor editor;

    /*
     * Background image
     */
    protected Bitmap background;

    /** Traverse the object */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(started);
        info.Marshal(external);
        info.Marshal(argv);
        info.Marshal(background);
    }

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
        System.err.println("Nuprl.applicationDidStop");
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

            // Change background color
            RootView main = mainRootView();
            main.setImage(Bitmap.bitmapNamed("background.gif"));
            main.setImageDisplayStyle(Image.TILED);

            // Build the context
            Context context = new Context();
            context.status = new Status(0, HEIGHT - 48, WIDTH, 64, 10, HEIGHT - 74);
            context.alert = context.status;

            // Start the window
            editor = new Editor(16, 16, WIDTH - 32, HEIGHT - 32, 8, 8);
            editor.Show();

            // Show the window
            if(external)
                mainWindow.show();

            started = true;
        }
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:14  jyh
 * This is a simple term display in an applet.
 *
 */
