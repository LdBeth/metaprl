/*
 * This is the Nuprl applet.
 * It can be run as an applet, or a standalone application.
 */

package edu.cornell.cs.jyh.nuprl;

import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import java.applet.*;
import java.util.*;
import java.net.*;
import java.io.*;

import javax.swing.*;

public class Nuprl
extends JApplet
implements WindowListener, ComponentListener, NuprlContext
{
    /**
     * Default size of the application.
     */
    static final int WIDTH  = 900;
    static final int HEIGHT = 750;

    /*
     * Parameter names
     */
    static final String PARAM_imagebase = "imagebase";
    static final String PARAM_host      = "host";
    static final String PARAM_user      = "user";
    static final String PARAM_port      = "port";

    static final String DEFAULT_HOST    = "localhost";
    static final int    DEFAULT_PORT    = 8772;

    static final String[][] info = {
        { PARAM_imagebase,  "String",       "URL of location where images are contained" },
        { PARAM_host,       "String",       "MetaPRL host" },
        { PARAM_user,       "String",       "MetaPRL user" },
        { PARAM_port,       "int",          "MetaPRL port" }
    };

    /*
     * Commands.
     */
    private static final String UPDATE_COM = "update";

    /*
     * We keep a common argv list.
     */
    private String[] argv;

    /**
     * Frame will be set if this is not standalone.
     */
    protected JFrame frame;

    /**
     * The applet contains a desktop.
     */
    protected JDesktopPane desktop;

    /**
     * Flag if the applet has started.
     */
    protected boolean initialized = false;
    protected boolean started = false;

    /**
     * Directory where images are stored.
     */
    protected URL imagebase;

    /*
     * Authorization contains connection info.
     */
    private NuprlAuthorization auth;

    /**
     * Manager window manages all the subwindows.
     */
    private NuprlManager manager;

    /************************************************************************
     * ComponentListener                                                    *
     ************************************************************************/

    /**
     * Applet is hidden.
     */
    public void componentHidden(ComponentEvent event)
    {
        stop();
    }

    /**
     * Applet is shown again.
     */
    public void componentShown(ComponentEvent event)
    {
        start();
    }

    /**
     * Component was moved.
     */
    public void componentMoved(ComponentEvent event)
    {
    }

    /**
     * Component was resized.
     */
    public void componentResized(ComponentEvent event)
    {
    }

    /************************************************************************
     * WindowListener                                                       *
     ************************************************************************/

    /**
     * Need to implement this as a WindowAdapter.
     */
    public void windowActivated(WindowEvent event)
    {
        if(started == false) {
            init();
            start();
        }
   }

    /**
     * Need to implement this as a WindowAdapter.
     */
    public void windowClosed(WindowEvent event)
    {
    }

    /**
     * Catch destroy events and force the quit.
     * Otherwise we don't quit because there
     * are threads that keep running.
     */
    public void windowClosing(WindowEvent event)
    {
        System.exit(0);
    }

    /**
     * Need to implement this as a WindowAdapter.
     */
    public void windowDeactivated(WindowEvent event)
    {
    }

    /**
     * Need to implement this as a WindowAdapter.
     */
    public void windowDeiconified(WindowEvent event)
    {
    }

    /**
     * Need to implement this as a WindowAdapter.
     */
    public void windowIconified(WindowEvent event)
    {
    }

    /**
     * Need to implement this as a WindowAdapter.
     */
    public void windowOpened(WindowEvent event)
    {
    }

    /************************************************************************
     * NUPRL CONTEXT                                                        *
     ************************************************************************/

    /**
     * The desktop.
     */
    public JDesktopPane getDesktop()
    {
        return desktop;
    }

    /**
     * The mode: this may be an applet, or a shell window, or an rsh window.
     */
    public int getMode()
    {
        if(frame == null)
            return NuprlContext.APPLET_MODE;
        else
            return NuprlContext.TELNET_MODE;
    }

    /**
     * Get an image.
     * This function is different for applets and applications.
     */
    public Image getImage(String name)
    {
        Image image = null;

        if(frame == null) {
            System.err.println("Loading image: " + imagebase.toString() + "/" + name);
            image = getImage(imagebase, name);
        }
        else
            image = frame.getToolkit().getImage(name);
        return image;
    }

    /**
     * Open a file.
     */
    public InputStream getFile(String name)
    {
        InputStream stream;

        if(frame == null) {
            // Applet file is relative to document base
            URL url = getDocumentBase();
            try {
                url = new URL(url, name);
            }
            catch(MalformedURLException e) {
                return null;
            }
            try {
                stream = url.openStream();
            }
            catch(IOException e) {
                return null;
            }
        }
        else {
            File file = new File(name);
            try {
                stream = new FileInputStream(file);
            }
            catch(FileNotFoundException e) {
                return null;
            }
        }
        return stream;
    }

    /************************************************************************
     * MENUS                                                                *
     ************************************************************************/

    /*
     * Send an undo event.
     */
    protected class QuitAction
        extends AbstractAction
    {
        public QuitAction() {
            super("Quit");
        }
          
        public void actionPerformed(ActionEvent e) {
            stop();
        }
    }    

    /*
     * The edit menu allows undo/redo and copy/paste.
     */
    protected JMenu createFileMenu()
    {
        JMenu menu = new JMenu("File");

        menu.add(new QuitAction());

        return menu;
    }

    /**
     * Main menu.
     */
    protected JMenuBar createMenuBar()
    {
        // Menu
        JMenu fileMenu = createFileMenu();
        JMenuBar mb = new JMenuBar();
        mb.add(fileMenu);
        return mb;
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
    String getParameter(String name, String[] argv)
    {
        String value = null;

        // Try to get the command line
        if(argv != null) {
            int i;
            String arg = name + "=";
            int length = arg.length();

            for(i = 0; i < argv.length; i++) {
                if(argv[i].length() >= length) {
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
        }

        // Try getParameter
        if(value == null && frame == null)
            value = super.getParameter(name);

        return value;
    }

    /*
     * Get the list of bitmap names
     */
    void getParameters(String[] argv)
    {
        // Authorization
        auth = new NuprlAuthorization();
        auth.host = getParameter(PARAM_host, argv);
        auth.user = getParameter(PARAM_user, argv);

        if(frame == null)
            auth.port = DEFAULT_PORT;
        else
            auth.port = 23;
        String port = getParameter(PARAM_port, argv);
        if(port != null) {
            try {
                auth.port = Integer.parseInt(port);
            }
            catch (NumberFormatException e) {
                // Ignore
            }
        }

        // Location of images
        if(frame == null) {
            String base = getParameter(PARAM_imagebase, argv);
            if(base == null) {
                System.err.println("No image base found, using document base");
                imagebase = getDocumentBase();
            }
            else {
                try {
                    System.err.println("Using image base: " + base);
                    imagebase = new URL(base);
                }
                catch(MalformedURLException e) {
                    System.err.println("URL is malformed: " + base);
                    imagebase = getDocumentBase();
                }
            }
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
        // Frame
        JFrame frame = new JFrame("MetaPRL");
        frame.setSize(WIDTH, HEIGHT);

        // Applet construction
        Nuprl applet = new Nuprl();
        applet.argv = args;
        applet.frame = frame;

        // Add to the frame
        frame.addWindowListener(applet);

        // Add the menu
        frame.setJMenuBar(applet.createMenuBar());

        // Start the applet
        applet.init();

        // Now show it
        frame.pack();
        frame.setVisible(true);

        // Start it
        applet.start();
    }

    /**
     * Constructor.
     */
    public Nuprl()
    {
        addComponentListener(this);
    }

    /**
     * String for the "about box."
     */
    public String getAppletInfo()
    {
        return "Name: Nuprl applet\r\n" +
            "Author: Jason Hickey\r\n";
    }

    /**
     * These are the parameters that we care about.
     */
    public String[][] getParameterInfo()
    {
        return info;		
    }

    /**
     * This is the function that is called when this is
     * an applet.  In this case, don't create a frame
     * since we already have one.
     *
     * Be careful about browsers that call init more than once.
     */
    public synchronized void init()
    {
        if(initialized == false) {
            // Set all options from the parameters
            getParameters(argv);

            // Window is managed by a desktop
            desktop = new JDesktopPane();
            desktop.setPreferredSize(new Dimension(WIDTH, HEIGHT));
            if(frame == null) {
                setJMenuBar(createMenuBar());
                getContentPane().add(desktop, BorderLayout.CENTER);
            }
            else
                frame.getContentPane().add(desktop, BorderLayout.CENTER);

            // Create window manager
            manager = new NuprlManager(this, auth);

            // State that we are initialized
            initialized = true;
        }
    }

    /**
     * Destructor.
     */
    public void destroy()
    {
    }

    /**
     * Applet is in view.
     */
    public void start()
    {
        if(started == false) {
            manager.start();
            started = true;
        }
    }
	
    /**
     * Applet is hidden.
     */
    public void stop()
    {
    }
}
