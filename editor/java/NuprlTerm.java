/*
 * This is an internal window for editing.
 */
package edu.cornell.cs.jyh.nuprl;

import java.awt.*;
import java.io.*;
import java.net.*;
import java.util.*;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.text.html.*;
import javax.swing.event.*;

public class NuprlTerm
extends JScrollPane
{
    /**
     * The text pane is needed for loading new text.
     */
    protected JTextPane textPane;

    /**
     * Document is formatted in HTML.
     */
    protected HTMLDocument document;

    /**
     * This is our endpoint on the bus.
     */
    protected NuprlBusEndpoint endpoint;

    /**
     * Save the hostname for debugging.
     */
    protected String debug_host;

    /**
     * This is the client that want to receive our events.
     */
    protected NuprlBusPort client;

    /**
     * Buffer for collecting input data.
     */
    protected NuprlDataToken buffer;

    /**
     * Directory for serving MetaPRL cd commands.
     */
    protected String metaprl_base = "";

    /**
     * Frame for setting the title.
     */
    protected NuprlFrame frame;

    /**
     * This is what we display while text is being formatted.
     */
    protected Cursor default_cursor;
    protected Cursor wait_cursor;
    protected Random random;

    /*
     * Default style sheet.
     * HACK!  We need this because the default Sun implementation of CSS isn't ready yet.
     * We have to correct "tt" to be "monospaced" (the implementation doesn't recognize "monospace"),
     * and we use the italic font for a symbol font (the implementation doesn't handle the
     * <font> tag), and the CSS parser implementation doesn't handle "+i" options for the font
     * size.  Also, it is better to use ".tt" and ".i", but the implementation doesn't handle it.
     * Ok, so we load this hack for every window...
     *
     * Also hack the table style.  We really want invisible table cells,
     * but the Sub code doesn't seem to want to grant them.
     */
    private final static String table_style =
    "{border-width:0;"
    + "border-style:line;"
    + "margin:0;"
    + "margin-left:-2;"
    + "margin-right:-2;"
    + "margin-top:-2;"
    + "margin-bottom:-2;"
    + "padding-left:0;"
    + "padding-right:0;"
    + "padding-top:0;"
    + "padding-bottom:0;"
    + "border-left-width:0;"
    + "border-right-width:0;"
    + "border-top-width:0;"
    + "border-bottom-width:0;"
    + "font-family:LucidaSansUnicode};";

    private final static String text_style_body =
       "{font-family:LucidaSans;font-style:normal;font-size:18};";

    private final static String text_style_bold =
       "{font-family:LucidaSans;font-style:normal;font-size:18};";

    private final static String text_style_italic =
       "{font-family:LucidaSansUnicode;font-style:normal;font-size:18};";

    private final static String hacked_style = "tt {font-family:monospaced};"
    + "body " + text_style_body
    + "b " + text_style_bold
    + "i" + text_style_italic
    + "table " + table_style
    + "tr" + table_style
    + "td" + table_style;

    /************************************************************************
     * IMPLEMENTATION                                                       *
     ************************************************************************/

    /**
     * This is a hack because Sun's style sheet can't handle
     * quoted arguments.
     */
    class NuprlDocument
    extends HTMLDocument
    {
        public Font getFont(AttributeSet attrs)
        {
            String family = StyleConstants.getFontFamily(attrs);
            System.err.println("Request for font: " + family);
            return super.getFont(attrs);
        }
    }

    void ShowStyles(HTMLDocument document)
    {
        StyleSheet styles = document.getStyleSheet();
        Enumeration rules = styles.getStyleNames();
        while (rules.hasMoreElements()) {
            String name = (String) rules.nextElement();
            Style rule = styles.getStyle(name);
            System.out.println(rule.toString());
        }
    }

    /*
     * Create the window.
     */
    public NuprlTerm(NuprlFrame frame, NuprlBus bus, String host, int port)
        throws NuprlException
    {
        super(VERTICAL_SCROLLBAR_ALWAYS, HORIZONTAL_SCROLLBAR_AS_NEEDED);

        // Save the frame
        this.frame = frame;

        // Create the HTML pane and configure it
        textPane = new JTextPane();
        textPane.setContentType("text/html");
        document = (HTMLDocument) textPane.getDocument();
        textPane.setCaretPosition(0);
        textPane.setMargin(new Insets(5, 5, 5, 5));
        textPane.setEditable(false);
        textPane.addHyperlinkListener(new NuprlHyperlinkListener());
        setViewportView(textPane);
        setPreferredSize(new Dimension(200, 200));

        // Load the style sheet and initial contents
        try {
            StyleSheet style_sheet = document.getStyleSheet();
            StringReader reader = new StringReader(hacked_style);
            style_sheet.loadRules(reader, null);
        }
        catch(IOException e) {
            // Ignore it
        }
        // ShowStyles(document);

        // Get cursors
        default_cursor = textPane.getCursor();
        wait_cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
        random = new Random();

        // Subscribe to the bus
        endpoint = bus.Subscribe(new NuprlTermClient(host), port);
        debug_host = host + "." + port;

        // Show what we're doing
        if(NuprlDebug.debug_term)
            System.err.println(debug_host + ": created");
    }

    /**
     * Set the client that will handle input events.
     */
    void setClient(NuprlBusPort client)
    {
        this.client = client;
    }

    /**
     * Disconnect from the bus.
     */
    void Close()
    {
        endpoint.Unsubscribe();
    }

    /************************************************************************
     * HANDLE HYPERLINKS                                                    *
     ************************************************************************/

    class NuprlHyperlinkListener
    implements HyperlinkListener
    {
        public void hyperlinkUpdate(HyperlinkEvent e)
        {
            if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                JEditorPane pane = (JEditorPane) e.getSource();
                if (e instanceof HTMLFrameHyperlinkEvent) {
                    HTMLFrameHyperlinkEvent  evt = (HTMLFrameHyperlinkEvent) e;
                    HTMLDocument doc = (HTMLDocument) pane.getDocument();
                    doc.processHTMLFrameHyperlinkEvent(evt);
                }
                else {
                    URL url = e.getURL();
                    String host = url.getHost();
                    String filename = url.getFile();
                    if(host.equals(NuprlConstants.METAPRL_CD_HOST) && filename.length() > 1) {
                        String dir = filename.substring(1);
                        if(dir.charAt(0) != '/')
                            dir = metaprl_base + "/" + dir;
                        dir = "cd%20%22" + dir + "%22";
                        endpoint.Send(NuprlConstants.CONTROL_NAME, 1, new NuprlCommandToken(dir));
                    }
                    else if(host.equals(NuprlConstants.METAPRL_COMMAND_HOST) && filename.length() > 0) {
                        // MetaPRL command
                        endpoint.Send(NuprlConstants.CONTROL_NAME, 1, new NuprlCommandToken(filename.substring(1)));
                    }
                    else {
                        try {
                            pane.setPage(e.getURL());
                        }
                        catch (Throwable t) {
                            t.printStackTrace();
                        }
                    }
                }
            }
        }
    }

    /************************************************************************
     * HANDLE INPUT                                                         *
     ************************************************************************/

    /**
     * Set the title.
     */
    protected void setTitle(int code, String title)
    {
        switch(code) {
        case NuprlConstants.METAPRL_BASE_CODE:
            try {
                System.err.println(debug_host + ": setting URL to: " + title);
                document.setBase(new URL(title));
            }
            catch(MalformedURLException e) {
                System.err.println("NuprlTerm: malformed url: " + title);
            }
            break;

        case NuprlConstants.METAPRL_DIR_CODE:
            // This is the document base to resolve cd commands
            metaprl_base = title;
            frame.setTitle("[" + endpoint.getPort() + "] " + title);
            break;
        }
    }

    /**
     * Data is collected in a buffer.
     */
    protected void HandleData(NuprlDataToken token)
    {
        if(NuprlDebug.debug_term)
            System.err.println(debug_host + ".HandleData");
        if(buffer == null)
            buffer = token;
        else
            buffer.append(token);
    }

    /**
     * A Sync closes the buffer.
     */
    protected void HandleSync(NuprlSyncToken token)
    {
        if(NuprlDebug.debug_term)
            System.err.println(debug_host + ".HandleSync");
        if(buffer != null) {
            String filename =
                "nocache/"
                + random.nextInt() 
                + "/cache/"
                + endpoint.getHost()
                + endpoint.getPort()
                + ".html";
            System.err.println("Loading file: " + filename);
            try {
                textPane.setPage(new URL(document.getBase(), filename));
                                         
            }
            catch(MalformedURLException e) {
                System.err.println("URL is malformed" + document.getBase().toString() + "/" + filename);
            }
            catch(IOException e) {
                System.err.println("Can't load page: " + e.getMessage());
            }
            buffer = null;
        }
    }

    /**
     * Handle commands.
     */
    protected void HandleArgument(NuprlArgumentToken token)
    {
        // Now parse the command
        switch(token.charcode) {
        case ']':
            if(NuprlDebug.debug_term)
                System.err.println("ESC ]");

            if(token.arguments.length == 2) {
                try {
                    setTitle(Integer.parseInt(token.arguments[0]), token.arguments[1]);
                }
                catch(NumberFormatException e) {
                    // Ignore it
                }
            }
            break;
        }
    }

    /**
     * Handle incoming messages.
     */
    protected void Poll(NuprlBusEndpoint endpt)
    {
        NuprlBusMessage msg;
        while((msg = endpt.getMessage()) != null) {
            NuprlToken token = msg.token;
            if(token instanceof NuprlDataToken)
                HandleData((NuprlDataToken) token);
            else if(token instanceof NuprlSyncToken)
                HandleSync((NuprlSyncToken) token);
            else if(token instanceof NuprlArgumentToken)
                HandleArgument((NuprlArgumentToken) token);
        }
    }

    /**
     * Handle an event in the regular window queue.
     */
    class NuprlTermEvent
        implements Runnable
    {
        NuprlBusEndpoint endpt;

        NuprlTermEvent(NuprlBusEndpoint endpt)
        {
            this.endpt = endpt;
        }

        public void run()
        {
            Poll(endpt);
        }
    }
            
    /**
     * This is the client interface.
     */
    class NuprlTermClient
        implements NuprlBusClient
    {
        /**
         * Our hostname.
         */
        String host;

        /**
         * Create the client.
         */
        NuprlTermClient(String host)
        {
            this.host = host;
        }

        /**
         * Our hostname.
         */
        public String getHost()
        {
            return host;
        }

        /**
         * Handle a wakeup from the bus.
         */
        public void wakeup(NuprlBusEndpoint endpt)
        {
            SwingUtilities.invokeLater(new NuprlTermEvent(endpt));
        }
    }
}
