/*
 * This is an internal window for editing.
 */
package edu.cornell.cs.jyh.nuprl;

import java.net.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.text.html.*;
import javax.swing.event.*;

public class NuprlText
extends JScrollPane
implements KeyListener, ComponentListener
{
    /**
     * Text attributes.
     */
    protected Style stderr_style;
    protected Style stdout_style;
    protected Style stdin_style;

    /**
     * Terminal styles.
     */
    protected static final int TERMINAL_STYLE_COUNT     = 8;
    protected static final int OVERWRITE_MODE           = 0;
    protected static final int INSERT_MODE              = 1;

    protected Style[] terminal_styles                     = new Style[TERMINAL_STYLE_COUNT];
    protected Style terminal_style;
    protected int terminal_cursor                         = 0;
    protected int terminal_mode                           = OVERWRITE_MODE;
    protected int terminal_width                          = 80;
    protected int terminal_height                         = 65;
    protected char[] terminal_blanks;

    /**
     * Text window.
     */
    protected static final int MAX_SIZE = 65536;
    protected StyledDocument document;
    protected JTextPane textPane;
    protected Caret textCaret;

    /**
     * Actions for key bindings.
     */
    protected Hashtable actions;

    /**
     * Target for commands.
     */
    protected NuprlBusPort client;
    protected String host;

    /**
     * Our endpoint on the bus.
     */
    protected NuprlBusEndpoint endpoint;

    /**
     * Enclosing frame.
     */
    protected NuprlFrame frame;

    /*
     * Options.
     */
    protected boolean terminalMode;
    protected Keymap originalKeymap;
    protected Keymap editKeymap;

    /************************************************************************
     * IMPLEMENTATION                                                       *
     ************************************************************************/

    /**
     * Override processInputMethodEvent
     * to turn off bells.
     */
    static class NuprlPane
    extends JTextPane
    {
        public NuprlPane(StyledDocument document)
        {
            super(document);
        }

        /**
         * Override replaceSelection.
         */
        public void replaceSelection(String content)
        {
            if(isEditable())
                super.replaceSelection(content);
        }

        protected void processInputMethodEvent(InputMethodEvent e)
        {
            if(isEditable())
                super.processInputMethodEvent(e);
        }
    }

    /*
     * Create the window.
     */
    NuprlText(NuprlBus bus, String host, int port, boolean terminalMode, NuprlFrame frame)
        throws NuprlException
    {
        // Some initial setup
        super(VERTICAL_SCROLLBAR_ALWAYS, HORIZONTAL_SCROLLBAR_AS_NEEDED);

        // Remember the frame
        this.frame = frame;

        // Create the text pane and configure it.
        document = new DefaultStyledDocument();
        textPane = new NuprlPane(document);
        initStyles();
        textPane.setCaretPosition(0);
        textPane.setMargin(new Insets(5, 5, 5, 5));
        createActionTable(textPane);
        addComponentListener(this);

        // Add some key bindings to the keymap.
        addKeymapBindings(textPane);
        setTerminalMode(terminalMode);

        // Set the contents
        setViewportView(textPane);
        setPreferredSize(new Dimension(400, 80));

        // Subscribe to the bus
        endpoint = bus.Subscribe(new NuprlTextClient(host), port);
    }

    /**
     * Close this window by unsubscribing to the bus.
     */
    void Close()
    {
        endpoint.Unsubscribe();
    }

    /**
     * Set the destination for input event.
     */
    void setClient(NuprlBusPort client)
    {
        this.client = client;
    }

    /**
     * Put the window in terminal mode.
     */
    public void setTerminalMode(boolean terminalMode)
    {
        this.terminalMode = terminalMode;
        if(terminalMode) {
            textPane.setKeymap(originalKeymap);
            textPane.addKeyListener(this);
            terminal_blanks = new char[terminal_width];
            for(int i = 0; i != terminal_width; i++)
                terminal_blanks[i] = ' ';
            terminal_mode = OVERWRITE_MODE;
        }
        else {
            textPane.setKeymap(editKeymap);
            textPane.removeKeyListener(this);
        }
        textPane.setEditable(!terminalMode);
        textCaret = textPane.getCaret();
        textCaret.setVisible(true);
    }

    /*
     * Text attributes.
     */
    protected void initStyles()
    {
        Style default_style = StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);

        /*
         * Set up the styles for the editor.
         */
        stdout_style = document.addStyle("stdout", default_style);
        StyleConstants.setBold(stdout_style, false);
        StyleConstants.setForeground(stdout_style, Color.black);

        stderr_style = document.addStyle("stderr", stdout_style);
        StyleConstants.setForeground(stderr_style, Color.red);

        stdin_style = document.getStyle("default");
        StyleConstants.setFontFamily(stdin_style, "SansSerif");
        StyleConstants.setFontSize(stdin_style, 18);
        StyleConstants.setBold(stdin_style, true);
        StyleConstants.setForeground(stdin_style, new Color((float) 0.1, (float) 0.3, (float) 0.5));

        /*
         * Set up the styles for the terminal.
         */
        Style fixed_style = document.addStyle("terminal", stdout_style);
        StyleConstants.setFontFamily(fixed_style, "Monospaced");
        StyleConstants.setFontSize(fixed_style, 18);

        for(int i = 0; i != 4; i++)
            terminal_styles[i] = document.addStyle("terminal" + i, fixed_style);
        StyleConstants.setBold(terminal_styles[1], true);
        StyleConstants.setUnderline(terminal_styles[2], true);
        StyleConstants.setForeground(terminal_styles[3], Color.blue);

        terminal_styles[4] = document.addStyle("terminal4", stdout_style);
        StyleConstants.setBackground(terminal_styles[4], Color.blue);
        StyleConstants.setForeground(terminal_styles[4], Color.white);
        for(int i = 5; i != 8; i++)
            terminal_styles[i] = document.addStyle("terminal" + i, terminal_styles[4]);
        StyleConstants.setBold(terminal_styles[6], true);
        StyleConstants.setUnderline(terminal_styles[7], true);

        terminal_style = terminal_styles[0];
        terminal_cursor = 0;
    }

    /************************************************************************
     * HISTORY MANAGEMENT                                                   *
     ************************************************************************/

    /**
     * There is a shared copy of the history for all instances of this class.
     */
    static Vector history = new Vector();
    int history_index = 0;

    /**
     * Add a line to the history.
     */
    void appendToHistory(String line)
    {
        history.addElement(new String(line));
        history_index = history.size() - 1;
    }

    /**
     * Scroll the history line.
     */
    void scrollHistory(int scroll)
    {
        int index = history_index + scroll;
        if(index < 0 || index >= history.size())
            getToolkit().beep();
        else {
            try {
                history_index = index;
                String line = (String) history.elementAt(index);
                Element elem = document.getParagraphElement(textCaret.getDot());
                int start = elem.getStartOffset();
                int end = elem.getEndOffset() - 1;
                int len = line.length();
                document.insertString(start, line, stdin_style);
                document.remove(start + len, end - start);
            }
            catch(BadLocationException e) {
                // Ignore it
            }
        }
    }

    /************************************************************************
     * I/O                                                                  *
     ************************************************************************/

    /*
     * Add to window.
     * Reset the prompt region, then insert
     * just before region.
     */
    public synchronized void Append(String line)
    {
        if(terminalMode) {
            try {
                int length = document.getLength();
                if(terminal_cursor >= length) {
                    if(NuprlDebug.debug_text_terminal)
                        System.err.println("Append(" + line.length() + "): " + line);
                    document.insertString(length, line, terminal_style);
                    terminal_cursor = length + line.length();
                }
                else if(terminal_mode == OVERWRITE_MODE) {
                    if(NuprlDebug.debug_text_terminal)
                        System.err.println("Overwrite(" + line.length() + "): " + line);
                    OverwriteText(line);
                }
                else {
                    if(NuprlDebug.debug_text_terminal)
                        System.err.println("Insert(" + line.length() + "): " + line);
                    InsertText(line);
                }
                textCaret.setDot(terminal_cursor);
                textCaret.setVisible(true);
            }
            catch(BadLocationException e) {
                System.err.println("AppendLine: terminal cursor is out of range: 0 <= "
                                   + terminal_cursor
                                   + " <= "
                                   + document.getLength());
            }
        }
        else {
            // In editing mode, insert before the current line of input
            Style style = stdout_style;
            try {
                document.insertString(textCaret.getDot(), line, style);

                // Check for overflow
                int length = document.getLength();
                if(length > 2 * MAX_SIZE) {
                    Element elem = document.getParagraphElement(MAX_SIZE);
                    int start = elem.getStartOffset();
                    document.remove(0, start);
                }
            }
            catch (BadLocationException x) {
                System.err.println("AppendLine: locations are bad");
            }
        }
    }

    /*
     * Handle a command.
     */
    protected synchronized void StuffLine()
    {
        int pos = textPane.getCaretPosition();
        if(pos > 0)
            pos--;
        Element elem = document.getParagraphElement(pos);
        int start = elem.getStartOffset();
        int end = elem.getEndOffset();
        try {
            // Get the line
            String line = document.getText(start, end - start);
            appendToHistory(line);

            // And send the entire line
            if(NuprlDebug.debug_text)
                System.err.println("Command: " + line);
            if(client != null)
                endpoint.Send(client, new NuprlDataToken(line));
        }
        catch(BadLocationException e) {
            System.err.println("GetLine failed");
        }
    }

    /**
     * Show next item in history.
     */
    protected class HistoryAction
        extends AbstractAction
    {
        int scroll;

        public HistoryAction(int i)
        {
            super("History " + (i > 0 ? "+" : "-") + i);
            scroll = i;
            setEnabled(false);
        }

        public void actionPerformed(ActionEvent e)
        {
            scrollHistory(scroll);
        }
    }

    /**
     * The action that is called when a <return> is pressed.
     */
    protected class StuffAction
        extends AbstractAction
    {
        public StuffAction() {
            super("Stuff");
            setEnabled(false);
        }

        public void actionPerformed(ActionEvent e) {
            StuffLine();
        }
    }

    /**
     * Handle the key typed event from the text area.
     * This isn't supported on Win32, so we ignore it.
     */
    public void keyTyped(KeyEvent e)
    {
    }

    /** Handle the key pressed event from the text field. */
    public void keyPressed(KeyEvent e)
    {
        if(terminalMode && client != null) {
            char c = e.getKeyChar();
            if(c >= 0 && c < 256)
                endpoint.Send(client, new NuprlDataToken((byte) c));
        }
    }

    /**
     * Handle the key released event from the text area.
     * Ignore it.
     */
    public void keyReleased(KeyEvent e)
    {
    }

    /************************************************************************
     * ComponentListener for NuprlCommand                                   *
     ************************************************************************/

    /**
     * Applet is hidden.
     */
    public void componentHidden(ComponentEvent event)
    {
    }

    /**
     * Component is shown.
     */
    public void componentShown(ComponentEvent event)
    {
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
        Dimension dim = getViewport().getExtentSize();
        Font font = document.getFont(terminal_style);
        FontMetrics metrics = textPane.getFontMetrics(font);
        int width = metrics.charWidth('M');
        int height = metrics.getHeight();
        if(width < 5)
            width = 5;
        if(height < 6)
            height = 6;
        terminal_width = dim.width / width - 4;
        if(terminal_width < 10)
            terminal_width = 10;
        terminal_height = dim.height / height;
        if(terminal_height < 1)
            terminal_height = 1;
        terminal_blanks = new char[terminal_width];
        for(int i = 0; i != terminal_width; i++)
            terminal_blanks[i] = ' ';
        if(NuprlDebug.debug_text_terminal) {
            System.err.println("SIGWINCH: Pixels (" + dim.width + ", " + dim.height
                               + ") Charpixels (" + width + ", " + height
                               + ") Charsize (" + terminal_width + ", " + terminal_height + ")");
        }
        if(client != null)
            endpoint.Send(client, windowSizeToken());
    }

    /************************************************************************
     * TERMINAL FUNCTIONS                                                   *
     ************************************************************************/

    /**
     * Generate a string of the given width.
     */
    protected String blankString(int count)
    {
        return new String(terminal_blanks, 0, count);
    }

    /**
     * Append a completely blank line to the document.
     */
    protected void insertBlankLine(int pos)
        throws BadLocationException
    {
        document.insertString(pos, blankString(terminal_width), terminal_style);
    }

    /**
     * Validate a line by making sure it has at least as many characters
     * to give a valid cursor.  The element should be valid.
     */
    protected void validateLine(Element elem)
        throws BadLocationException
    {
        int start = elem.getStartOffset() + terminal_width;
        int end = elem.getEndOffset() - 1;
        if(start < end)
            document.remove(start, end - start);
        else if(start > end)
            document.insertString(end, blankString(start - end), terminal_style);
        if(terminal_cursor >= start)
            terminal_cursor = start - 1;
    }

    /**
     * Validate this many lines including the current one.
     * Possibly scroll.
     */
    protected void validateDown(int count)
        throws BadLocationException
    {
    }

    /**
     * Move home.
     */
    protected void CursorHome()
    {
        terminal_cursor = 0;
    }

    /**
     * Move to beginning of line.
     */
    protected void CursorCR()
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        if(NuprlDebug.debug_text_terminal)
            System.err.println("CTRL-M: old: " + terminal_cursor + " new: " + elem.getStartOffset());
        terminal_cursor = elem.getStartOffset();
    }

    /**
     * Move to the next line.
     */
    protected void CursorNL()
        throws BadLocationException
    {
        int length = document.getLength();
        if(terminal_cursor == length) {
            document.insertString(terminal_cursor, "\n", terminal_style);
            terminal_cursor++;
        }
        else {
            Element elem = document.getParagraphElement(terminal_cursor);
            int end = elem.getEndOffset();
            if(NuprlDebug.debug_text_terminal) {
                System.err.println("CTRL-J: old: " + terminal_cursor
                                   + " start: " + elem.getStartOffset()
                                   + " new: " + end
                                   + " length: " + length);
            }
            terminal_cursor = end;
            if(terminal_mode == INSERT_MODE || end >= length)
                document.insertString(end - 1, "\n", terminal_style);
        }
    }

    /**
     * Move to a particular position.
     */
    protected void CursorMove(int row, int col)
        throws BadLocationException
    {
        terminal_cursor = 0;
        CursorDown(row);
        CursorRight(col);
    }

    /**
     * Move the cursor left.
     * Do not move past the left margin.
     */
    protected void CursorLeft(int count)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        if(terminal_cursor - count < start)
            terminal_cursor = start;
        else
            terminal_cursor -= count;
    }

    /**
     * Move right.
     * Extend the line if necessary, but do not
     * extend past the right margin.
     */
    protected void CursorRight(int count)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();

        // Don't extend past the line width
        if(terminal_cursor + count - start > terminal_width)
            terminal_cursor = start + terminal_width;
        else
            terminal_cursor += count;

        // Extend the line if necessary
        validateLine(elem);
    }

    /**
     * Move down.
     * Scroll the window if necessary.
     */
    protected void CursorDown(int count)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        validateLine(elem);
        while(count > 0) {
            terminal_cursor += terminal_width + 1;
            int length = document.getLength();
            if(terminal_cursor > length)
                insertBlankLine(length);
            else {
                elem = document.getParagraphElement(terminal_cursor);
                validateLine(elem);
            }
            count--;
        }
    }

    /**
     * Move up, but don't go past the beginning of the document.
     */
    protected void CursorUp(int count)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        validateLine(elem);
        while(count > 0 && terminal_cursor > terminal_width) {
            terminal_cursor -= terminal_width + 1;
            elem = document.getParagraphElement(terminal_cursor);
            validateLine(elem);
        }
    }

    /**
     * Insert some lines before this one.
     */
    protected void InsertLines(int count)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        while(count >= 0) {
            insertBlankLine(start);
            terminal_cursor += terminal_width + 1;
        }
    }

    /**
     * Insert some blank chars.
     */
    protected void InsertBlanks(int count)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        validateLine(elem);
        document.insertString(terminal_cursor, blankString(count), terminal_style);
        document.remove(start + terminal_width, count);
    }

    /**
     * Delete one char back.
     */
    protected void BackSpace()
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        if(terminal_cursor > start)
            document.remove(--terminal_cursor, 1);
    }

    /**
     * Add some tabs.
     */
    protected void Tab()
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        int count = 8 - ((terminal_cursor - start) % 8);
        Append(blankString(count));
    }

    /**
     * Delete some chars.
     */
    protected void DeleteChars(int count)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        int remaining = terminal_width - terminal_cursor + start;
        if(count > remaining)
            count = remaining;
        validateLine(elem);
        document.remove(terminal_cursor, count);
        document.insertString(start + terminal_width - count, blankString(count), terminal_style);
    }

    /**
     * Delete the current line.
     */
    protected void DeleteLines(int count)
        throws BadLocationException
    {
        while(count >= 0) {
            Element elem = document.getParagraphElement(terminal_cursor);
            int start = elem.getStartOffset();
            int end = elem.getEndOffset() - 1;
            document.remove(start, end - start + 1);
            terminal_cursor = start;
            count--;
        }
    }

    /**
     * Delete to end-of-screen.
     */
    protected void KillScreen()
        throws BadLocationException
    {
        int pos = terminal_cursor;
        while(true) {
            int length = document.getLength();
            if(pos >= length)
                break;
            Element elem = document.getParagraphElement(pos);
            int start = elem.getStartOffset();
            int end = elem.getEndOffset() - 1;
            document.remove(pos, end - pos);
            document.insertString(pos, blankString(start + terminal_width - pos), terminal_style);
            pos = start + terminal_width + 1;
        }
    }

    /**
     * Delete to end of line.
     */
    protected void KillLine()
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        int end = elem.getEndOffset() - 1;
        if(start + terminal_width > terminal_cursor)
            document.insertString(end, blankString(start + terminal_width - terminal_cursor), terminal_style);
        if(end > terminal_cursor)
            document.remove(terminal_cursor, end - terminal_cursor);
    }

    /**
     * Scroll region.
     * I don't know what this means.
     */
    protected void ScrollRegion(int line1, int line2)
    {
    }

    /**
     * Start overwrite mode.
     */
    protected void OverwriteMode()
    {
        terminal_mode = OVERWRITE_MODE;
    }

    /**
     * Start insert mode.
     */
    protected void InsertMode()
    {
        terminal_mode = INSERT_MODE;
    }

    /**
     * Insert a line of text.
     */
    protected void InsertText(String text)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        int end = elem.getEndOffset() - 1;
        int length = text.length();
        document.insertString(terminal_cursor, text, terminal_style);
        if(end + length > start + terminal_width) {
            document.remove(start + terminal_width, end + length - start - terminal_width);
            terminal_cursor = end;
        }
        else
            terminal_cursor += length;
    }

    /**
     * Overwrite a pice of text.
     */
    protected void OverwriteText(String text)
        throws BadLocationException
    {
        Element elem = document.getParagraphElement(terminal_cursor);
        int start = elem.getStartOffset();
        int end = elem.getEndOffset() - 1;
        int length = text.length();
        document.insertString(terminal_cursor, text, terminal_style);
        if(terminal_cursor + length > start + terminal_width) {
            length = terminal_cursor + length - start - terminal_width;
            document.remove(start + terminal_width, length);
            terminal_cursor = start + terminal_width - 1;
        }
        else {
            int amount = length;
            if(end - terminal_cursor < length)
                amount = end - terminal_cursor;
            if(NuprlDebug.debug_text_terminal) {
                System.err.println("OverwriteText(" + length + ") " + text
                                   + " old: " + terminal_cursor
                                   + " amount: " + amount
                                   + " start: " + start
                                   + " end: " + end);
            }
            document.remove(terminal_cursor + length, amount);
            terminal_cursor += length;
        }
    }

    /**
     * Set the title.  Check for the special title
     *   MetaPRL http://hostname:port/password
     */
    protected static String title_head = "MetaPRL ";
    protected static int title_head_length = title_head.length();

    protected void setTitle(String title)
    {
        if(title_head.regionMatches(0, title, 0, title_head_length)) {
            try {
                URL url = new URL(title.substring(title_head_length));
                endpoint.Send(NuprlConstants.CONTROL_NAME, 1, new NuprlURLToken(NuprlURLToken.PASSWORD, url));
            }
            catch(MalformedURLException e) {
                frame.setTitle(title);
            }
        }
        else
            frame.setTitle(title);
    }

    /************************************************************************
     * OPTION HANDLING                                                      *
     ************************************************************************/

    /**
     * Handle a terminal argument.
     */
    private void HandleArgumentError(NuprlArgumentToken token)
        throws BadLocationException
    {
        // Try to parse the arguments into numbers
        String[] string_args = token.arguments;
        boolean[] int_flags = new boolean[string_args.length];
        int[] int_args = new int[string_args.length];
        for(int i = 0; i != string_args.length; i++) {
            if(string_args[i].length() == 0) {
                int_flags[i] = true;
                int_args[i] = 0;
            }
            else {
                try {
                    int_args[i] = Integer.parseInt(string_args[i]);
                    int_flags[i] = true;
                }
                catch(NumberFormatException e) {
                    // Ignore it
                }
            }
        }

        // Now parse the command
        switch(token.charcode) {
        case ']':
            if(NuprlDebug.debug_text_terminal)
                System.err.println("ESC ]");

            if(token.arguments.length == 2) {
                if(int_flags[0] && int_args[0] == 0)
                    setTitle(string_args[1]);
            }
            break;

        case '[':
        case '?':
            if(NuprlDebug.debug_text_terminal) {
                StringBuffer buffer = new StringBuffer();
                buffer.append("ESC [");
                for(int i = 0; i != string_args.length; i++) {
                    if(i != 0)
                        buffer.append(";");
                    buffer.append(string_args[i]);
                }
                buffer.append((char) token.c);
                buffer.append(" (");
                buffer.append(token.c);
                buffer.append(")");
                System.err.println(buffer.toString());
            }
            switch(token.c) {
            case 'm':
                // Change the printing mode
                if(int_flags[0]) {
                    int index = int_args[0] % TERMINAL_STYLE_COUNT;
                    terminal_style = terminal_styles[index];
                }
                break;

            case 'L':
                // Insert %1 lines
                if(int_flags[0])
                    InsertLines(int_args[0]);
                break;

            case 'P':
                // Delete %1 characters
                if(int_flags[0])
                    DeleteChars(int_args[0]);
                break;

            case 'M':
                // Delete %1 lines
                if(int_flags[0])
                    DeleteLines(int_args[0]);
                break;

            case 'B':
                // Cursor down %1 lines
                if(int_flags[0])
                    CursorDown(int_args[0] + 1);
                break;

            case '@':
                // Insert %d characters
                if(int_flags[0])
                    InsertBlanks(int_args[0]);
                break;

            case 'D':
                // Cursor left %1 characters
                if(int_flags[0])
                    CursorLeft(int_args[0]);
                break;

            case 'C':
                // Cursir right %1 characters
                if(int_flags[0])
                    CursorRight(int_args[0]);
                break;

            case 'A':
                // Cursor up %1 lines
                if(int_flags[0])
                    CursorUp(int_args[0]);
                break;

            case 'J':
                // Clear to end of screen
                KillScreen();
                break;

            case 'K':
                // Clear to end of line
                KillLine();
                break;

            case 'H':
                // Move to row %1, column %2
                if(int_flags.length >= 2 && int_flags[0] && int_flags[1])
                    CursorMove(int_args[0] - 1, int_args[1] - 1);
                else
                    CursorHome();
                break;

            case 'r':
                // Scroll region from line %1 to line %2
                if(int_flags.length >= 2 && int_flags[0] && int_flags[1])
                    ScrollRegion(int_args[0] - 1, int_args[1] - 1);
                break;

            case 'l':
                if(int_flags[0]) {
                    switch(int_args[0]) {
                    case 4:
                        // End insert mode
                        OverwriteMode();
                        break;
                    }
                }
                break;

            case 'h':
                if(int_flags[0]) {
                    switch(int_args[0]) {
                    case 4:
                        InsertMode();
                        break;
                    }
                }
                break;

            default:
                System.err.println("NuprlText.ESC [ " + ((char) token.c) + " (" + token.c + ") not handled");
                break;
            }
            break;

        default:
            System.err.println("NuprlText.ESC " + ((char) token.charcode) + " " + ((char) token.c) + " not handled");
            break;
        }
    }

    protected void HandleArgument(NuprlArgumentToken token)
    {
        try {
            HandleArgumentError(token);
            if(terminalMode) {
                textCaret.setDot(terminal_cursor);
                textCaret.setVisible(true);
            }
        }
        catch(BadLocationException e) {
            System.err.println("HandleArgument: bad location");
        }
    }

    /**
     * Handle a control char.
     */
    protected void HandleControl(NuprlControlToken token)
    {
        if(NuprlDebug.debug_text_terminal)
            System.err.println("CTRL-" + ((char) (token.c + '@')) + " (" + token.c + ")");

        try {
            switch(token.c) {
            case 'H' - '@':
                BackSpace();
                break;
            case 'M' - '@':
                CursorCR();
                break;
            case 'J' - '@':
                CursorNL();
                break;
            case 'I' - '@':
                Tab();
                break;
            }
            if(terminalMode) {
                textCaret.setDot(terminal_cursor);
                textCaret.setVisible(true);
            }
        }
        catch(BadLocationException e) {
            System.err.println("HandleControl: bad location");
        }
    }

    /**
     * Format the window size token.
     */
    private NuprlToken windowSizeToken()
    {
        byte[] buffer = new byte[5];
        int width = terminal_width - 1;
        buffer[0] = NuprlClient.TELNET_OPT_NAWS;
        buffer[1] = (byte) ((width >> 8) & 0xff);
        buffer[2] = (byte) (width & 0xff);
        buffer[3] = (byte) ((terminal_height >> 8) & 0xff);
        buffer[4] = (byte) (terminal_height & 0xff);
        return new NuprlOptionSBToken(buffer);
    }

    /**
     * Respond to an option.
     */
    private void OptionReply(NuprlBusEndpoint endpt, String host, int port, byte option)
    {
        byte[] buffer;

        // Show negotiation
        if(NuprlDebug.debug_text)
            System.err.println("NuprlText.OptionReply: " + host + "." + port + " " + option);

        // Don't take any options except binary mode
        switch(option) {
        case NuprlClient.TELNET_OPT_BINARY:
            endpt.Send(host, port, new NuprlOptionResponseToken(option, true));
            break;
        case NuprlClient.TELNET_OPT_TTYPE:
            endpt.Send(host, port, new NuprlOptionResponseToken(option, true));
            buffer = new byte[7];
            buffer[0] = NuprlClient.TELNET_OPT_TTYPE;
            buffer[1] = NuprlClient.TELNET_OPT_SB_IS;
            buffer[2] = (byte) 'x';
            buffer[3] = (byte) 't';
            buffer[4] = (byte) 'e';
            buffer[5] = (byte) 'r';
            buffer[6] = (byte) 'm';
            endpt.Send(host, port, new NuprlOptionSBToken(buffer));
            endpt.Send(host, port, new NuprlOptionRequestToken(NuprlClient.TELNET_OPT_NAWS, true));
            break;
        case NuprlClient.TELNET_OPT_NAWS:
            endpt.Send(host, port, new NuprlOptionResponseToken(option, true));
            endpt.Send(host, port, windowSizeToken());
            break;
        case NuprlClient.TELNET_OPT_ECHO:
        default:
            endpt.Send(host, port, new NuprlOptionResponseToken(option, false));
            break;
        }
    }

    /**
     * Handle an option request.
     */
    private void OptionRequest(NuprlBusEndpoint endpt, String host, int port, NuprlOptionRequestToken token)
    {
        OptionReply(endpt, host, port, token.option);
    }

    /**
     * Record the option status.
     */
    private void OptionResponse(NuprlBusEndpoint endpt, String host, int port, NuprlOptionResponseToken token)
    {
        OptionReply(endpt, host, port, token.option);
    }

    /**
     * Drop any extended options.
     */
    private void OptionBlock(NuprlBusEndpoint endpt, String host, int port, NuprlOptionBlockToken token)
    {
    }

    /**
     * This is the client interface.
     */
    class NuprlTextClient
        implements NuprlBusClient
    {
        /**
         * Our hostname.
         */
        String host;

        /**
         * Create the client.
         */
        NuprlTextClient(String host)
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
            NuprlBusMessage msg;
            while((msg = endpt.getMessage()) != null) {
                NuprlToken token = msg.token;
                if(token instanceof NuprlDataToken)
                    Append(new String(((NuprlDataToken) token).getData()));
                else if(token instanceof NuprlArgumentToken)
                    HandleArgument((NuprlArgumentToken) token);
                else if(token instanceof NuprlControlToken)
                    HandleControl((NuprlControlToken) token);
                else if(token instanceof NuprlOptionRequestToken)
                    OptionRequest(endpt, msg.src_host, msg.src_port, (NuprlOptionRequestToken) token);
                else if(token instanceof NuprlOptionResponseToken)
                    OptionResponse(endpt, msg.src_host, msg.src_port, (NuprlOptionResponseToken) token);
                else if(token instanceof NuprlOptionBlockToken)
                    OptionBlock(endpt, msg.src_host, msg.src_port, (NuprlOptionBlockToken) token);
            }
        }
    }

    /************************************************************************
     * ACTIONS                                                              *
     ************************************************************************/

    /*
     * Add some Emacs key bindings,
     * and capture the <return> event.
     */
    protected void addKeymapBindings(JTextPane textPane)
    {
        // Add a new key map to the keymap hierarchy.
        originalKeymap = textPane.getKeymap();
        editKeymap = textPane.addKeymap("MyEmacsBindings", originalKeymap);

        // Ctrl-b to go backward one character
        Action action = getActionByName(DefaultEditorKit.backwardAction);
        KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_B, Event.CTRL_MASK);
        editKeymap.addActionForKeyStroke(key, action);

        // Ctrl-f to go forward one character
        action = getActionByName(DefaultEditorKit.forwardAction);
        key = KeyStroke.getKeyStroke(KeyEvent.VK_F, Event.CTRL_MASK);
        editKeymap.addActionForKeyStroke(key, action);

        // Ctrl-p to go up one line
        action = new HistoryAction(-1);
        key = KeyStroke.getKeyStroke(KeyEvent.VK_P, Event.CTRL_MASK);
        editKeymap.addActionForKeyStroke(key, action);

        // Ctrl-n to go down one line
        action = new HistoryAction(1);
        key = KeyStroke.getKeyStroke(KeyEvent.VK_N, Event.CTRL_MASK);
        editKeymap.addActionForKeyStroke(key, action);

        // Ctrl-m to perform the action
        action = new StuffAction();
        key = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        editKeymap.addActionForKeyStroke(key, action);
    }

    /*
     * The following two methods allow us to find an
     * action provided by the editor kit by its name.
     */
    private void createActionTable(JTextComponent textComponent)
    {
        actions = new Hashtable();
        Action[] actionsArray = textComponent.getActions();
        for (int i = 0; i < actionsArray.length; i++) {
            Action a = actionsArray[i];
            actions.put(a.getValue(Action.NAME), a);
        }
    }

    private Action getActionByName(String name) {
        return (Action)(actions.get(name));
    }

    /************************************************************************
     * MENUS                                                                *
     ************************************************************************/

    /*
     * The edit menu allows undo/redo and copy/paste.
     */
    JMenu createEditMenu()
    {
        JMenu menu = new JMenu("Edit");

        // These actions come from the default editor kit.
        // Get the ones we want and stick them in the menu.
        menu.add(getActionByName(DefaultEditorKit.cutAction));
        menu.add(getActionByName(DefaultEditorKit.copyAction));
        menu.add(getActionByName(DefaultEditorKit.pasteAction));

        menu.addSeparator();

        menu.add(getActionByName(DefaultEditorKit.selectAllAction));

        return menu;
    }
}
