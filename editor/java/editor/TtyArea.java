/**
 * This TextArea implements a window for interaction.
 * When <return> is pressed, the current line is
 * provided as output.
 *
 * The current line is prompted for at the end of the page.
 * Text is added before the prompt.
 */

import netscape.application.*;
import netscape.util.*;

/**
 * This text area maintains a vector of strings and a font.
 */
class TtyArea
extends TextView
implements Marshalable
{
    /*
     * Notify the actions through a queue.
     */
    private BlockingQueue queue;

    /**
     * There are three different kinds of text:
     *     1. Input from the keyboard
     *     2. Input from the program
     *     3. Response from net
     */
    private Hashtable key_hash;
    private Hashtable prog_hash;
    private Hashtable out_hash;
    public final static int KEY_TEXT = 0;
    public final static int PROG_TEXT = 1;
    public final static int OUT_TEXT = 2;
    public final static String INPUT_FLAG = "Input";
    private final static String PROMPT = "> ";

    /**
     * Allow max contents of 64K chars.
     */
    protected static final int MAX_SIZE = 65536;

    /**
     * Position of the prompt.
     */
    private Range prompt;

    /**
     * Size.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal((Marshalable) queue);
        info.Marshal(key_hash);
        info.Marshal(prog_hash);
        info.Marshal(out_hash);
        info.Marshal(prompt);
    }

    /**
     * Mode conversion.
     */
    private Hashtable AttributesForMode(int mode)
    {
        Hashtable hash;

        switch(mode) {
        case KEY_TEXT:
            hash = key_hash;
            break;
        case PROG_TEXT:
            hash = prog_hash;
            break;
        case OUT_TEXT:
            hash = out_hash;
            break;
        default:
            hash = null;
            break;
        }
        return hash;
    }
            
    /**
     * Reset the prompt region.
     * This is the last line with key_hash attributes.
     */
    private Range PromptRange()
    {
        Range range;

        Hashtable hash = attributesAtIndex(length());
        if(hash.containsKey(INPUT_FLAG) && ((Boolean) hash.get(INPUT_FLAG)).booleanValue()) {
            // Last char has key characteristic
            range = paragraphForIndex(length());
        }
        else
            range = new Range(length(), 0);
        return range;
    }

    /**
     * Cut the data down to 128K.
     */
    public synchronized void DeleteOverflow()
    {
        int len = length();
        if(len > 2 * MAX_SIZE) {
            Range range = paragraphForIndex(MAX_SIZE);
            range.length = range.index;
            range.index = 0;
            replaceRangeWithString(range, "");
        }
    }

    /**
     * Add to window.
     * Reset the prompt region, then insert
     * just before region.
     */
    public synchronized void AppendLine(String line, int mode)
    {
        Range prompt = PromptRange();
        Range select = selectedRange();
        Range range = new Range(prompt.index, 0);
        replaceRangeWithString(range, line + "\n");
        range.length = line.length() + 1;
        Hashtable hash = AttributesForMode(mode);
        if(hash != null)
            setAttributesForRange(hash, range);
        if(select.index >= prompt.index) {
            select.index += range.length;
            selectRange(select);
        }
        DeleteOverflow();
        scrollRangeToVisible(range);
    }

    /**
     * Get the current line and stuff it.
     */
    public synchronized void StuffLine()
    {
        Range range = selectedRange();
        if(range != Range.nullRange()) {
            // Stuff the current line
            range = paragraphForIndex(range.index + range.length);
            setAttributesForRange(key_hash, range);
            String command = stringForRange(range);
            if(command.startsWith("> "))
                command = command.substring(2);
            if(DebugFlags.tty_area)
                System.out.println("TtyArea.StuffLine: " + command);
            queue.PushFirst(command);

            // Add a <cr> if at the end of the page
            if(range.index + range.length == length())
                appendString("\n");

            // Produce a new prompt
            range = PromptRange();
            if(range.length == 0) {
                range = appendString(PROMPT);
                setAttributesForRange(key_hash, range);
            }

            // Move cursor to end of page
            range = new Range(length(), 0);
            selectRange(range);
        }
    }

    /**
     * Filter waits for <return>, then returns line ending at cursor.
     */
    class Filter
    implements TextFilter
    {
        public boolean acceptsEvent(Object textObject, KeyEvent event, Vector events)
        {
            if(event.isReturnKey() && event.modifiers == 0) {
                StuffLine();
                return false;
            }
            return true;
        }
    }

    /**
     * Constructor set the <cr> filter, and the queue
     * for placing output.
     */
    TtyArea(BlockingQueue q)
    {
        // Initialize the TextArea
        super(0, 0, 300, 200);

        // Input filter
        setFilter(new Filter());

        // Save queue
        queue = q;

        // Default attributes for the mode
        Font plain = Font.defaultFont();
        String family = plain.family();
        int size = plain.size();
        Font bold = new Font(family, Font.BOLD, size);
        Color red = new Color(127, 31, 0);
        Color blue = new Color(9, 80, 160);

        key_hash = new Hashtable();
        key_hash.put(TextView.FONT_KEY, bold);
        key_hash.put(TextView.TEXT_COLOR_KEY, red);
        key_hash.put(INPUT_FLAG, new Boolean(true));
        prog_hash = new Hashtable();
        prog_hash.put(TextView.FONT_KEY, plain);
        prog_hash.put(TextView.TEXT_COLOR_KEY, red);
        out_hash = new Hashtable();
        out_hash.put(TextView.FONT_KEY, plain);
        out_hash.put(TextView.TEXT_COLOR_KEY, blue);

        // Initial prompt
        Range prompt = appendString(PROMPT);
        setAttributesForRange(key_hash, prompt);

        // Window management
        setHorizResizeInstruction(View.WIDTH_CAN_CHANGE);
        setVertResizeInstruction(View.HEIGHT_CAN_CHANGE);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:28  jyh
 * This is a simple term display in an applet.
 *
 * Revision 1.5  1997/12/15 22:16:58  jyh
 * First working version using ocaml-1.07
 *
 * Revision 1.4  1997/12/15 15:26:24  jyh
 * Upgrading to ocaml-1.07.
 *
 * Revision 1.3  1997/11/14 22:08:16  jyh
 * Small changes.
 *
 * Revision 1.2  1997/11/05 23:10:04  jyh
 * Initial recording.
 *
 * Revision 1.1  1997/10/15 01:25:17  jyh
 * Converted from ranks to abstract handles (Appl_handle).
 *
 * Revision 1.1  1997/07/25 19:12:05  jyh
 * Raw Java version.
 *
 */
