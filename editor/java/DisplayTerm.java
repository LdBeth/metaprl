/**
 * The Display contains a formatter to display a term vector
 * as a sequence of characters.
 */

import netscape.application.*;
import netscape.util.*;

/*
 * A FormatError is an exception used as longjmp.
 */
class FormatError
extends Exception
{
    FormatError(String msg)
    {
        super(msg);
    }
}

/*
 * We use a context in the formatting that
 * contains info about the tab stop and the breaking
 * selection.
 */
class DisplayContext
{
    /* Stack of curent tab stops */
    IntStack tab_stack;

    /* Satck of current zones */
    IntStack zone_stack;

    /* Create an initial context */
    DisplayContext(int zone)
    {
        tab_stack = new IntStack(0);
        zone_stack = new IntStack(zone);
    }

    /* Push the left margin */
    void pushMargin(int tabstop)
    {
        tab_stack.push(tabstop);
    }

    /* Pop it */
    void popMargin()
    {
        tab_stack.pop();
    }

    /* Get the current left margin */
    int getMargin()
    {
        return tab_stack.top();
    }

    /* Push the zone */
    void pushZone(int zone)
    {
        zone_stack.push(zone);
    }

    /* Pop the zone */
    void popZone()
    {
        zone_stack.pop();
    }

    /* Get the current zone */
    int getZone()
    {
        return zone_stack.top();
    }
}
 
/*
 * The display comtains implicitely a display engine.
 */
class DisplayTerm
extends DisplayEngine
{
    /**
     * Format a term vector as a sequence of characters.
     * This algorithm is exponential in the number of
     * soft zones!
     *
     * When a soft zone is entered, it is first chosen
     * as linear.  If the width of the line exceeds the right
     * margin, then we restart and try the zone as hard.
     *
     * We collect the text into a tagged text buffer.
     * This buffer has a prespecified width.
     */
    static void format(Vector terms, int zone, TextBuffer buffer)
    {
        DisplayContext context = new DisplayContext(zone);
        try {
            format(context, terms, 0, buffer);
        }
        catch(FormatError x) {
            /*
             * A format error happens only if the zone is linear,
             * and the buffer will contain the part of the text
             * that displays on a single line.  Ignore this error
             * because the buffer contains enough to display the
             * line.
             */
        }
    }

    /*
     * Format into the text buffer.
     */
    static void format(DisplayContext context, Vector terms, int i, TextBuffer buffer)
        throws FormatError
    {
        int length = terms.size();
        while(i != length) {
            Term command = (Term) terms.elementAt(i);
            if(command instanceof TermString)
                add(context, (TermString) command, buffer);
            else if(command instanceof TermZone) {
                // This function adds all the remaining terms
                enterZone(context, (TermZone) command, terms, i + 1, buffer);
                break;
            }
            else if(command instanceof TermPush)
                push(context, (TermPush) command, buffer);
            else if(command instanceof TermPop)
                context.popMargin();
            else if(command instanceof TermBreak)
                do_break(context, (TermBreak) command, buffer);
            else if(command instanceof TermFont)
                font(context, (TermFont) command, buffer);
            else
                throw new FormatError("Term is not recognized");
            i++;
        }
    }

    /*
     * Add a string to the buffer.
     * raise FormatError if the margin is exceeded
     * and we are in linear or soft mode.
     */
    private static void add(DisplayContext context, TermString command, TextBuffer buffer)
        throws FormatError
    {
        buffer.append(command.value);
        if(context.getZone() == TermZone.SOFT_ZONE && buffer.overflow())
            throw new FormatError("Overflow");
    }

    /*
     * Enter a new break zone.
     * This function formats the remaining text.
     * Throws FormatError only if zone is linear.
     */
    private static void enterZone(DisplayContext context, TermZone zone, Vector terms, int i, TextBuffer buffer)
        throws FormatError
    {
        switch(zone.command) {
        case zone.LINEAR_ZONE:
        case zone.HARD_ZONE:
            context.pushZone(zone.command);
            format(context, terms, i, buffer);
            break;
        case zone.SOFT_ZONE:
            /* If the margin is already exceeded, make this a hard zone */
            if(buffer.overflow())
                context.pushZone(zone.HARD_ZONE);
            else {
                // Try soft zone first
                Object attributes = buffer.getAttributes();
                context.pushZone(zone.SOFT_ZONE);
                int length = buffer.length();
                try {
                    format(context, terms, i, buffer);
                }
                catch(FormatError x) {
                    // The soft zone failed, backup and try the hard format
                    context.popZone();
                    context.pushZone(zone.HARD_ZONE);
                    buffer.truncate(length);
                    buffer.setAttributes(attributes);
                    format(context, terms, i, buffer);
                }
            }
            break;
        case zone.END_ZONE:
            /* Pop the last zone */
            context.popZone();
            format(context, terms, i, buffer);
            break;
        }
    }
                
    /*
     * Push the left margin from the current column by hundredths of a point.
     */
    private static void push(DisplayContext context, TermPush push, TextBuffer buffer)
    {
        int column = buffer.getColumn();
        context.pushMargin(column + push.offset);
    }

    /*
     * Break the current line; we have to decide whether to break:
     *    1. If the zone is linear, don't break.
     *    2. Otherwise, if the break is hard, break.
     *    3. Otherwise, if the zone is hard, break.
     *    4. Otherwise, the zone is soft, and the break is soft, so don't break.
     */
    private static void do_break(DisplayContext context, TermBreak brk, TextBuffer buffer)
        throws FormatError
    {
        int zone = context.getZone();
        if(zone == TermZone.LINEAR_ZONE || (zone == TermZone.SOFT_ZONE && brk.soft))
            add(context, new TermString(brk.linear), buffer);
        else {
            // Take the break
            add(context, new TermString(brk.take), buffer);
            buffer.nl();
            buffer.tab(context.getMargin());
        }
    }

    /*
     * Change the current font.
     */
    private static void font(DisplayContext context, TermFont font, TextBuffer buffer)
    {
        if(font.style == FontBase.FONT_NOP && font.size == FontBase.FONT_NOP)
            buffer.popFont();
        else
            buffer.pushFont(font.style, font.size);
    }
}        

