/**
 * This is the HTML syntax for a term.
 * <term opname="name">
 *     subterms...
 * </term>
 */

import netscape.application.*;
import netscape.util.*;

public class HTMLTerm
extends TextViewHTMLContainer
{
    /* Contents are in bold */
    public Hashtable attributesForContents(Hashtable context, Hashtable initial, TextView view)
    {
        Hashtable hash = (Hashtable) initial.clone();
        hash.put(view.TEXT_COLOR_KEY, Color.red);
        return hash;
    }

    /* Return the result as a string */
    public String string(Hashtable context)
    {
        return "No way";
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:59  jyh
 * This is a simple term display in an applet.
 *
 */
