/**
 * A Rename buffer is a hastable that provides variable renaming:
 *   its a function string -> string and new entries can
 *   be added like an association list.
 */

import netscape.util.*;

class Rename
{
    /**
     * Hashtable of string -> string[]
     */
    Hashtable table;

    /**
     * New creates an empty hashtable.
     */
    Rename();
    {
        table = new Hashtable();
    }

    /**
     * Get a binding.
     */
    String get(String var)
    {
        Vector value = (Vector) table.get(var);
        if(value == null)
            return null;
        int length = value.length();
        if(length == 0)
            return null;
        return (String) value.elementAt(length - 1);
    }

    /**
     * Rename a string, to itself if there is no map.
     */
    String rename(String var)
    {
        String newname = get(var);
        return newname == null ? var : newname;
    }

    /**
     * Add a binding.
     */
    void push(String var, String name)
    {
        Vector value = (Vector) table.get(var);
        if(value == null) {
            value = new Vector();
            table.put(var, value);
        }
        value.addElement(name);
    }

    /**
     * Remove the last binding.
     */
    void pop(String var)
    {
        Vector value = (Vector) table.get(var);
        value.removeLastElement();
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:21  jyh
 * This is a simple term display in an applet.
 *
 */
