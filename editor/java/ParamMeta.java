/**
 * This is a generic class from which named meta-type
 * parameters are derived.
 */

import netscape.util.*;

public abstract class ParamMeta
extends ParamString
{
    /**
     * Constructor.
     */
    public ParamMeta(String s)
    {
        super(s);
    }

    /**
     * Get the name of the meta variable.
     */
    public String getName()
    {
        return value;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:27  jyh
 * This is a simple term display in an applet.
 *
 */
