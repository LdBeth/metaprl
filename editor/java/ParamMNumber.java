/**
 * A meta-number.
 */

import netscape.util.*;

public class ParamMNumber
extends ParamMeta
{
    /**
     * Make the number var.
     */
    public ParamMNumber(String v)
    {
        super(v);
    }

    /**
     * Match against a number.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamNumber)
            return param2;
        throw new ParamMatchError("Parameter is not a number", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_MNUMBER;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_NUMBER;
    }

    /**
     * Display witha '$' to indicate meta.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString("$" + value + ":n"), i);
        return i + 1;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:10  jyh
 * This is a simple term display in an applet.
 *
 */
