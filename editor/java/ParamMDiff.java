/**
 * A meta-sum.
 */

import netscape.util.*;

public class ParamMDiff
extends ParamMPair
{
    /**
     * Make the number var.
     */
    public ParamMDiff(Param param1, Param param2)
    {
        super(param1, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_MDIFF;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_NUMBER;
    }

    /**
     * Display form.
     */
    public int display(Vector results, int i)
    {
        i = param1.display(results, i);
        results.insertElementAt(new TermString(" - "), i);
        return param2.display(results, i + 1);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:01  jyh
 * This is a simple term display in an applet.
 *
 */
