/**
 * A meta-string.
 */

import netscape.util.*;

public class ParamMToken
extends ParamString
{
    /**
     * Make the number var.
     */
    public ParamMToken(String v)
    {
        super(v);
    }

    /**
     * Match against a token.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamToken)
            return param2;
        throw new ParamMatchError("Parameter is not a token", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_MTOKEN;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_TOKEN;
    }

    /**
     * Display witha '$' to indicate meta.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString("$" + value + ":t"), i);
        return i + 1;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:22  jyh
 * This is a simple term display in an applet.
 *
 */
