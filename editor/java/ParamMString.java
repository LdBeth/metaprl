/**
 * A meta-string.
 */

import netscape.util.*;

public class ParamMString
extends ParamMeta
{
    /**
     * Make the number var.
     */
    public ParamMString(String v)
    {
        super(v);
    }

    /**
     * Match against a string.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamString)
            return param2;
        throw new ParamMatchError("Parameter is not a string", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_MSTRING;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_STRING;
    }

    /**
     * Display witha '$' to indicate meta.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString("$" + value + ":s"), i);
        return i + 1;
    }
}

