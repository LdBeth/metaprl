/**
 * A meta-string.
 */

import netscape.util.*;

public class ParamMVar
extends ParamMeta
{
    /**
     * Make the number var.
     */
    public ParamMVar(String v)
    {
        super(v);
    }

    /**
     * Match against a level expression.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamVar)
            return param2;
        throw new ParamMatchError("Parameter is not a variable", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_MVAR;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_VAR;
    }

    /**
     * Display witha '$' to indicate meta.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString("$" + value + ":v"), i);
        return i + 1;
    }
}

