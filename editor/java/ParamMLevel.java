/**
 * A meta-string.
 */

import netscape.util.*;

public class ParamMLevel
extends ParamMeta
{
    /**
     * Make the number var.
     */
    public ParamMLevel(String v)
    {
        super(v);
    }

    /**
     * Match against a level expression.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamLevelExp)
            return param2;
        throw new ParamMatchError("Parameter is not a level expression", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_MLEVEL;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_LEVEL;
    }

    /**
     * Display witha '$' to indicate meta.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString("$" + value + ":l"), i);
        return i + 1;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:06  jyh
 * This is a simple term display in an applet.
 *
 */
