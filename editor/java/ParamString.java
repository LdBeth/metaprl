/*
 * This is a string parameter.
 */

import netscape.util.*;

public class ParamString
extends Param
{
    /**
     * This is the string.
     */
    String value;

    /**
     * Match with another number.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamString && ((ParamString) param2).value.equals(value))
            return null;
        throw new ParamMatchError("Numbers do not match", this, param2);
    }

    /**
     * Create the string.
     */
    public ParamString(String s)
    {
        value = s;
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_STRING;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_STRING;
    }

    /**
     * Default display.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString("\"" + value + "\""), i);
        return i + 1;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:30  jyh
 * This is a simple term display in an applet.
 *
 */
