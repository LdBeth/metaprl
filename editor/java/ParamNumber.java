/*
 * this is an infinite precision number.
 */

import netscape.util.*;

public class ParamNumber
extends ParamString
{
    /**
     * Create the number.
     */
    public ParamNumber(String v)
    {
        super(v);
    }

    /**
     * Match with another number.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamNumber && ((ParamNumber) param2).value.equals(value))
            return null;
        throw new ParamMatchError("Numbers do not match", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_NUMBER;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_NUMBER;
    }

    /**
     * Default display.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString(value), i);
        return i + 1;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:29  jyh
 * This is a simple term display in an applet.
 *
 */
