/**
 * This is a variable parameter.
 */

import netscape.util.*;

public class ParamVar
extends ParamString
{
    /**
     * Build the variable.
     */
    public ParamVar(String v)
    {
        super(v);
    }

    /**
     * Match with another number.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamVar && ((ParamVar) param2).value.equals(value))
            return null;
        throw new ParamMatchError("Numbers do not match", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_VAR;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_VAR;
    }

    /**
     * Default display.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString(value + ":v"), i);
        return i + 1;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:34  jyh
 * This is a simple term display in an applet.
 *
 */
