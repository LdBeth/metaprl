/**
 * This is a token (like a string).
 */

import netscape.util.*;

public class ParamToken
extends ParamMeta
{
    /**
     * Make the token.
     */
    public ParamToken(String s)
    {
        super(s);
    }

    /**
     * Match with another number.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamToken && ((ParamToken) param2).value.equals(value))
            return null;
        throw new ParamMatchError("Numbers do not match", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_TOKEN;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_TOKEN;
    }

    /**
     * Default display.
     */
    public int display(Vector results, int i)
    {
        results.insertElementAt(new TermString("'" + value + "'"), i);
        return i + 1;
    }
}

