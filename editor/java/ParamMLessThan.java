/**
 * A meta-sum.
 */

import netscape.util.*;

public class ParamMLessThan
extends ParamMPair
{
    /**
     * Make the number var.
     */
    public ParamMLessThan(Param param1, Param param2)
    {
        super(param1, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_MLESS_THAN;
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
        results.insertElementAt(new TermString(" < "), i);
        return param2.display(results, i + 1);
    }
}

