/**
 * This is the base parameter.
 */

import netscape.util.*;

public abstract class Param
{
    /**
     * Parameter types.
     */
    public static final int PARAM_NUMBER        = 0;
    public static final int PARAM_STRING        = 1;
    public static final int PARAM_TOKEN         = 2;
    public static final int PARAM_LEVEL         = 3;
    public static final int PARAM_VAR           = 4;

    public static final int PARAM_MNUMBER       = 5;
    public static final int PARAM_MSTRING       = 6;
    public static final int PARAM_MTOKEN        = 7;
    public static final int PARAM_MLEVEL        = 8;
    public static final int PARAM_MVAR          = 9;

    public static final int PARAM_OBID          = 10;
    public static final int PARAM_PARAM_LIST    = 11;

    public static final int PARAM_MSUM          = 12;
    public static final int PARAM_MDIFF         = 13;
    public static final int PARAM_MPRODUCT      = 14;
    public static final int PARAM_MQUOTIENT     = 15;
    public static final int PARAM_MREM          = 16;
    public static final int PARAM_MLESS_THAN    = 17;
    public static final int PARAM_MEQUAL        = 18;
    public static final int PARAM_MNOT_EQUAL    = 19;

    /**
     * Function that returns the parameter type.
     */
    public abstract int getType();

    /**
     * Function that returns the "simple" type.
     * This is one of the basic types like number, string, etc.
     */
    public abstract int getSimpleType();

    /**
     * A simple param is a constant.
     */
    public boolean isSimple()
    {
        int i = getType();
        return i >= PARAM_NUMBER && i <= PARAM_VAR;
    }

    /**
     * A meta param is one of the meta types.
     */
    public boolean isMeta()
    {
        return getType() >= PARAM_MNUMBER;
    }

    /**
     * Match with another parameter.
     * Return the matched value if matching a meta
     * param with a simple, or null otherwise.
     */
    abstract Param match(Param param2)
        throws ParamMatchError;

    /**
     * Default display form.
     */
    abstract int display(Vector results, int i);
}

